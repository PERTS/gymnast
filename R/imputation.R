modules::import("dplyr")
modules::import("reshape2", "melt")
modules::import("tidyr", "fill")

logging <- import_module("logging")
util <- import_module("util")

`%+%` <- paste0


# There's the question of how do we scope "active" cycles in which we'd like to
# impute data. And there's a SEPARATE question of how do we scope the index on
# the table that we're actually imputed. The first concerns how many distinct
# values are in the time ordinal column, and the second concerns the index of
# the table that we're imputing and filling

# The first thing is active_time_ordinals, and the second thing can be imputation_index

# So now we can define how we calculate active time ordinals and imputation_index
# for each of the different types of reports:

# - For classroom and team reports, active_time_ordinals are scoped to team, and
# the imputation_index is scoped to class-participant-question. We don't want
# answers to cross questions within people, or to cross classrooms within
# people, people within classrooms. None of that nonsense.

# - For org reports, active_time_ordinals are scoped to community, and the
# imputation_index is still scoped to class-participant-question

# - For networks, active_time_ordinals are scoped to network, and the
# imputation_index is STILL scoped to class-participant-question


# So really, active_time_ordinals is always scoped to the reporting_unit
# this function simply stitches the two functions above together. The only
# reason it's needed is because some of Dave's code elsewhere uses the melted
# imputed data object

impute_to_time_ordinal <- function(
  response_data,
  time_ordinal_column,
  imputation_index,
  cols_to_impute,
  time_ordinal_scope_vars
){
  # takes response-level dataset (response_data) and imputes the values in
  # cols_to_compute across values of the time_ordinal column, within whatever
  # variables are defined as imputation_index

  # response_data: unimputed data.frame with one row per response_id
  # time_ordinal_column: string defining the time column, e.g., week_start or
  # cycle_name (must be ordinal)
  # imputation_index is the set of variables within which imputation should be performed

  if(!"participant_id" %in% imputation_index){
    stop("for now, participant_id is required in the imputation_index")
  }

  if(!all(time_ordinal_scope_vars %in% imputation_index)){
    stop("impute_to_time_ordinal assumes data can be joined on time_ordinal_scope_vars, " %+%
           "so these are required to appear in the imputation_index.")
  }

  if(!all(imputation_index %in% names(response_data))){
    missing_cols <- imputation_index[!imputation_index %in% names(response_data)]
    stop("The following columns in the imputation index were not found in the response data: " %+%
           paste0(missing_cols))
  }

  if(!time_ordinal_column %in% names(response_data)){
    stop("The time ordinal column was not found in the response data: " %+% time_ordinal_column)
  }

  # handle NSE by hard-coding `time_ordinal_column` as the values in "time_ordinal_column"
  response_data$time_ordinal_column <- response_data[[time_ordinal_column]]
  melt_ids <- c(imputation_index, "time_ordinal_column")

  # integrity: if metrics are factors, turn them into characters
  cols_to_impute <- as.character(cols_to_impute)

  # integrity: Remove metrics that are not present & numeric.
  cols_filtered <- cols_to_impute[cols_to_impute %in% names(response_data)]
  if( length(cols_filtered) == 0 ){
    warning("None of the metrics to report exist in the response data")
    return(NULL)
  }

  numeric_cols_bool <- util$apply_columns(
    as.data.frame(response_data[,cols_filtered]),
    is.numeric) %>% unlist()
  numeric_metrics <- cols_filtered[numeric_cols_bool]

  # Each report_metric (question_code) must be imputed to the ordinal separately
  # otherwise there would be extra "dots" for weeks that were not collected
  # including after data collection stopped.

  # responses_melted is a tibble of present, reportable responses.
  # Those are the only responses that need to be imputed.
  responses_melted <- response_data %>%
    select(all_of(c(melt_ids, numeric_metrics))) %>%
    filter(!is.na(time_ordinal_column)) %>%
    reshape2::melt(id.vars=melt_ids) %>%
    filter(!is.na(value)) %>%
    rename(question_code = variable)

  # retain only one response if a participant
  # has multiple responses in a given level of time_ordinal_column
  responses_melted_last <- responses_melted %>%
    group_by(.dots = c(imputation_index, "time_ordinal_column", "question_code")) %>%
    # For medium-quality reasons, we don't care which value we select within the
    # time ordinal within participants, because the data should have been
    # filtered to complete responses already. In the future it might be great to
    # have a principled way of selecting these, though.
    summarise(value = last(value)) %>%
    ungroup()

  # In what time ordinals did each unit actively collect data?
  # If a student is missing data from a a week in which
  # their unit was collecting data, data will need to be imputed for that
  # time ordinal combination for that student/code.
  active_time_ordinals <- responses_melted_last %>%
    # take all values of the combined index, EXCLUDING participants (because we
    # want to impute over participants)
    select(all_of(time_ordinal_scope_vars), time_ordinal_column) %>%
    distinct()

  active_questions <- responses_melted_last %>%
    select(all_of(time_ordinal_scope_vars), question_code) %>%
    distinct()

  # @to-do: should this table include question_code?
  user_on_imputation_index <- response_data %>%
    select(all_of(imputation_index)) %>%
    distinct()

  # for imputation, you need every combination of active time_ordinal for a user x time_ordinal
  # and the expanded grid of questions
  imputation_base <- user_on_imputation_index %>%
    inner_join(active_time_ordinals,
               by = time_ordinal_scope_vars) %>%
    inner_join(active_questions,
               by = time_ordinal_scope_vars)

  # rmv = responses, melted, variables
  rmv <- left_join(
    imputation_base,
    responses_melted_last,
    by=c(imputation_index, "time_ordinal_column", "question_code"))

  # rmvi = responses, melted, variables
  imputed_melted <- rmv %>%
    group_by(.dots = c(imputation_index, "question_code")) %>%
    arrange(time_ordinal_column) %>%
    mutate(value_imputed = value) %>%
    # we choose downup because when arrange is in ascending order, downward means
    # to the future. So we impute down first to give preference to what we've observed.
    # E.g., if someone answers "yes" on their second observation and "no" on their
    # fourth, we assume that the THIRD observation was "yes" rather than "no."
    # @to-do: test this
    fill(value_imputed, .direction="downup") %>%
    ungroup() %>%
    mutate(imputed_row = is.na(value)) %>%
    arrange(participant_id, time_ordinal_column) %>%
    mutate_if(is.factor, as.character) %>%
    stats::setNames(util$recode(names(.), "time_ordinal_column", time_ordinal_column))

  imputed_melted
}

cast_imputed <- function(imputed_melted,
                         imputation_index,
                         time_ordinal_column,
                         cols_to_impute){

  # This casts the imputed data back to response-level and assigns a boolean
  # as to whether anything in the row has been imputed (some questions can be
  # imputed while others are not within the same response, but we don't want
  # to wring our hands over such subtleties here)
  imputed_melted$time_ordinal_column <- imputed_melted[[time_ordinal_column]]

  imputed_rows <- imputed_melted %>%
    reshape2::dcast(stats::as.formula(
      paste0(imputation_index, collapse = " + ") %+%
        "+ time_ordinal_column ~ question_code"
      ),
      value.var = "imputed_row"
    ) %>%
    rowwise() %>%
    mutate(
      imputed_row = any(c_across(
        all_of(cols_to_impute)
      ))
    ) %>%
    select(-all_of(cols_to_impute))


  imputed_values <- imputed_melted %>%
    reshape2::dcast(
      stats::as.formula(
        paste0(imputation_index, collapse = " + ") %+%
          "+ time_ordinal_column ~ question_code"
      ),
      value.var = "value_imputed"
    ) %>%
    left_join(
      .,
      imputed_rows,
      by = c(imputation_index, "time_ordinal_column")
    ) %>%
    stats::setNames(util$recode(names(.), "time_ordinal_column", time_ordinal_column))

  imputed_values
}

impute_responses_downup <- function(response_data,
                             time_ordinal_column,
                             imputation_index,
                             cols_to_impute,
                             time_ordinal_scope_vars){


  # response_data: unimputed data.frame with one row per response_id
  # time_ordinal_column: string defining the time column, e.g., week_start or
  # cycle_name (must be ordinal)
  # imputation_index is the additional set of variables beyond
  # (which is hard-coded & mandatory) within which imputation should be
  # performed, usually team_id or child_id
  imputed_melted <- impute_to_time_ordinal(
    response_data,
    time_ordinal_column,
    imputation_index,
    cols_to_impute,
    time_ordinal_scope_vars
  )
  imputed_casted <- cast_imputed(
    imputed_melted,
    imputation_index,
    time_ordinal_column,
    cols_to_impute
  )

  imputed_casted
}



