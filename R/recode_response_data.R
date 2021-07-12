modules::import("dplyr", `%>%`, "filter", "one_of", "rename", "select")
modules::import("lubridate", "floor_date")
modules::import("stats", "setNames")

logging <- import_module("logging")
scale_computation <- import_module("scale_computation")
util <- import_module("util")


`%+%` <- paste0

row_has_true <- function(df){
  # takes response level data and a set of columns, and returns a vector of length
  # nrow(df) where the value is TRUE if one or more values is TRUE
  apply(df, 1, function(x) any(x, na.rm = T))
}

row_is_all_blank <- function(df){
  # takes response level data and a set of columns, and returns a vector of length
  # nrow(df) where the value is TRUE if one or more values is blank and FALSE
  # otherwise
  apply(df, 1, function(x) all(util$is_blank(x)))
}

define_gender_disadvantage <- function(gender) {
  # CONFIG FUNCTION. Changing this requires Critical QA,
  # see https://docs.google.com/document/d/1xrA23oIWJCvXGyCGy_M0n0GLi0dj93NwmdqbsQfW1rs/edit#heading=h.r5j0dej35oxb
  ifelse(
    gender %in% "female",
    TRUE,
    # Note that this assigns all gender values other than "male" to be
    # removed. Right now these only include "other", but
    # any values that are later introduced will also be removed.
    ifelse(gender %in% "male", FALSE, NA)
  )
}

define_race_disadvantage <- function(response_data, race_config){
  # CONFIG FUNCTION. Changing this requires Critical QA,
  # see https://docs.google.com/document/d/1xrA23oIWJCvXGyCGy_M0n0GLi0dj93NwmdqbsQfW1rs/edit#heading=h.r5j0dej35oxb

  adv_cols <- race_config$question_code[!race_config$race_disadv]
  disadv_cols <- race_config$question_code[race_config$race_disadv]

  if (!all(unlist(util$apply_columns(response_data[c(adv_cols, disadv_cols)], is.logical)))) {
    stop("Got non-logical column for race");
  }

  # Participants are NA if they did not answer the race item; and
  # disadvantaged if they chose ANY disadvantaged categories
  race_disadv <- apply(response_data[disadv_cols], 1, function(x) any(x, na.rm = T))
  all_blank <- apply(
    response_data[c(adv_cols, disadv_cols)],
    1,
    function(x) all(util$is_blank(x) | !x)
  )
  race_disadv[all_blank] <- NA
  return(race_disadv)
}

define_high_fin_stress <- function(
  response_data,
  items
){

  # CONFIG FUNCTION. Changing this requires Critical QA,
  # see https://docs.google.com/document/d/1xrA23oIWJCvXGyCGy_M0n0GLi0dj93NwmdqbsQfW1rs/edit#heading=h.r5j0dej35oxb

  # define values that denote food insecurity and define allowed values
  FOOD_INSECURE_VALS <- c('often', 'sometimes', '1', '2')
  FOOD_INSECURITY_ALLOWED_VALS = c('often', 'sometimes', 'never', '1', '2', '3')

  # check the values in the columns. food_insecure should have only expected
  # values
  present_food_ins_vals <- response_data$food_insecure[
    !is.na(response_data$food_insecure)
    ] %>%
    unique()
  unexpected_food_ins_vals <- setdiff(present_food_ins_vals, FOOD_INSECURITY_ALLOWED_VALS)
  if(length(unexpected_food_ins_vals) > 0){
    stop(
      "In `define_high_fin_stress`, the following unexpected values were found " %+%
      "in the `food_insecure` column: " %+%
      paste0(unexpected_food_ins_vals)
    )
  }

  # check the presence of required columns
  fin_stress_vars <- items$question_code[items$financial_stress]
  missing_fin_stress <- fin_stress_vars[!fin_stress_vars %in% names(response_data)]
  if(length(missing_fin_stress) > 0){
    stop("in define_high_fin_stess, required fin stress vars were missing: " %+%
          paste0(missing_fin_stress, collapse = ", "))
  }

  # fin_insecure cols should be boolean
  fin_ins_cols <- fin_stress_vars[!fin_stress_vars  %in% "food_insecure"]
  fin_ins_is_logical <- util$apply_columns(response_data[fin_ins_cols], is.logical) %>%
    unlist()
  if(any(!fin_ins_is_logical)){
    stop(
      "Logical values are required for all financial stress variables. The following " %+%
      "were found to contain non-logicals: " %+%
      paste0(fin_ins_cols[!fin_ins_is_logical], collapse = ", ")
    )
  }

  # fin_stress includes any form of food insecurity (defined by food_ins_vals)
  is_food_insecure <- response_data$food_insecure %in% FOOD_INSECURE_VALS
  financial_cols <- fin_stress_vars[!fin_stress_vars %in% "food_insecure"]
  is_fin_str <- row_has_true(response_data[financial_cols])
  financial_stress_disadv <- is_fin_str | is_food_insecure
  financial_stress_disadv[
    row_is_all_blank(response_data[c(financial_cols, "food_insecure")])
  ] <- NA
  return(financial_stress_disadv)
}



recode_response_data <- function(
  response_data,
  items,
  subset_config
){
  response_data_recoded <- response_data %>%
    util$apply_columns(util$as_numeric_if_number)

  logging$debug("########## CHUNK DATES ##########")
  ##  Survey responses are grouped by day for tracking purposes

  # compute week_start and day_start values
  survey_date_format <- "%Y-%m-%d %H:%M:%S"
  stripped_time <- strptime(response_data_recoded$StartDate , survey_date_format)
  response_data_recoded$week_start <- floor_date(
    stripped_time, unit = "week"
    ) %>%
    as.Date()
  response_data_recoded$day_start <- floor_date(
    stripped_time, unit = "day"
    ) %>%
    as.Date()

  # all observations must have a class name parameter. Otherwise set it to "Not reporting"
  response_data_recoded$class_name[
    util$is_blank(response_data_recoded$class_name)
  ] <- "Not reporting"

  ########################################################################
  ################### code binary advantaged/disadv subsets ##############
  logging$debug("########## code binary advantaged/disadv subsets ##########")

  # gender
  response_data_recoded$gender_disadv <- define_gender_disadvantage(
    response_data_recoded$gender
  )
  # race
  race_config <- items %>%
    dplyr::filter(!is.na(race_disadv)) %>%
    select(question_code, race_disadv)
  response_data_recoded$race_disadv <- define_race_disadvantage(
    response_data,
    race_config = race_config
  )

  # code financial stress and target groups if the relevant items appear in the
  # subsets config
  subset_types <- unique(subset_config$subset_type)

  if(any(subset_types %in% "financial_stress")){
    response_data_recoded$financial_stress_disadv <- define_high_fin_stress(
      response_data_recoded,
      items = items
    )
  }

  if(any(subset_types %in% "target_group")){
    response_data_recoded$target_group_disadv <- as.logical(
      response_data_recoded$in_target_group
    )
  }
  
  # @to-do here retrieve login_hash values

  # now recode to categoricals based on the subset config
  for(type in subset_types){
    subset_rows <- subset_config %>% filter(subset_type %in% type)
    response_data_recoded[[type %+% "_cat"]] <- util$recode(
      response_data_recoded[[type %+% "_disadv"]],
      originals = subset_rows$disadv,
      replacements = subset_rows$subset_value
    )
    # @sg to-do there needs to be a step here to resolve discrepancies across individual humans' data entry
    # first_pass <- do what's done above
    # second_pass <- resolve_conflicting_demographics(first_pass, hashed_logins)
    # response_data_recoded[[type %+% "_cat]] <- second_pass
  }

  ##### compute composites (where appropriate)
  # use the items df to define the scale_variables input to sc.append_scales
  scale_vars <- items %>%
    filter(composite_input) %>%
    select(question_code, driver) %>%
    rename(
      var_name = question_code,
      scale = driver
    )

  # recode numeric fields as numbers. This does nothing when no scales are requested
  response_data_recoded <- scale_computation$append_scales(
    response_data_recoded,
    scale_vars,
    add_z_score = FALSE
  ) %>%
    setNames(gsub("\\-", "_", names(.)))

  return(response_data_recoded)

}

check_present_recoded_subsets <- function(response_data_recoded, subset_config){
  # check for present subset columns here because prior to this point in the script,
  # we wouldn't expect them to exist.
  expected_subset_cols <- subset_config$subset_type %>% unique() %+% "_cat"
  PRESENT_SUBSET_COLS <- expected_subset_cols[
    expected_subset_cols %in% names(response_data_recoded)
  ]
  if(length(PRESENT_SUBSET_COLS) == 0){
    return("Error: no subset columns were found in imputed data. Rendering no reports.")
  }
}

check_recoded_numerics <- function(response_data_recoded, items){

  numeric_metrics <- items$question_code[
    items$metric_for_reports | items$composite_input
    ] %>%
    unique()

  expected_are_numeric <- response_data_recoded %>%
    select(one_of(numeric_metrics)) %>%
    lapply(is.numeric) %>%
    unlist()

  expected_numerics_are_all_blank <- response_data_recoded %>%
    select(one_of(numeric_metrics)) %>%
    lapply(., function(x) all(util$is_blank(x))) %>%
    unlist()

  # throw an error if expected are neither numeric nor all-blank
  if(!all(expected_are_numeric | expected_numerics_are_all_blank)){
    return("Non-numeric data types were found where numerics were expected." %+%
           " No reports were created.")
  }
}

check_recoded_composites <- function(response_data_recoded, items){
  # make sure all the right scales were computed successfully, if any were computed
  # if none were computed, skip the checks
  scale_vars <- items %>%
    filter(composite_input) %>%
    select(question_code, driver) %>%
    rename(
      var_name = question_code,
      scale = driver
    )

  expected_scale_vars <- scale_vars$scale %>% gsub("\\-", "_", .) %>% unique()

  if(length(expected_scale_vars) > 0){

    if(!all(expected_scale_vars %in% names(response_data_recoded))){
      return("Some expected scales were not computed. No reports generated.")
    }
    composites_are_numeric <- response_data_recoded[expected_scale_vars] %>%
      lapply(is.numeric) %>%
      unlist()
    if(any(!composites_are_numeric)){
      return("computing composites resulted in unexpected data types. No reports generated.")
    }

    all_blank_composites <- response_data_recoded[expected_scale_vars] %>%
      lapply(., function(x) all(util$is_blank(x))) %>%
      unlist()
    if(any(all_blank_composites)){
      logging$info("In computing composites, some columns were found with all-blank values. This is " %+%
                     "technically possible but highly unlikely. It could indicate a failed calculation.")
    }

  }
}

check_recoded_subsets <- function(
  response_data_recoded,
  subset_config,
  emailer = emailer
){
  ###### Make sure subsets got recoded correctly
  subset_cols <- subset_config$subset_type %>% unique() %+% "_cat"
  subset_vals_all <- response_data_recoded[subset_cols] %>%
    unlist() %>%
    unique() %>%
    util$na_omit()
  subset_vals_expected <- subset_config$subset_value

  if(any(!subset_vals_all %in% subset_vals_expected)){
    return("recoding of subsets produced unexpected subset values. No reports " %+%
           "generated.")
  }
  if(any(!subset_vals_expected %in% subset_vals_all)){
    missing_subsets <- subset_vals_expected[!subset_vals_expected %in% subset_vals_all]
    affected_teams <- response_data_recoded$team_id %>% unique()
    n_affected_participants <- length(unique(response_data_recoded$participant_id))
    emailer(
      to = c("chris@perts.net", "sarah@perts.net"),
      subject = ("unexpectedly missing subsets"),
      body_text = "Some expected subsets were missing from the response data " %+%
        "generated for the following teams: " %+%
        paste0(affected_teams, collapse = ", ") %+%
        ". This means NONE of the following subset values were found in the data: " %+%
        paste0(missing_subsets, collapse = ", ") %+%
        ". This can happen by chance if students happen to be " %+%
        "particularly homogeneous within a given batch of reports. " %+%
        "However, it could also indicate problems with the recoding " %+%
        "of subsets. Investigate if the number of teams/participants " %+%
        "sounds too large for subsets to be missing by chance. " %+%
        "(N participants = " %+% n_affected_participants %+% " from " %+%
        length(affected_teams) %+% " team[s])"
    )
  }
}


get_logins_for_participants <- function(participant_ids, participant_tbl){
  # @to-do this function takes a vector of participant_ids and a
  # participant_tbl, and links through the participant_tbl to a login_id
  # corresponding to each participant_id. Hashed login_id values are returned
  # because Sarah is too paranoid to use real ids for any calculation ever
  
  # takes a vector of length(participant_ids) and uses the participants_tbl to
  # look up their login_id value. Returns a vector of distinct, hashed
  # login_ids.
}


resolve_conflicting_demographics <- function(login_hashes, demog_vector){
  # @to-do this function takes as its input a vector of login hashes and a
  # vector of demographic data, and returns an updated vector of demographic
  # data with conflicting entries marked as "NA"
}