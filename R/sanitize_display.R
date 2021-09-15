# Functions to obscure, anonymize, and otherwise sanitize data to prepare it
# for display in graphs.

logging <- import_module("logging")
util <- import_module("util")

modules::import('dplyr')

`%+%` <- paste0

expand_subsets_agm_df <- function(
  agm_df_ungrouped,
  desired_subset_config,
  time_ordinal_column,
  combined_index = c("reporting_unit_id", "metric", "subset_value"),
  unpropagated_fields = c("pct_good", "se", "n", "disadv"),
  cols_varying_by_subset_type = c("p", "grand_mean")
){
  # this function expands the agm objects to include ALL subset_values specified
  # by the desired_subset_config.
  # susbset_values that are present in desired_subset_config but not agm_df_ungrouped
  # should be added
  # Subset_values added by expansion will have NA
  # values in the pct_good and se fields, "0" values in the n field and contain
  # cols_varying_by_subset_type is a string vector identifying any columns that
  # MUST be propagated within subset_type. An example is grand_mean, which is
  # "Masked" for some subset_types but not others. Don't want to propagate
  # "Masked" from gender to race subset_values!

  # first make sure the combined_index is valid
  comb_index_time <- c(combined_index, time_ordinal_column)
  missing_index <- comb_index_time[!comb_index_time %in% names(agm_df_ungrouped)]
  if(length(missing_index) > 0){
    stop(
      "In expand_subsets_agm_df, some indexes were missing: " %+%
      paste0(missing_index, collapse = ", ")
    )
  }

  if(!any(comb_index_time %in% "subset_value")){
    stop("In expand_subsets_agm_df, subset_value must appear in the combined index.")
  }

  agm_subset_types <- agm_df_ungrouped$subset_type %>%
    unique() %>%
    Filter(function (st) !st %in% "All Students", .)
  desired_subset_types <- desired_subset_config$subset_type %>% unique()
  unrecognized_subset_types <- agm_subset_types[
    !agm_subset_types %in% desired_subset_types
  ]

  if(length(unrecognized_subset_types) > 0){
    stop(
      "In expand_subsets_agm_df, it is required that all subset_types " %+%
      "in the agm_df_ungrouped input match a subset_type in the " %+%
      "desired_subset_config. This allows merging on that key. " %+%
      "Please fix the config file or the agm values before proceeding. "
    )
  }

  # identify which subsets need to be propagated. Which ones are missing from
  # some combination of the index variables?

  expand_vars <- comb_index_time[!comb_index_time %in% "subset_value"]
  complete_subsets <- tidyr::expand_grid(
    agm_df_ungrouped[expand_vars],
    subset_value = c(desired_subset_config$subset_value, "All Students")
    ) %>%
    unique()

  observed_subsets <- agm_df_ungrouped[comb_index_time] %>%
    dplyr::mutate(present = TRUE)

  merged_subsets <- dplyr::left_join(
    complete_subsets,
    observed_subsets,
    by = comb_index_time,
    suffix = c("_complete", "_observed")
  )
  if(!nrow(merged_subsets) == nrow(complete_subsets)){
    stop("in expand_subsets_agm_df, procedure to identify missing subsets failed.")
  }

  indexes_to_propagate <- merged_subsets %>%
    dplyr::filter(is.na(present))

  # if there are no new subsets, just return agm_df_ungrouped
  if(nrow(indexes_to_propagate) == 0){
    warning(
      "Your desired_subset_config identified no subset values in need " %+%
      "of propagating. No expansion was performed."
    )
    return(agm_df_ungrouped)
  }

  # make a data.frame of the new subset values, but with all other information
  # propagated from agm_df_ungrouped. In this move, we also set the "n" field to be zero,
  # since we know that if we're adding these values, there weren't any students
  # in the data from those subsets.

  # we start with the indexes to propagate,
  new_subsets_grid <-  dplyr::left_join(
    indexes_to_propagate,
    desired_subset_config[c("subset_type", "subset_value")],
    by = "subset_value"
  ) %>%
    dplyr::select(-present) %>%
    ungroup() %>%
    # Back-fill the subset_type field to match what's in the subset_value field
    # when subset_value is "All Students." This is pretty hacky, but necessary
    # because "All Students" does not appear in the subset_config, and thus
    # there's nowhere else to take this value from. We also know the subset_type
    # for "All Student" will always be "All Students." When we change this
    # string for Catalyze, the principle will be the same — whatever string we
    # replace "All Students" with (e.g., "All Respondents") will still not live
    # in the subset_config and thus will still need to be back-filled here. So
    # at that point we'll replace the hard-coded "All Students" string with a
    # variable that can take any value, but the back-filling will stay the same.
    mutate(subset_type = ifelse(subset_value %in% "All Students",
                                subset_value,
                                subset_type))


  # add the propagated values to the new_subsets_df
  propagated_values_df_base <- agm_df_ungrouped %>%
    dplyr::select(-dplyr::one_of(unpropagated_fields)) %>%
    unique()

  # some columns vary by subset_type and some don't. For totally missing
  # subset_types, we'll only propagate the columns that don't vary by
  # subset_type

  # add any missing subset_types to the propagated_values_df. If subset_types
  # are totally missing from the data. The subset_type "All Students" is always
  # missing.
  missing_subset_types <- c(setdiff(desired_subset_config$subset_type,
                                  propagated_values_df_base$subset_type),
                            "All Students")
  propagated_values_across_subset_type <- propagated_values_df_base %>%
    dplyr::select(-any_of(c(cols_varying_by_subset_type, "subset_type"))) %>%
    tidyr::expand_grid(., subset_type = missing_subset_types)

  propagated_values_df <- bind_rows(propagated_values_df_base,
                                    propagated_values_across_subset_type)


  new_subsets_propagated <- new_subsets_grid %>%
    dplyr::left_join(
      .,
      propagated_values_df,
      by = c(expand_vars, "subset_type"),
      suffix = c("", "_propagated")
    ) %>%
    dplyr::mutate(n = 0)

  agm_expanded <- util$rbind_union(list(
    agm_df_ungrouped, new_subsets_propagated
  ))
  return(agm_expanded)

}

pixellate_agm_df <- function(agm_df_ungrouped){
  # takes as input an agg_metrics type data.frame, and returns a pixellated
  # version of the data.frame. This function does NOT rely on grouping variables
  # and thus the agm_df_ungrouped input is required to be ungrouped.

  if(dplyr::is.grouped_df(agm_df_ungrouped)){
    agm_df_ungrouped <- dplyr::ungroup(agm_df_ungrouped)
    logging$warning("pixellate_agm_df removed grouping from input data.frame.")
  }

  pixellated_agm_df <- agm_df_ungrouped %>%
    # find the minimum non-zero standard error for each reporting unit
    # and smallest possible non-zero pct good
    dplyr::mutate(min_nonzero_se = min(se[!se %in% 0], na.rm = T)) %>%
    # ungroup to summarise se as a long vector instead of by reporting unit
    dplyr::ungroup() %>%
    # pixellate standard errors
    dplyr::mutate(
      se = ifelse(
        # wherever se is zero OR has a blank value where data existed
        # (non-blank pct_good and blank se) replace with min_se
        se %in% 0 | (is.na(se) & !is.na(pct_good)),
        min_nonzero_se,
        se
      )
    ) %>%
    # pixellate pct_good values so that reports always say at least 1 student
    # gave the good response. We'll have to replace zero pct_good values with
    # 1/n for a given cell. Then, compute text versions of these edited values
    dplyr::mutate(
      # derive new pct_good values adjusted for pixellated display.
      pct_good = ifelse(pct_good %in% 0, 1/n, pct_good)
    )
  return(pixellated_agm_df)
}

anonymize_agm_df <- function(
  agm_df_ungrouped,
  time_ordinal_column,
  mask_within = c("reporting_unit_id", "subset_type", "metric"),
  cols_to_hide = c("pct_good", "se", "p"),
  min_cell = 5
){
  # takes an agg_metrics type object and returns its anonymized version

  if(dplyr::is.grouped_df(agm_df_ungrouped)){
    agm_df_ungrouped <- dplyr::ungroup(agm_df_ungrouped)
    logging$warning("pixellate_agm_df removed grouping from input data.frame.")
  }
  mask_within <- c(mask_within, time_ordinal_column)
  agm_anon <- agm_df_ungrouped %>%
    dplyr::mutate(mask_row = n < min_cell) %>%
    dplyr::group_by_(.dots = mask_within) %>%
    dplyr::mutate(mask_type = any(mask_row)) %>%
    dplyr::ungroup() %>%
    # mark grand_mean as "Masked" to keep track of what has been masked
    dplyr::mutate(grand_mean = ifelse(mask_type, "Masked", grand_mean))

  # now replace all relevant data in cols_to_hide with NA
  for(col in cols_to_hide){
    agm_anon[[col]][agm_anon$grand_mean %in% "Masked"] <- NA
  }

  return(agm_anon)
}

display_anon_agm <- function(
  agm_anon,
  combined_index = c("reporting_unit_id", "cycle_name", "metric", "subset_value")
){
  # takes a masked agm object and formats it for display in a
  # bar or line graph: replaces masked NA pct_good values with the grand mean
  # (all students), and configures text and factor levels for display

  # the agm object is assumed to be queued up for displaying in bar/line charts
  # and thus has been filtered to one ru and possibly its parent (e.g., one
  # roster and possibly the corresponding team)

  # first check that agm_anon is in fact unique per the combined index:
  ur <- agm_anon %>%
    dplyr::select(dplyr::one_of(combined_index)) %>%
    unique() %>%
    nrow()
  tr <- agm_anon %>% nrow()
  if(ur != tr){
    stop(
      "In display_anon_agm, the combined index does not uniquely identify " %+%
      "rows."
    )
  }

  # we merge on everything but subset_value
  merge_vars <- combined_index[!combined_index %in% "subset_value"]

  grand_mean_pct_good <- agm_anon %>%
    dplyr::filter(subset_value %in% "All Students") %>%
    dplyr::select(dplyr::one_of(c(merge_vars, "pct_good"))) %>%
    dplyr::rename(pct_good_all = pct_good)

  agm_display_anon <- agm_anon %>%
    dplyr::left_join(
      .,
      grand_mean_pct_good,
      by = merge_vars
    ) %>%
    dplyr::mutate(
      pct_good = ifelse(mask_type, pct_good_all, pct_good),
      pct_good_text = ifelse(mask_type, "n/a", round(pct_good *100) %+% "%")
    )
  return(agm_display_anon)
}

simulate_agm <- function(
  subset_config,
  n_teams = 1,
  n_classes = 1,
  metrics = c("mw2_1", "mw2_2", "mw2_3"),
  cycles = c("Cycle 01 (Date)", "Cycle 02 (Date)"),
  small_n_types = c(),
  min_cell = 5,
  cell_n = 25 # defines the cell size for non-all-students subset_value
){
  # creates an object of the format of agg_metrics


  # check the inputs to the simulation
  if(any(duplicated(subset_config$subset_value))){
    stop("simulated agg metrics data cannot be created with duplicate subset_values")
  }

  present_subset_types <- unique(subset_config$subset_type)
  if(length(small_n_types) > 0){
    if(any(!small_n_types %in% present_subset_types)){
      stop("you cannot mask subset types that are not present in the " %+%
             "subset_config")
    }
  }

  # create dummy values
  team_ids <- paste0("Team_", 1:n_teams)
  class_ids <- paste0("Classroom_", 1:n_classes)

  # add all students to subset config
  subset_config_as <- util$rbind_union(list(
    subset_config,
    dplyr::tibble(subset_value = "All Students", subset_type = "All Students")
    )) %>%
    dplyr::mutate(
      subset_type = as.character(subset_type),
      subset_value = as.character(subset_value)
    )

  # ns for each subset type (these will get divided in half for subset values)

  ns <- subset_config_as %>%
    dplyr::select(subset_value, subset_type) %>%
    dplyr::mutate(n = cell_n)
  total_n <- sum(ns$n[!ns$subset_value %in% "All Students"])
  ns$n[ns$subset_value %in% "All Students"] <- total_n

  ns$n[ns$subset_type %in% small_n_types] <- min_cell - 1


  # build the data.frame
  agm <- tidyr::expand_grid(
    reporting_unit_id = c(team_ids, class_ids),
    metric = metrics,
    cycle_name = cycles,
    subset_value = c(as.character(subset_config$subset_value), "All Students")
  ) %>%
    unique() %>%
    dplyr::left_join(
      .,
      subset_config_as,
      by = "subset_value"
    ) %>%
    dplyr::left_join(
      .,
      ns,
      by = c("subset_type", "subset_value")
    ) %>%
    dplyr::mutate(
      reporting_unit_name = gsub("_", " ", reporting_unit_id),
      grand_mean = ifelse(subset_value %in% "All Students", "All Students", "Subset"),
      pct_good = sample(seq(0, 1, .01), nrow(.), replace = T),
      se = sample(seq(0, .3, .001), nrow(.), replace = T)
    ) %>%
    dplyr::group_by(subset_type) %>%
    dplyr::mutate(p = ifelse(
      subset_type %in% "All Students",
      NA, sample(seq(.01, .99, .01), 1)
      )
    ) %>%
    dplyr::ungroup()

  # fill in accurate grand mean cell values
  return(agm)
}

check_bar_graph_input <- function(
  bar_df,
  subset_config,
  required_columns = c("grand_mean", "pct_good", "subset_value",
                       "question_text_wrapped","reporting_unit_name_wrapped",
                       "pct_good_text", "reporting_unit_id")
){
  # bar_df is the input to the ggplot call that produces the bar graphs for
  # project/roster reports. This test is to be implemented immediately prior to
  # the ggplot call that renders the bar graphs in the project reports.
  #
  # Returns NULL if there are no problems found, or a unitary character
  # error message.

  # All required columns are present.
  absent_required_columns <- required_columns[!required_columns %in% names(bar_df)]
  if(length(absent_required_columns)){
    return(
      "The following missing are needed to render ggplot graph: " %+%
      paste0(absent_required_columns, collapse = ", ")
    )
  }

  # Uniqueness of required columns (each row in the df is required; there should
  # be no aggregation in the ggplot call)
  unique_rows <- bar_df %>%
    dplyr::select(dplyr::one_of(required_columns)) %>%
    unique() %>%
    nrow()
  total_rows <- bar_df %>% nrow()
  if(unique_rows != total_rows){
    return(
      "Your ggplot is not set to plot unique values. This means " %+%
      "nasty aggregation business will ensue and the bars will be wrong."
    )
  }

  # ALL subset values that are present in the subset config
  # appear for all metric/reporting unit combos in the data.frame.
  expected_subsets <- subset_config$subset_value
  missing_subsets_df <- bar_df %>%
    dplyr::group_by(reporting_unit_id) %>%
    dplyr::summarise(
      missing_subsets = expected_subsets[!expected_subsets %in% subset_value] %>%
        paste0(collapse = ", ")
    ) %>%
    dplyr::filter(!util$is_blank(missing_subsets))
  if (nrow(missing_subsets_df) > 0) {
    return(
      "Subset values are missing from the bar graph input df: " %+%
      paste0(missing_subsets_df$missing_subsets, collapse = ", ") %+%
      " from reporting units " %+%
      paste0(missing_subsets_df$reporting_unit_id, collapse = ", ")
    )
  }

  # validity of pct_good values: no non-numeric, blank, NA, < 0, or > 1 allowed
  if(any(is.na(bar_df$pct_good))){
    return(
      "NA values were found in the pct_good field. These will cause " %+%
      "bars to be missing unexpectedly."
    )
  }
  if(any(!is.numeric(bar_df$pct_good))){
    return(
      "Non-numeric values were found in the pct_good field. These will cause " %+%
      "graphs not to render."
    )
  }
  if(!all(bar_df$pct_good <= 1 & bar_df$pct_good >= 0)){
    return(
      "Values outside the bounds of 0 and 1 were found in the pct_good " %+%
      "field. These indicate something deeply wrong with the data."
    )
  }

  return(NULL)
}


build_bar_display <- function(agm, desired_subset_config, time_ordinal_column = "cycle_name"){
  bar_df <- agm %>%
    pixellate_agm_df() %>%
    expand_subsets_agm_df(
      desired_subset_config = desired_subset_config
    ) %>%
    anonymize_agm_df(time_ordinal_column = time_ordinal_column) %>%
    display_anon_agm()
  return(bar_df)
}