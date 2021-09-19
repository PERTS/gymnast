context("sanitize_display.R")
# ^^^ First line of file, or else!
# https://github.com/r-lib/testthat/issues/700#issuecomment-367675035

# To run these tests:
#
# * Open a terminal window
# * Change to the rserve directory
# * Run this command:
#   Rscript -e "testthat::auto_test('R', 'tests/testthat')"
#
# The test runner will watch your code files and re-run your tests when you
# change something. Get all those yummy green checks!

if (grepl('tests/testthat$', getwd())) {
  setwd('../..')  # root dir of gymnast repo
}

library(testthat)

modules::import("dplyr", `%>%`)
sanitize_display <- import_module("sanitize_display")

# this is a mock-up of a subset_config object. In the pipeline,
# the object is called `subsets`
subset_config_default <- data.frame(
  subset_value = c("Race Struct. Adv.", "Race Struct. Disadv.",
                   "Male", "Female", "Target Grp.", "Not Target Grp."),
  subset_type = c("race_cat", "race_cat", "gender_cat", "gender_cat",
                  "target_group_cat", "target_group_cat"),
  disadv = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)
agm_indexes <- c("reporting_unit_id", "metric", "subset_value")

create_missing_unimputed <- function(subset_config){
  agm <- sanitize_display$simulate_agm(subset_config_default) %>%
    dplyr::filter(!(
      subset_value %in% "Male" &
      cycle_name %in% "Cycle 02 (Date)"
  ))
  return(agm)
}


describe('simulate_agm', {

  it('simulates expected default subsets', {

    agm <- sanitize_display$simulate_agm(subset_config_default)
    expect_equal(
      unique(subset_config_default$subset_value),
      unique(agm$subset_value[!agm$subset_value %in% "All Students"])
    )
  })

  it('creates the expected combined index', {
    agm <- sanitize_display$simulate_agm(subset_config_default)
    expect_equal(nrow(unique(agm)), nrow(unique(agm[c(agm_indexes, "cycle_name")])))
  })

  it('simulates small n types', {
    agm <- sanitize_display$simulate_agm(
      subset_config_default,
      small_n_types = "target_group_cat"
    )
    agm_tg <- agm %>%
      dplyr::filter(subset_type %in% "target_group_cat")
    expect_true(all(agm_tg$n < 5))
  })

  it('simulates small missing subset_values', {
    subset_config_no_male <- subset_config_default %>%
      dplyr::filter(!subset_value %in% "Male")
    agm <- sanitize_display$simulate_agm(subset_config_no_male)
    expect_false(any(agm$subset_value %in% "Male"))
  })

  it('can simulate unimputed data with fiddling', {
    # with unimputed data, we cannot count on the property that n values
    # from cycles 2+ will be >= n values from the previous cycle.
    # Therefore, it is possible for subset values to be missing JUST
    # from particular cycles, rather than for the whole agm object.
    agm <- create_missing_unimputed(subset_config_default)
    # make sure we've successfully removed "Male" from the cycle 2 data only
    agm_c2 <- agm %>%
      dplyr::filter(cycle_name %in% "Cycle 02 (Date)")
    expect_true(! "Male" %in% agm_c2$subset_value)
    expect_true("Male" %in% agm$subset_value)

  })

  it('simulates p-values, i.e., contains a p-value column called `p` with one distinct value per subset type', {
    agm <- sanitize_display$simulate_agm(subset_config = subset_config_default)
    # check that the column `p`` exists
    expect_true("p" %in% names(agm))

    p_summary <- agm %>%
      dplyr::group_by(subset_type) %>%
      dplyr::summarise(
        distinct_values = dplyr::n_distinct(p),
        NA_values = sum(is.na(p)),
        n = dplyr::n(),
        all_NA = NA_values == n
      )
    # check that there is one distinct p-value per subset_type:
    expect_equal(p_summary$distinct_values, rep(1, nrow(p_summary)))
    # check that p-value is NA for "All Students"
    all_students_p_is_na <- p_summary$all_NA[p_summary$subset_type %in% "All Students"]
    expect_true(all_students_p_is_na)

    # check that p-value is not NA for all other subset types
    subset_types_p_isnt_na <- p_summary$all_NA[!p_summary$subset_type %in% "All Students"]
    expect_equal(subset_types_p_isnt_na, rep(FALSE, length(subset_types_p_isnt_na)))
  })

})

describe('expand_subsets_agm_df',{

  #simulate the data
  subset_config_no_male <- subset_config_default %>%
    dplyr::filter(!subset_value %in% "Male")
  agm <- sanitize_display$simulate_agm(subset_config_no_male)
  agm_expanded <- agm %>%
    sanitize_display$expand_subsets_agm_df(
      desired_subset_config = subset_config_default,
      time_ordinal_column = "cycle_name"
    )
  default_combined_index <- c("reporting_unit_id", "cycle_name", "subset_value", "metric")

  it('expanded subset values that are missing from all cycles', {
    expect_false("Male" %in% agm$subset_value)
    expect_true("Male" %in% agm_expanded$subset_value)
  })

  it('expands subset values for just cycles 2+ in unimputed data', {

    # there should be the same number of "Male" values for the unimputed
    # data.frame compared with the imputed one. (i.e., it shouldn't matter
    # whether the subset_value was missing from the whole df or from
    # just one cycle)

    agm_unimputed <- create_missing_unimputed(subset_config_default)
    agm_unimputed_expanded <- agm_unimputed %>%
      sanitize_display$expand_subsets_agm_df(
        subset_config_default,
        time_ordinal_column = "cycle_name"
      )

    imputed_num_rows_male <- agm_expanded %>%
      dplyr::filter(subset_value %in% "Male") %>%
      nrow()
    unimputed_num_rows_male <- agm_unimputed_expanded %>%
      dplyr::filter(subset_value %in% "Male") %>%
      nrow()

    expect_equal(imputed_num_rows_male, unimputed_num_rows_male)

  })

  it('doesnt propagate non-subset indexes - no missing subsets', {

    # we want to add in missing subset values, but we don't
    # want to add in other indexes like cycle/reporting unit/metric
    # combos. So we'll remove one of these combos and check that
    # our expand values thing didn't put it right back in
    agm <- sanitize_display$simulate_agm(subset_config_default)
    agm_missing_metric <- agm %>%
      dplyr::filter(!(
        grepl("01", cycle_name) &
          reporting_unit_id %in% "Classroom_1" &
          metric %in% "mw2_1"
      ))
    agm_missing_metric_expanded <- sanitize_display$expand_subsets_agm_df(
      agm_missing_metric,
      subset_config_default,
      time_ordinal_column = "cycle_name"
    )

    is_missing_index <- agm_missing_metric_expanded$metric %in% "mw2_1" &
      agm_missing_metric_expanded$reporting_unit_id %in% "Classroom_1" &
      grepl("01", agm_missing_metric_expanded$cycle_name)

    expect_true(!any(is_missing_index))

  })

  it('doesnt propagate non-subset indexes when subsets are missing', {
    agm_missing_metric_expanded <- create_missing_unimputed(
      subset_config_default
    ) %>%
      dplyr::filter(!(
        grepl("01", cycle_name) &
          reporting_unit_id %in% "Classroom_1" &
          metric %in% "mw2_1"
      )) %>%
      sanitize_display$expand_subsets_agm_df(
        subset_config_default,
        time_ordinal_column = "cycle_name"
      )

    is_missing_index <- agm_missing_metric_expanded$metric %in% "mw2_1" &
      agm_missing_metric_expanded$reporting_unit_id %in% "Classroom_1" &
      grepl("01", agm_missing_metric_expanded$cycle_name)

    expect_true(!any(is_missing_index))

  })

  it('put NA values in pct_good field', {
    new_pct_good_vals <- agm_expanded$pct_good[
      agm_expanded$subset_value %in% "Male"
      ]
    expect_true(all(is.na(new_pct_good_vals)))
  })

  it('put NA values in se field', {
    new_se_vals <- agm_expanded$se[agm_expanded$subset_value %in% "Male"]
    expect_true(all(is.na(new_se_vals)))
  })

  it('put zeros in n field', {
    new_n_vals <- agm_expanded$n[agm_expanded$subset_value %in% "Male"]
    expect_true(all(!is.na(new_n_vals)))
    expect_true(all(new_n_vals == 0))
  })

  it('didnt change how agm is indexed', {
    unique_rows_by_index <- agm_expanded %>%
      dplyr::select(dplyr::one_of(default_combined_index)) %>%
      unique() %>%
      nrow()
    expect_equal(unique_rows_by_index, nrow(agm_expanded))
  })

  it('produces acceptable bar graph input after display handling', {
    agm_display <- agm_expanded %>%
      sanitize_display$pixellate_agm_df() %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name") %>%
      sanitize_display$display_anon_agm() %>%
      dplyr::mutate(
        question_text_wrapped = metric,
        reporting_unit_name_wrapped = reporting_unit_name
      ) %>%
      dplyr::mutate(max_cycle = max(cycle_name)) %>%
      dplyr::filter(cycle_name %in% max_cycle) %>%
      dplyr::ungroup()

    # There is a special error-checking function we should use, because it's
    # also used at run-time so it can examine live data, not just simulated
    # data.
    error <- sanitize_display$check_bar_graph_input(agm_display, subset_config_default)
    expect_null(error)

  })

  it('throws error when subset_values in data dont match config', {
    unmatching_agm <- sanitize_display$simulate_agm(subset_config_default) %>%
      dplyr::mutate(subset_type = gsub("_cat", "", subset_type))
    expect_error(
      sanitize_display$expand_subsets_agm_df(
        unmatching_agm,
        subset_config_default,
        time_ordinal_column = "cycle_name"
      )
    )
  })

  it('propagates accurately when an entire subset_type is missing', {

    agm <- data.frame(
      reporting_unit_id = "Organization_1",
      reporting_unit_type = "child_id",
      subset_type = c("race", "race", "All Students"),
      subset_value = c("Race Struct. Disadv.", "Race Struct. Adv. ", "All Students"),
      pct_good = c(.5, .5, .5),
      metric = "item1",
      cycle_ordinal = 1,
      se = .01,
      n = c(10, 10, 20)
    )

    subset_config <- data.frame(
      subset_type = c("race", "race", "gender", "gender"),
      subset_value = c("Race Struct. Disadv.", "Race Struct. Adv. ",
                       "Girl/Woman", "Boy/Man")
    )

    # gender is NOT one of the subset_types in agm
    expect_false("gender" %in% agm$subset_type)

    # but gender IS one of the subset_types in the subset_config
    expect_true("gender" %in% subset_config$subset_type)

    agm_expanded <- sanitize_display$expand_subsets_agm_df(
      agm_df_ungrouped = agm,
      desired_subset_config = subset_config,
      time_ordinal_column = "cycle_ordinal"
    )

    # reporting_unit_type is one of the fields that's supposed to be propagated.
    # It's a stand-in for similar fields that are also supposed to be propagated.
    expected_reporting_unit_type <- rep("child_id", nrow(agm_expanded))
    actual_reporting_unit_type <- agm_expanded$reporting_unit_type

    expect_equal(expected_reporting_unit_type, actual_reporting_unit_type)

  })

  it('propagates accurately when the "All Students" row is missing', {
    agm <- data.frame(
      reporting_unit_id = "Organization_1",
      reporting_unit_type = "child_id",
      subset_type = c("race", "race"),
      subset_value = c("Race Struct. Disadv.", "Race Struct. Adv. "),
      pct_good = c(.5, .5),
      metric = "item1",
      cycle_ordinal = 1,
      se = .01,
      n = c(10, 10)
    )

    subset_config <- data.frame(
      subset_type = c("race", "race", "gender", "gender"),
      subset_value = c("Race Struct. Disadv.", "Race Struct. Adv. ",
                       "Girl/Woman", "Boy/Man")
    )

    # All Students is NOT one of the subset_types in agm
    expect_false("All Students" %in% agm$subset_type)

    agm_expanded <- sanitize_display$expand_subsets_agm_df(
      agm_df_ungrouped = agm,
      desired_subset_config = subset_config,
      time_ordinal_column = "cycle_ordinal"
    )

    # "All Students" IS in agm_expanded$subset_type and subset_value
    expect_true("All Students" %in% agm_expanded$subset_type)
    expect_true("All Students" %in% agm_expanded$subset_value)

    # reporting_unit_type is one of the fields that's supposed to be propagated.
    # It's a stand-in for similar fields that are also supposed to be propagated.
    expected_reporting_unit_type <- rep("child_id", nrow(agm_expanded))
    actual_reporting_unit_type <- agm_expanded$reporting_unit_type

    expect_equal(expected_reporting_unit_type, actual_reporting_unit_type)


  })

  it('propagates accurately when a subset_type is missing just from a reporting unit', {

    agm <- data.frame(
      reporting_unit_id = c("Organization_1", "Organization_1",
                            "Organization_2", "Organization_2",
                            "Organization_2", "Organization_2"),
      reporting_unit_type = "child_id",
      subset_type = c("race", "race", "race", "race", "gender", "gender"),
      subset_value = c("Race Struct. Disadv.", "Race Struct. Adv. ",
                       "Race Struct. Disadv.", "Race Struct. Adv. ",
                       "Boy/Man", "Girl/Woman"),
      pct_good = .5,
      metric = "item1",
      cycle_ordinal = 1,
      se = .01,
      n = 10
    )

    org1_agm <- agm %>% filter(reporting_unit_id %in% "Organization_1")
    org2_agm <- agm %>% filter(reporting_unit_id %in% "Organization_2")

    # gender is in org 2 but not org 1
    expect_true("gender" %in% org2_agm$subset_type)
    expect_false("gender" %in% org1_agm$subset_type)

    subset_config <- data.frame(
      subset_type = c("race", "race", "gender", "gender"),
      subset_value = c("Race Struct. Disadv.", "Race Struct. Adv. ",
                       "Girl/Woman", "Boy/Man")
    )

    # and gender IS in the subset config
    expect_true("gender" %in% subset_config$subset_type)

    agm_expanded <- sanitize_display$expand_subsets_agm_df(
      agm_df_ungrouped = agm,
      desired_subset_config = subset_config,
      time_ordinal_column = "cycle_ordinal"
    )

    # reporting_unit_type is one of the fields that's supposed to be propagated.
    # It's a stand-in for similar fields that are also supposed to be propagated.
    expected_reporting_unit_type <- rep("child_id", nrow(agm_expanded))
    actual_reporting_unit_type <- agm_expanded$reporting_unit_type

    expect_equal(expected_reporting_unit_type, actual_reporting_unit_type)

  })

  it('throws an error when propagation by combined_index will lead to a duplicated index', {

    # if there's a column set to be propagated that varies within the index,
    # it will screw everything up and cause multiple duplicates of the index.
    # Avoid this by throwing an error.

    agm <- data.frame(
      reporting_unit_id = "Organization_1",
      subset_type = c("gender", "gender", "race", "race"),
      subset_value = c("Boy/Man", "Girl/Woman",
                       "Race Struct. Adv. ", "Race Struct. Disadv."),
      pct_good = .5,
      metric = "item1",
      cycle_ordinal = 1,
      extra_col = c(1, 1, 2, 2)
    )

    # the extra_col varies within the combined_index
    ec_summary <- agm %>%
      group_by(reporting_unit_id, metric, cycle_ordinal) %>%
      summarise(n_distinct_extra_col = n_distinct(extra_col)) %>%
      ungroup()

    # note the two distinct values
    expect_equal(ec_summary$n_distinct_extra_col, 2)


    subset_config <- data.frame(subset_type = c("gender", "gender", "race", "race"),
                                subset_value = c("Girl/Woman", "Boy/Man",
                                                 "Race Struct. Adv. ", "Race Struct. Disadv."))

    expect_error(sanitize_display$expand_subsets_agm_df(agm_df_ungrouped = agm,
                                                        desired_subset_config = subset_config,
                                                        time_ordinal_column = "cycle_ordinal"),
                 regexp = "The columns set to be propagated by the combined index are not " %+%
                   "unique within the combined index.")

  })

  it('throws an error where propagation by combined index and subset_type will lead to a duplicated index', {
    # This test is the same as the one above, but specifically varies the extra
    # col WITHIN subset type, and sets the extra col to be propagated within subset_type only

    agm <- data.frame(
      reporting_unit_id = "Organization_1",
      subset_type = c("gender", "gender"),
      subset_value = c("Boy/Man", "Girl/Woman"),
      pct_good = .5,
      metric = "item1",
      cycle_ordinal = 1,
      extra_col = c(1, 2)
    )

    # the extra_col varies within the combined_index AND subset_type
    ec_summary <- agm %>%
      group_by(reporting_unit_id, subset_type, metric, cycle_ordinal) %>%
      summarise(n_distinct_extra_col = n_distinct(extra_col)) %>%
      ungroup()

    # note the two distinct values
    expect_equal(ec_summary$n_distinct_extra_col, 2)


    subset_config <- data.frame(subset_type = c("gender", "gender", "race", "race"),
                                subset_value = c("Girl/Woman", "Boy/Man",
                                                 "Race Struct. Adv. ", "Race Struct. Disadv."))

    expect_error(sanitize_display$expand_subsets_agm_df(agm_df_ungrouped = agm,
                                                        desired_subset_config = subset_config,
                                                        time_ordinal_column = "cycle_ordinal",
                                                        cols_varying_by_subset_type = "extra_col"),
                 regexp = "The columns set to be propagated by the combined index are not " %+%
                   "unique within the combined index.")
  })

})



describe('anonymize_agm_df', {

  it('didnt change the combined index', {
    agm <- sanitize_display$simulate_agm(subset_config_default)
    agm_anonymized <- agm %>%
      sanitize_display$anonymize_agm_df(
        time_ordinal_column = "cycle_name",
        mask_within = agm_indexes
      )
    expect_equal(
      nrow(unique(agm_anonymized)),
      nrow(unique(agm_anonymized[c(agm_indexes, "cycle_name")]))
    )
  })

  it('filled in NA for small subset types', {
    small_subs_agm <- sanitize_display$simulate_agm(
      subset_config = subset_config_default,
      small_n_types = "race_cat"
    ) %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name")

    expect_true(
      all(is.na(small_subs_agm$pct_good[small_subs_agm$subset_type %in% "race_cat"]))
    )

    expect_true(
      all(is.na(small_subs_agm$se[small_subs_agm$subset_type %in% "race_cat"]))
    )
  })

  it('filled in NA for both subset values for small types', {
    agm_anon <- sanitize_display$simulate_agm(subset_config_default) %>%
      dplyr::mutate(n = ifelse(subset_value %in% "Male", 4, n)) %>%
      sanitize_display$pixellate_agm_df() %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name")

    # anonymize_agm_df should set pct_good values to NA for male AND female (becasuse
    # they share a subset_type)
    pct_good_male <- agm_anon$pct_good[agm_anon$subset_value %in% "Male"]
    pct_good_female <- agm_anon$pct_good[agm_anon$subset_value %in% "Female"]

    expect_true(all(is.na(c(pct_good_male, pct_good_female))))

  })

  it('didnt fill in NA for non-small subset value types', {
    agm_anon <- sanitize_display$simulate_agm(subset_config_default) %>%
      dplyr::mutate(n = ifelse(subset_value %in% "Male", 4, n)) %>%
      sanitize_display$pixellate_agm_df() %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name")
    # anonymize_agm_df should NOT set any other values to NA besides those
    # corresponding to the subset_type with one small-n cell
    pct_good_non_gender <- agm_anon$pct_good[
      !agm_anon$subset_type %in% "gender_cat"
    ]
    expect_true(!any(is.na(pct_good_non_gender)))
  })

  it('fills in NA for the p-values for small subset value types', {
    agm <- sanitize_display$simulate_agm(subset_config_default, small_n_types = "race_cat")

    agm_anonymized <- agm %>%
      sanitize_display$anonymize_agm_df(
        time_ordinal_column = "cycle_name",
        mask_within = agm_indexes
      )
    should_be_masked_ps <- agm_anonymized$p[agm_anonymized$mask_type]
    expect_equal(
      is.na(should_be_masked_ps),
      rep(TRUE, length(should_be_masked_ps))
    )
  })

  it('fills in NA to expanded subset agm when one subset_value of a type has zero and the other has > MIN_CELL', {
    subset_config_simulated <- data.frame(
      subset_type = c(rep("gender_cat", 2), rep("race_cat", 2), rep("target_group_cat", 2)),
      subset_value = c("Female", "Male", "Race Struct. Adv.", "Race Struct. Disadv.", "Not Target Grp.", "Target Grp."),
      stringsAsFactors = FALSE
    )

    agm_small_simulated_expanded <- sanitize_display$simulate_agm(
      subset_config = subset_config_default,
      n_teams = 1,
      n_classes = 1
      ) %>%
      dplyr::filter(!subset_value %in% "Target Grp.") %>%
      dplyr::select(-disadv) %>%
      sanitize_display$expand_subsets_agm_df(
        subset_config_simulated,
        time_ordinal_column = "cycle_name"
      )

    agm_anon <- sanitize_display$anonymize_agm_df(
      agm_small_simulated_expanded,
      time_ordinal_column = "cycle_name",
      min_cell = 5
    )

    tg_pct_good <- agm_anon %>%
      dplyr::filter(subset_type %in% "target_group_cat") %>%
      dplyr::pull(pct_good)

    # all target group values should be NA
    expect_equal(is.na(tg_pct_good), rep(TRUE, length(tg_pct_good)))

    # and there should be values corresponding to target group and not target group
    nrow_in_tg <- agm_anon %>%
      dplyr::filter(subset_value %in% "Target Grp.") %>%
      nrow()
    nrow_not_tg <- agm_anon %>%
      dplyr::filter(subset_value %in% "Not Target Grp.") %>%
      nrow()
    expect_equal(nrow_in_tg, nrow_not_tg)

  })

})

describe('display_anon_agm', {

  it('default case', {
    agm_most_recent_display <- sanitize_display$simulate_agm(subset_config_default) %>%
      sanitize_display$pixellate_agm_df() %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name") %>%
      dplyr::filter(cycle_name %in% max(cycle_name)) %>%
      dplyr::mutate(
        reporting_unit_name_wrapped = reporting_unit_name,
        question_text_wrapped = metric
      ) %>%
      sanitize_display$display_anon_agm()

    # There is a special error-checking function we should use, because it's
    # also used at run-time so it can examine live data, not just simulated
    # data.
    error <- sanitize_display$check_bar_graph_input(
      agm_most_recent_display,
      subset_config_default
    )

    # The double-bang unquotes the argument within `expect()` so the test
    # runner will display the _value_ of error$message when things go wrong,
    # which is more useful.
    # https://www.tidyverse.org/blog/2017/12/testthat-2-0-0/#quasiquotation-support
    # https://adv-r.hadley.nz/quasiquotation.html#unquoting
    expect_null(!!error)
  })

  it('generates good bar input with small n subset_types', {

    agm <- sanitize_display$simulate_agm(
       subset_config_default,
       small_n_types = "target_group_cat"
      ) %>%
      sanitize_display$pixellate_agm_df() %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name") %>%
      sanitize_display$display_anon_agm() %>%
      dplyr::mutate(
        question_text_wrapped = metric,
        reporting_unit_name_wrapped = reporting_unit_name
      )  %>%
      dplyr::filter(cycle_name == max(cycle_name))
    error <- sanitize_display$check_bar_graph_input(agm, subset_config_default)
    expect_null(!!error)

  })

  ###########

  it('generates good bar input with small n subset_values', {
    subset_config_no_female <- subset_config_default %>%
      dplyr::filter(!subset_value %in% "Female")

    agm <- sanitize_display$simulate_agm(subset_config_no_female) %>%
      sanitize_display$pixellate_agm_df() %>%
      sanitize_display$expand_subsets_agm_df(
        desired_subset_config = subset_config_default,
        time_ordinal_column = "cycle_name"
      ) %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name") %>%
      sanitize_display$display_anon_agm() %>%
      dplyr::mutate(
        question_text_wrapped = metric,
        reporting_unit_name_wrapped = reporting_unit_name
      ) %>%
      dplyr::filter(cycle_name == max(cycle_name))
    error <- sanitize_display$check_bar_graph_input(agm, subset_config_default)
    expect_null(!!error)
  })

  it('generates good bar input with missing unimputed cycle 2+ subsets',{

    agm <- create_missing_unimputed(subset_config_default) %>%
      sanitize_display$pixellate_agm_df() %>%
      sanitize_display$expand_subsets_agm_df(
        desired_subset_config = subset_config_default,
        time_ordinal_column = "cycle_name"
      ) %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name") %>%
      sanitize_display$display_anon_agm() %>%
      dplyr::mutate(
        question_text_wrapped = metric,
        reporting_unit_name_wrapped = reporting_unit_name
      ) %>%
      dplyr::filter(cycle_name %in% max(cycle_name))

      error <- sanitize_display$check_bar_graph_input(agm, subset_config_default)
      expect_null(!!error)
  })

  it('Fills in numeric values for masked NA values (these correspond to the grand mean)', {
    subset_config_simulated <- data.frame(
      subset_type = c(rep("gender_cat", 2), rep("race_cat", 2), rep("target_group_cat", 2)),
      subset_value = c("Female", "Male", "Race Struct. Adv.", "Race Struct. Disadv.", "Not Target Grp.", "Target Grp."),
      stringsAsFactors = FALSE
    )

    agm_anon <- sanitize_display$simulate_agm(
      subset_config = subset_config_default,
      n_teams = 1,
      n_classes = 1
      ) %>%
      dplyr::filter(!subset_value %in% "Target Grp.") %>%
      dplyr::select(-disadv) %>%
      sanitize_display$expand_subsets_agm_df(
        subset_config_simulated,
        time_ordinal_column = "cycle_name"
      ) %>%
      sanitize_display$anonymize_agm_df(time_ordinal_column = "cycle_name", min_cell = 5)
    tg_anon_pct_good <- agm_anon %>%
      dplyr::filter(subset_type %in% "target_group_cat") %>%
      dplyr::pull(pct_good)
    expect_equal(is.na(tg_anon_pct_good), rep(TRUE, length(tg_anon_pct_good)))

    agm_display <- sanitize_display$display_anon_agm(agm_anon)
    tg_display_pct_good <- agm_display %>%
      dplyr::filter(subset_type %in% "target_group_cat") %>%
      dplyr::pull(pct_good)
    expect_equal(is.na(tg_display_pct_good), rep(FALSE, length(tg_display_pct_good)))
  })

})
