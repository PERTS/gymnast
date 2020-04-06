modules::import('dplyr', 'filter')

logging <- import_module("logging")
util <- import_module("util")

`%+%` <- paste0

append_scales <- function(
    survey_df,
    scale_variables_table,
    add_z_score = TRUE
){
    # survey_df — cleaned, subject-level survey dataset (e.g., from Qualtrics
    # or Qit)  scale_variables_table contains one row per scale item and
    # specifies information about the item including the item’s intended
    # membership in a psychometric scale (e.g., item “gms1” is part of the
    # growth mindset scale).
    # add_z_score — boolean value indicating whether z-score version of each
    # scale should be computed

    # take all the unique values of the scale column of scale_variables_table,
    # and for each value, look up the column name associated with it in
    # survey_variables_table.

    # filter any non-scale variables from scale_variables_table
    scale_variables_table <- filter(
        scale_variables_table,
        !util$is_blank(scale)
    )

    scales <- unique(scale_variables_table$scale)

    for(scale_name in scales){

        # Find all the column names in survey_df corresponding to these
        # var_names
        scale_items <- scale_variables_table$var_name[
            scale_variables_table$scale %in% scale_name
        ]

        # look up the corresponding column names in survey_df
        survey_scale_column_names <- names(survey_df)[
            names(survey_df) %in% scale_items
        ]

        missing_items <- scale_items[
            !scale_items %in% survey_scale_column_names
        ]

        # If any scale items are present in survey_df, compute row means and
        # append to survey_df.
        # If none are present, throw a warning and do nothing.
        if(length(survey_scale_column_names) > 0){
            # compute and append row means
            scale_means <- util$row_means(survey_df[survey_scale_column_names])
            survey_df[[scale_name]] <- scale_means
            # optionally compute zscore of rowMeans(), or rowMeans() of the
            # standardized scale items
            if(add_z_score){
                scale_means_z <- util$z_score(scale_means)
                survey_df[[scale_name %+% "_z"]] <- scale_means_z
            }
            # warn if any items in scale_variables_table are missing from survey
            # data
            if(length(missing_items) > 0){
                logging$warning("The following items were missing from survey data "
                    %+% "from scale " %+%
                    scale_name %+% ": " %+%
                    paste0(missing_items, collapse = ", ")
                )
            }
        # if no items were found in the survey data, throw a warning
        } else{
            logging$warning("No items corresponding to " %+%
                scale_name %+%
                " were found in survey data. No scale was computed."
            )
        }
    }
    return(survey_df)
}
