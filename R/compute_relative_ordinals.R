# @ to-do make error message non-CTC specific.
# @to-do add timeline as an optional input, to be specified if there
# are possible times that don't happen to be represented in a particular dataset.
# and/or if the time variable is not ordinal

# Note--this function ignores any gaps in the inputs

logging <- import_module("logging")

`%+%` <- paste0

compute_relative_ordinals <- function(
    times,
    ids,
    id_reference_time_table,
    # @to-do change all refs from subject to ID
    ordered_timeline = NULL
){
    # times - a vector of data representing
    # some time-dependent thing (e.g., terms, years).

    # ids - a vector of the same length as times indicating
    # subject id values (e.g., student ID)

    # note that times and ids should be vectors of data in
    # the same data.frame!

    # id_reference_time_table-a 2-column table where one
    # field contains unique id values and the other contains values
    # of times relative to which relative ordinals
    # should be computed (e.g., the name
    # of the term in which an intervention took place.) The
    # column names of this table MUST be 'id' and 'reference_time'

    # ordered_timeline is an optional vector of all the possible
    # time values that can appear in the dataset, ordered in ascending
    # temporal order (i.e., from past to future). The order in which
    # values appear in the timeline will determine their order for
    # the purposes of computing relative terms.
    # If timeline is not specified, it will be created
    # by sorting all the unique values in the times

    # @to-do stop if times and ids are different lengths

    # preserve the original order of rows in each input vector so that
    # we can return a vector in the same order as the inputs
    df <- data.frame(ids, times)
    df$original_order <- seq(1:nrow(df))

    # stop if duplicates are found in id_reference_time_table
    # (because we don't know which reference times to use for each participant!)
    if(any(duplicated(id_reference_time_table$id))){
        n_duplicated_ids <- sum(util.duplicated_all(id_reference_time_table$id))
        stop("In compute_relative_ordinal(), " %+% n_duplicated_ids %+%
                 " duplicated id values were discovered. Cannot compute " %+%
                 "relative ordinals until id_reference_time_table contains " %+%
                 "exactly one reference time for each id value. For example, " %+%
                 "each student in the baseline records should have exactly one ug__cohort value.")
    }

    reference_table_names <- c("id", "reference_time")
    if(!all(names(id_reference_time_table) %in% reference_table_names)){
        wrong_names <- names(id_reference_time_table)[
            !names(id_reference_time_table) %in% reference_table_names
        ]
        stop(
            "In compute_relative_ordinals, wrong name values in " %+%
                "id_reference_time_table: " %+%
                paste0(wrong_names, collapse = ", ") %+%
                ". Should be " %+%
                paste0(reference_table_names, collapse = ", ") %+%
                "."
        )
    }

    # create a master ordinal value for all values of the time variable,
    # but warn if not all the values in id_reference_time_table appear in
    # the times
    if(is.null(ordered_timeline)){
        ordered_timeline <- sort(unique(times))
    }

    # warn if any reference times are missing from the ordered timeline,
    # since these will result in NA values in the relative_ordinals output.
    # (because if a reference time isn't in the timeline, we don't know
    # how to order it!)
    reference_times <- id_reference_time_table$reference_time
    missing_reference_times <- reference_times[
        !reference_times %in% ordered_timeline
    ]
    if(length(missing_reference_times) > 0){
        logging$warning(
            "In compute_relative_ordinals, the following time " %+%
            "values are present in the id_reference_time_table " %+%
            "but not in the ordered_timeline: " %+%
            paste0(missing_reference_times, collapse = ", ") %+%
            ". Subjects with these reference times will receive NA " %+%
            "values in the relative ordinal output because there is " %+%
            "no information about how to order their reference time."
        )
    }

    # @to-do change to absolute
    absolute_ordinal <- data.frame(
        times = ordered_timeline,
        absolute_ordinal = seq(1:length(ordered_timeline)),
        stringsAsFactors = FALSE
    )

    # merge the master ordinal values into the subject referecne times to
    # find each subject's reference_time, or the ordinal value
    # corresponding to each subject's reference time
    subject_reference_time_table_mo <- merge(
        id_reference_time_table,
        absolute_ordinal,
        by.x = "reference_time",
        by.y = "times",
        all.x = TRUE,
        all.y = FALSE
    ) %>%
        rename(reference_time_ordinal = absolute_ordinal)

    rel_term_df <- df %>%
        # merge the master ordinal values into the data
        merge(
            .,
            absolute_ordinal,
            by = "times",
            all.x = TRUE,
            all.y = FALSE
        ) %>%
        rename(time_ordinal = absolute_ordinal) %>%
        # merge in the reference times and their corresponding ordinal values
        merge(
            .,
            subject_reference_time_table_mo[c("id", "reference_time_ordinal")],
            by.x = "ids",
            by.y = "id",
            all.x = TRUE,
            all.y = FALSE
        )  %>%
        # Now, relative_term is just time_var_ordinal - start_term_ordinal
        # i.e., if a given time has an ordinal value of "4", and the ordinal value
        # of the subject's reference time is also 4, then this should be relative_ordinal == 1
        mutate(
            relative_ordinal = time_ordinal - reference_time_ordinal
        ) %>%
        # restore the original order of the input vectors
        arrange(original_order)

    # return the relative_ordinal values
    return(rel_term_df$relative_ordinal)
}

