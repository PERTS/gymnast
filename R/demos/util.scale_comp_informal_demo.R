
# read in some survey data (survey_df)
test_df <- data.frame(matrix(ncol=0, nrow=10))

# create scales
test_df$gms_1 <- sample(c(1:6), nrow(test_df), replace = TRUE)
test_df$gms_2 <- sample(c(1:6), nrow(test_df), replace = TRUE)

test_df$purpose_1 <- sample(c(1:5), nrow(test_df), replace = TRUE)
test_df$purpose_2 <- sample(c(1:5), nrow(test_df), replace = TRUE)

# create scale_variables_table
scale_variables_table <- data.frame(
    var_name = c("gms_1", "gms_2", "purpose_1", "purpose_2", "purpose_3", "loc_1", "other_var"),
    scale = c("gms", "gms", "purpose", "purpose", "purpose", "locus_control", NA)
) %>%
    util.to_character()

# View scale_variables_table
# Note:
    # There is one item (purpose_3) that appears in the table but not test_df
    # There is one scale (locus_control) that isn't represented in test_df at all
util.print_pre(scale_variables_table)


# observe correct scales computed:
sc.append_scales(test_df, scale_variables_table) %>%
    util.print_pre

# There should be a warning for the missing purpose_3 item, 
# and a warning that no locus_control items were found