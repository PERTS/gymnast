# @todo (Sarah): rewrite these so they are agnostic as to directory and don't
# create global variables so they can be included in the gymnast package.

###########################
## Load packages and source script
#source("~/perts_analyses/common/util.R")
#source("~/perts_analyses/common/util_qualtrics_cleaning.R")
#
#
###########################
## Check it out!
#
## With a KU qualtrics dataset including pdd tags
#
#qdf_test1 <- read.csv("~/Dropbox (PERTS)/PERTS shared data/test_data_qualtrics_cleaning/qualtrics_test1.csv")
#qdf_test1_clean <- qc.clean_qualtrics(qdf_test1,remove_unnamed_columns = TRUE)
#head(qdf_test1_clean)
#
## did the numerics get properly recasted?
#table(qdf_test1_clean$toi_manip1_post,useNA="ifany")
#class(qdf_test1_clean$toi_manip1_post) # numeric
#
## were the right column names retained?
#names(qdf_test1_clean)
#
## With an mturk dataset from long ago (from grad when I was in grad school)
#
#qdf_test2 <- read.csv("~/Dropbox (PERTS)/PERTS shared data/test_data_qualtrics_cleaning/qualtrics_test2.csv")
#qdf_test2_clean <- pq.clean_qualtrics(qdf_test2)
#head(qdf_test2_clean)
#table(qdf_test2_clean$strategies_1)
#class(qdf_test2_clean$strategies_1)
#
## note that this "year of birth" field is interpreted as a character type because one person wrote in
## "united states" which I think is right - I don't think I want to make judgments here as to what
## the values should be. BUT, I could imagine a scenario where we throw a warning if there's a column
## that had like 90% numeric values or something like that (e.g., "yob" had XX% numeric values - was
## it supposed to be numeric?)
#table(qdf_test2_clean$yob)
#
#################################################################
## test that other functions work besides the delimiter function
#
##### Function that takes the first 10 characters of the string ####
#first_10_characters <- function(x){
#  # takes the first 10 characters in any string
#  substr(x,1,10)
#}
#
#pq.clean_qualtrics(qdf_test1, extract_column_name=first_10_characters) -> test1_first10
#names(test1_first10)
#
#pq.clean_qualtrics(qdf_test2, extract_column_name=first_10_characters) -> test2_first10
#names(test2_first10)
#
##### Function that takes the first 10 characters that occur AFTER a particular string, "con" ####
#
#first_10_after_con <- function(x){
#  # takes the first 10 characters that appear after the string "con"
#  xxx_positions <- str_locate(x, "con")
#  substr(x,xxx_positions[,2], xxx_positions[,2] + 10)
#}
#
#test1_first10_after_con <- pq.clean_qualtrics(qdf_test1, extract_column_name=first_10_after_con)
#
## Print the column names where the user-defined function is relevant
## (that have the string "con")
#names(test1_first10_after_con)[
#  grepl("con", util.to_character(qdf_test1[1,]))
#  ]
#
#test2_first10_after_con <- pq.clean_qualtrics(qdf_test2, extract_column_name=first_10_after_con)
#
## Print the column names where the user-defined function is relevant
## (that have the string "con")
#names(test2_first10_after_con)[
#  grepl("con", util.to_character(qdf_test2[1,]))
#  ]
#
#
##### Function that takes the first N characters after the string "con"
#
#first_N_after_con <- function(x, nchars){
#  # takes the first N characters that appear after the string "con"
#  xxx_positions <- str_locate(x, "con")
#  substr(x,xxx_positions[,2], xxx_positions[,2] + nchars)
#}
#
#test1_firstN_after_con <- pq.clean_qualtrics(qdf_test1, extract_column_name=first_N_after_con, nchars=8)
#
## Print the column names where the user-defined function is relevant
## (that have the string "con")
#names(test1_firstN_after_con)[
#  grepl("con", util.to_character(qdf_test1[1,]))
#  ]
#
#test2_firstN_after_con <- pq.clean_qualtrics(qdf_test2, extract_column_name=first_N_after_con, nchars=8)
#
## Print the column names where the user-defined function is relevant
## (that have the string "con")
#names(test2_firstN_after_con)[
#  grepl("con", util.to_character(qdf_test2[1,]))
#  ]
