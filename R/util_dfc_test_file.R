##########################################################################
###
###     util_dfc test file
###     Dan Greene, 1/9/18
###
###     This script runs a series of tests on util_dfc.
###
###     Depends on util.R
###


# REMOVE SECTION BEFORE FINALIZING
setwd("~/Sites/gymnast/R")
source("util.R")
source("util_dfc.R")
library(dplyr)



# Identical dfs
cars1 <- cars
cars2 <- cars
cars1$id <- 1:nrow(cars1)
cars2$id <- 1:nrow(cars2)
util_dfc.compare_dfs(cars1, cars2)

# column missing in one df
cars3 <- select(cars2, -speed)
util_dfc.compare_dfs(cars1, cars3)

# row and column missing in one df
cars4 <- filter(cars3, dist > 2)
util_dfc.compare_dfs(cars1, cars4)

# Identical dfs with identical single ID column
util_dfc.compare_dfs(cars1, cars2, id_cols = "id")

# Comparing IDs when one row and one column are missing from one df
util_dfc.compare_dfs(cars1, cars4, id_cols = "id")



##### Test ideas:

# Big real-world data frame!
# Probably going to need to only display the head of identifier vectors,
# and show the proportion of identifiers that are shared

# Completely different data frames with nothing in common!