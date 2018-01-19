##########################################################################
###
###     util_dfc test file
###     Dan Greene, 1/9/18
###
###     This script runs a series of tests on util_dfc.
###
###     Depends on util.R and util_data_summaries.R
###

setwd("~/Sites/gymnast/R")
source("util.R")
source("util_data_summaries.R")
source("util_dfc.R")
library(dplyr)

# identical dfs
cars1 <- cars
cars2 <- cars
cars1$id <- 1:nrow(cars1)
cars2$id <- 1:nrow(cars2)
result <- dfc.compare_dfs(cars1, cars2)

# column missing in one df
cars3 <- select(cars2, -speed)
result <- dfc.compare_dfs(cars1, cars3)

# row and column missing in one df
cars4 <- filter(cars3, dist > 2)
result <- dfc.compare_dfs(cars1, cars4)

# Identical dfs with identical single unique ID column
result <- dfc.compare_dfs(cars1, cars2, id_cols = "id")

# Comparing IDs when one row and one column are missing from one df
result <- dfc.compare_dfs(cars1, cars4, id_cols = "id")

# with wrong ID
result <- dfc.compare_dfs(cars1, cars4, id_cols = "foo")

# Completely different data frames with nothing in common!
result <- dfc.compare_dfs(cars, iris)

# Data frames with duplicate rows and duplicate IDs with different data
cars5 <- cars4
cars5[50, ] <- cars5[49, ]
result <- dfc.compare_dfs(cars1, cars5, id_cols = "id")

# Identical data frames with IDs but one value different
cars6 <- cars1
cars6[1, 1] <- "foo"
result <- dfc.compare_dfs(cars1, cars6, id_cols = "id")


##### Test ideas:

# Big real-world data frame!

