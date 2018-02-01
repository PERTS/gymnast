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
result <- dfc.compare_df_values(cars1, cars2, "id", verbose = TRUE)

# column missing in one df
cars3 <- select(cars2, -speed)
result <- dfc.compare_df_values(cars1, cars3, "id", verbose = TRUE)

# row and column missing in one df
cars4 <- filter(cars3, dist > 2)
result <- dfc.compare_df_values(cars1, cars4, "id", verbose = TRUE)

# Identical data frames with IDs but one value different
cars6 <- cars1
cars6[1, 1] <- "foo"
result <- dfc.compare_df_values(cars1, cars6, "id", verbose = TRUE)

# Identical data frames with duplicate IDs and matched-up values
cars7 <- cars1
cars8 <- cars1
cars7[51, ] <- cars7[50, ]
cars8[51, ] <- cars8[50, ]
result <- dfc.compare_df_values(cars7, cars8, "id", verbose = TRUE)

# Identical data frames with duplicate IDs and not-matched-up values
cars9 <- cars1
cars10 <- cars1
cars9[51, ] <- cars9[50, ]
cars10[51, ] <- cars10[50, ]
cars10[51, "speed"] <- "glorp"
result <- dfc.compare_df_values(cars9, cars10, "id", verbose = TRUE)


