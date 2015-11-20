

# Data Binding
# keep "intersection" or "union" of columns
util.rbind_many <- function(dfs, keep="intersection"){
  # what columns should remain in the r-bound df?
  columns <- names(dfs[[1]])
  for(i in 2:length(dfs)){
    if(keep == "intersection"){
      columns <- columns[ columns %in% names(dfs[[i]])]
    }else{ # keep == "union"
      columns <- unique(c( columns, names(dfs[[i]]) ))
    }
  }
  if(length(columns) < 2){
    stop("2+ columns must match for result to be a data.frame.")
  }
  for(i in 1:length(dfs)){
    # fill out missing columns
    dfs[[i]][, columns[! columns %in% names(dfs[[i]]) ]] <- NA
    # remove extraneous columns
    dfs[[i]] <- dfs[[i]][, columns]
  }
  do.call(rbind,dfs)
}

# rbind list of dfs, keep intersecting column names
util.rbind_intersection <- function(dfs){
  util.rbind_many(dfs, keep="intersection")
}

# rbind list of dfs, keep union of column names
util.rbind_union <- function(dfs){
  util.rbind_many(dfs, keep="union")
}

# demo of util.rbind_many
# x <- data.frame( b=c(1,2,3), a=c(1,2,3), z=c(1,2,3) )
# y <- data.frame( b=c(4,5,6), a=c(4,5,6), d=c(4,5,6))
# z <- data.frame( b=c(7,8,9), a=c(7,8,9), e=c(7,8,9))
# dfs <- list(x,y,z)
# util.rbind_union(dfs)
# util.rbind_intersection(dfs)


