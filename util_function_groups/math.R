

util.z_score <- function(x){
  # calculate the z-score of vector or for each vector in a data.frame
  if(class(x) %in% "data.frame"){
    util.apply_columns(x,util.z_score)
  }
  else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
}

util.row_means <- function(x){
  # like base::rowMeans, but returns self if vector
  if(is.vector(x)) return(x)
  rowMeans(x, na.rm = TRUE)
}

# round all numeric columns
util.round_df <- function(DF, digits=2, show_caution=TRUE){
  vec_list <- lapply( DF, function(vec){
    if( is.numeric(vec) ){
      vec <- round(vec,digits)
    }
    else{ 
      if(show_caution){
        util.caution("Any characters will make all columns characters.")
      }
    }
    return( vec )
  })
  do.call( cbind, vec_list ) %>%
    as.data.frame()
}

