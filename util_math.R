

# Basic Data Transformation
util.z_score <- function(x){
  if(class(x) %in% "data.frame"){
    do.call(cbind,lapply(x, util.z_score))
  }
  else{ ( x - mean(x,na.rm=T) ) / sd(x,na.rm=T) }
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

