# HTML PRINTING

util.html_table_from_model <- function(model){
  accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
  if( ! any(class(x) %in% accepted_models ) ){
    util.warn("Unaccepted model supplied!")
  }
  if( ! interactive() ){
    stargazer(
      model, 
      type="html", 
      star.cutoffs = c(.05, .01, .001),
      notes        = "", 
      notes.label = "1 star p<.05; 2 stars p<.01; 3 stars p<.001",
      notes.append = FALSE,
      single.row=TRUE
    )
  }
  else{
    util.print_pre(summary(model))
  }
}

util.html_table_data_frame <- function(df){
  # "grouped_df", "tbl_df" are dplyr type data.frames
  # ungroup to make them printable like a data.frame
  if(any(class(x) %in% c("grouped_df", "tbl_df"))){
    x <- data.frame(ungroup(x))
  }
  
  if( ! interactive() ){
    print(xtable(x, ...), 
          type="html",
          html.table.attributes = 
            getOption("xtable.html.table.attributes", 
                      "border=0, class='xtable'"), ...)  
  }else{
    return(x)
  }
}

util.html_table_psych_alphas <- function(x){
  # psych::alpha object, turn key data into data.frame
  if(! all(class(x) %in% c("psych","alpha"))){
    util.warn("Not a psych::alpha object!")
  } 
  # extract the alpha coefficients for printing
  x <- x$total
  util.html_table_data_frame(x)
}


# maybe we should remove this function
util.html_table <- function(x, ...) {
  accepted_models <- c("lmerMod","lm","aov","glm","glmerMod")
  accepted_psych_objects <- c("psych","alpha")
  accepted_dfs <- c("grouped_df", "tbl_df","data.frame")
  
  if( any(class(x) %in% accepted_models ) ){
    util.html_table_from_model(x)
  }
  
  if( all( class(x) %in% accepted_psych_objects ) ){
    util.html_table_psych_alphas(x)
  }
  
  if( any(class(x) %in% accepted_dfs )))
    util.html_table_data_frame(x)
  }
}

