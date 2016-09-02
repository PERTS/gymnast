###############################################################
###
###     util_data_summaries.R
###     Used to summarize data.
###
###     To summarize individual variable, call:
###
###     ds.summarize_by_column(    data , 
###                     [func_list] , 
###                     [codebook] 
###                 )     
###
###     It is anticipated that other data summary functions
###     will be added later.
###
###     Depends on util.R
###
###############################################################

# wrap ds.helper functions in an object to protect the namespace
ds.helper <- list(
    
    mean = function(x){
        if( util.is_vector_of_numbers(x) ){
            return(mean(as.numeric(x), na.rm=TRUE))
        }else{
            return(NA)
        }
    }
    ,
    pct_blank = function(x){
        mean(util.is_blank(x)) * 100
    }
    ,
    sd = function(x){
        sd(x, na.rm=TRUE)
    }
    ,
    obs_min = function(x){
        if( util.is_vector_of_numbers(x) ){
            return(min(as.numeric(x),na.rm=T))
        }else{
            return(NA)   
        }
    }
    ,
    obs_max = function(x){
        if( util.is_vector_of_numbers(x) ){
            return(max(as.numeric(x),na.rm=T))
        }else{
            return(NA)   
        }
    }
    ,
    logical_to_numeric = function(x){
        # if a value is logical (boolean), convert to numeric for calculation
        if(class(x) %in% "logical"){ x <- as.numeric(x) }
        return(x)
    }
    ,
    n_unique = function(x){
        # includes NA
        length(unique(x))
    }
)

# these must be defined outside ds.helper because they reference
# functions defined in it
ds.helper$default_col_funcs = list(
    "pct_NA" = ds.helper$pct_blank,
    "mean" = ds.helper$mean,
    "sd" = ds.helper$sd,
    "obs_min" = ds.helper$obs_min,
    "obs_max" = ds.helper$obs_max
)

ds.helper$default_categorical_col_funcs = list(
    "pct_NA" = ds.helper$pct_blank,
    "n_unique" = ds.helper$n_unique
)   


# ds.summarize_by_column_group - to generate scales, etc.


ds.summarize_by_column <- function(
    data,                             # the dataset to describe
    func_list=ds.helper$default_col_funcs,  # lists func_list functions to run
    codebook=NULL,                     # data.frame merged on "variable_name" col
    digits=2                          # round descriptions to 2 digits
){
    
    # return data.frame with a row for each column of "data"
    # columns include: 
    #   * "variable_name" containing name of each variable from "data"
    #   * all codebook columns merged on "variable_name" column
    #   * each names(func_list) containing value returned by func_list function
    
    # make univariate func_list (ud) data.frame to load with func_list
    ud_columns <- c("variable_name", names(func_list) )
    ud <- data.frame(matrix(NA, nrow = ncol(data), ncol = length(ud_columns))) 
    names(ud) <- ud_columns
    ud$variable_name <- names(data)
    
    # convert logicals to numbers for calculation of proportions
    data <- util.apply_columns(data, ds.helper$logical_to_numeric )
    
    # apply each descriptive function to each column
    for( calc_func_name in names(func_list) ){
        ud[,calc_func_name] <- sapply(data, func_list[[calc_func_name]])
    }
    
    # append codebook by variable_name (if it exists)
    if( ! is.null(codebook) ){
        if( any(duplicated(codebook$variable_name)) ){
            util.warn("Codebook had duplicated variable names! Cannot append.")
        } else{
            ud_with_cb <- merge(codebook, ud, by="variable_name", all.y=TRUE, sort=FALSE)            
        }
    } else{
        ud_with_cb <- ud   
    }
    
    ud_with_cb %>% 
        util.round_df(digits=digits) %>%
        return()
}

