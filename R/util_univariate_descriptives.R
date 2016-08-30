###############################################################
###
###     util_univariate_descriptives.R
###     Used to generate descriptions of variables.
###
###     The main call is:
###     ud.describe(    data , 
###                     [descriptives] , 
###                     [codebook] 
###                 )     
###
###
###     Depends on util.R
###
###############################################################

# wrap ud.helper functions in an object to protect the namespace
ud.helper <- list(
    
    mean = function(x){
        if( util.is_vector_of_numbers(x) ){
            return(mean(as.numeric(x), na.rm=TRUE))
        }else{
            return(NA)
        }
    }
    ,
    prop_blank = function(x){
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
    },
    n_unique = function(x){
        length(unique(x))
    }
)

# these must be defined outside ud.helper because they reference
# functions defined in it
ud.helper$default_desc_cols = list(
    "pct_NA" = ud.helper$prop_blank,
    "mean" = ud.helper$mean,
    "sd" = ud.helper$sd,
    "obs_min" = ud.helper$obs_min,
    "obs_max" = ud.helper$obs_max
)

ud.helper$default_categorical = list(
    "pct_NA" = ud.helper$prop_blank,
    "prop" = ud.helper$mean,
    "n_unique" = ud.helper$n_unique
)   


ud.describe <- function(
    data,                             # the dataset to describe
    descriptives=ud.helper$default_desc_cols,  # lists descriptives functions to run
    codebook=NULL                     # data.frame merged on "variable_name" col
){
    
    # return data.frame with a row for each column of "data"
    # columns include: 
    #   * "variable_name" containing name of each variable from "data"
    #   * all codebook columns merged on "variable_name" column
    #   * each names(descriptives) containing value returned by descriptives function
    
    # make univariate descriptives (ud) data.frame to load with descriptives
    ud_columns <- c("variable_name", names(descriptives) )
    ud <- data.frame(matrix(NA, nrow = ncol(data), ncol = length(ud_columns))) 
    names(ud) <- ud_columns
    ud$variable_name <- names(data)
    
    # convert logicals to numbers for calculation of proportions
    data <- util.apply_columns(data, ud.logical_to_numeric )
    
    # apply each descriptive function to each column
    for( calc_func_name in names(descriptives) ){
        ud[,calc_func_name] <- sapply(data, descriptives[[calc_func_name]])
    }
    
    # append codebook by variable_name (if it exists)
    if( ! is.null(codebook) ){
        ud_with_cb <- merge(codebook, ud, by="variable_name", all.y=TRUE, sort=FALSE)
    } else{
        ud_with_cb <- ud   
    }
    
    ud_with_cb %>% 
        util.round_df(digits=2) %>%
        return()
}

