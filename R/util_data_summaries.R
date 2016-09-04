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
ds.helper <- list()

ds.helper$mean <- function(x){
    if( util.is_vector_of_numbers(x) ){
        return(mean(as.numeric(x), na.rm=TRUE))
    }else{
        return(NA)
    }
}
    
ds.helper$pct_blank = function(x){
    mean(util.is_blank(x)) * 100
}

ds.helper$sd = function(x){
    sd(x, na.rm=TRUE)
}

ds.helper$obs_min = function(x){
    if( util.is_vector_of_numbers(x) ){
        return(min(as.numeric(x),na.rm=T))
    }else{
        return(NA)   
    }
}

ds.helper$obs_max = function(x){
    if( util.is_vector_of_numbers(x) ){
        return(max(as.numeric(x),na.rm=T))
    }else{
        return(NA)   
    }
}

ds.helper$logical_to_numeric = function(x){
    # if a value is logical (boolean), convert to numeric for calculation
    if(class(x) %in% "logical"){ x <- as.numeric(x) }
    return(x)
}

ds.helper$n_unique = function(x){
    # includes NA
    length(unique(x))
}

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

ds.build_formulas <- function(
    meta_formula = NULL, # @todo: enable specification through formula 
    variable_vectors = NULL, # @todo: meta_formula can reference named vectors
    
    dvs = c(),           # dependent variables each run independently
    ivs = c(),           # independent variables each run independently
    mods = c(""),        # moderators each run independently, default no mod
    covs = c(),          # covariates included all together or none
    cov_groups = list() # customize cov groups instead of default all or none

){
    # build vector of model formulas for each unique combination of 
    # dv x iv x mod x cov_group
    # if covs supplied, cov_group gets one entry for no covs (unadjusted) 
    # and one for all covs supplied in covs argument (adjusted)
    
    # @todo: to support meta_formula, make a new function to turn
    # meta formulas into variable vectors and call on self
    if( ! is.null(meta_formula) ){
        util.warn("meta_formulas are not yet supported :-(")
        # variable_list <- ds.meta_formula_to_variable_list(meta_formula)
        # do.call(ds.build_formulas, variable_list) %>%
        #   return()
    } 
    #
    
    if( length(dvs) == 0 | length(ivs) == 0 ){
        util.warn("dvs and ivs must both be")
    }
    
    if( length(covs) > 0 ){
        # if covs is supplied, use it to define adjusted and unadjusted
        # covariate groups with all and none of the covs, respectively
        cov_groups <- list()
        cov_groups[["Unadusted"]] <- ""
        cov_groups[["Adjusted"]] <- covs
    } else if( length(cov_groups) == 0){
        cov_groups <- list( Unadjusted = "" )
    }
    
    # build a vector of strings for each kind of variable
    # in prep for stringing together into a forumla string
    # i.e., append operators to non-empty entries
    dv_strs  <- dvs %+% " ~ "
    iv_strs  <- ivs
    mod_strs <- ifelse( mods %in% "" , "", " * " %+% mods)
    
    # turn covariate list into a vector of formula strings
    # this is more complicated because covariates get grouped
    cov_groups_strs <- lapply(cov_groups, function(cov_vec){
        if( is.null(cov_vec) | identical(cov_vec,"") ){
            # there is an empty (unadjusted) cov_group
            return("")
        } else{
            return( " + " %+% paste(cov_vec, collapse=" + ") )
        }
    }) %>% unlist()
    
    # build the formula data frame (fdf) as each combination of
    # variable strings
    fdf <- expand.grid(
        cov_str = cov_groups_strs,
        mod_str = mod_strs,
        iv_str  = iv_strs,
        dv_str  = dv_strs
    )

    # assemble the formula grid into 
    formula_vec <- fdf$dv_str %+% fdf$iv_str %+% fdf$mod_str %+% fdf$cov_str
    return(formula_vec)

}

ds.helper$variable_type <- function(x){
    # return variable type as
    # (Note that NAs are ignored)
    #  boolean - x contains exactly c(0,1) or c(TRUE,FALSE)
    #  numeric - x is numbers 
    #  categorical - x is a string or factor with up to 20 levels
    #  multitudinous - x is string or factor with > 20 levels
    #  invariant - only 0 or 1 values, not counting NAs

    boolean <- all( x %in% c(0,1,NA) ) & all( c(0,1) %in% x )
    
    if( boolean ){
        return("boolean")
    } else if( util.is_vector_of_numbers(x) ){
        return("numeric")
    }
    
    n_unique <- x[!util.is_blank(x)] %>%
        unique() %>%
        length()
    
    if(n_unique < 2){
        return("invariant")
    } else if( n_unique > 20){
        return("multitudinous")
    } else{
        return("categorical")
    }
}

# examples
ds.helper$variable_type(c("dog","dog","dog"))
ds.helper$variable_type(c(1,"dog","cat"))
ds.helper$variable_type(c(1,0,0))
ds.helper$variable_type(c(1,TRUE,0))
ds.helper$variable_type(c(1,0,20))
ds.helper$variable_type(c("cat1","cat2","cat3"))
ds.helper$variable_type("cat" %+% rep(1:21))

ds.helper$extract_formula_dv <- function(formula){
    str_split(formula,"[ ]*~[ ]*")[[1]][1]
}

ds.glm <- function( formula, data, family=NULL ){
    # run the appropriate model, unless family override is supplied
    dv_variable <- ds.helper$extract_formula_dv(formula)
    dv_var_type <- ds.helper$variable_type(data[,dv_variable])
    
    switch( dv_var_type ,
        "numeric" = lm(formula=formula, data=data)
            
    )
}

ds.glms <- function( formulas, data, families=NULL ){
    # run each formula on data.frame(s) supplied in data argument
    # use the outcome type to select the model, or 
    
}










