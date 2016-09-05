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

ds.helper$glm_formula_to_varlist <- function(formula){
    # provided with a canonical ds-style glm formula
    # it extracts a ds-style glm variable list
    # canonical ds-style glm formulas:
    #   dv ~ iv [ * mod ] [ + cov1 + cov2 + ... ]
    #   details: 
        #   iv is first predictor
        #   if moderator present, it is second predictor 
        #   and "*" separates iv and moderator 
        #   covs are any predictors following moderator
        #   "*" interactions are not supported outside moderator
        #   to use interactions in covariates, create manually
    
    varlist <- list( dv=NA, iv=NA, mod=NA, covs=NA )
    split_on_tilde <- str_split(formula,"[ ]*~[ ]*")[[1]]
    
    # check if formula has dvs and predictors
    if( length(split_on_tilde) != 2){
        stop("invalid ds.glm formula provided: " %+% formula)
    } else if(nchar(split_on_tilde[1]) == 0 | 
              nchar(split_on_tilde[2]) == 0 ){
        stop("invalid ds.glm formula provided: " %+% formula)
    }
    
    varlist$dv <- split_on_tilde[1]
    predictor_str <- split_on_tilde[2]
    
    # this block finds the iv and moderator
    # there are moderators if there is a star and
    # it comes before any pluses, if pluses are present
    split_at_star <- str_split(predictor_str,"[ ]*\\*[ ]*")[[1]]
    if( length(split_at_star) > 1){
        # there might be a moderator (because there's a star)
        # but it might also be a non-canonical ds.glm formula
        pre_star  <- split_at_star[1]
        post_star <- split_at_star[2]
        if( length(split_at_star) > 2 | grepl( "\\+", pre_star) ){
            "ds.glm supports [0,1] stars" %+%
                " and only between the 1st and 2nd predictor," %+%
                " but this formula was provided: " %+% formula %>%
                stop()
        }
        # mod is 1st term post_star and before + or end
        varlist$iv  <- pre_star
        varlist$mod <- str_split(post_star,"[ ]*\\+[ ]*")[[1]][1]
    } else{
        # there was no moderator, so the iv is the first predictor
        # before a plus or before the end of the predictors string
        varlist$iv <- str_split(predictor_str,"[ ]*\\+[ ]*")[[1]][1]
    }
    
    # find any covariate(s)
    match_plus_or_star <- "[ ]*\\+[ ]*|[ ]*\\*[ ]*"
    predictors <- str_split( predictor_str, match_plus_or_star)[[1]]
    if( sum( ! predictors %in% varlist ) > 0){
        varlist$covs <- predictors[ ! predictors %in% varlist ]
    }
    
    return(varlist)
}

ds.helper$map_dv_to_glm_family <- list(
    numeric = "gaussian",
    boolean = "binomial"
)

ds.glm <- function( formula, data, mod_family=NULL ){
    # run the appropriate model, unless mod_family override is supplied
    dv_variable <- ds.helper$glm_formula_to_varlist(formula)[["dv"]]
    if( is.null(mod_family) ){
        dv_var_type <- ds.helper$variable_type(data[,dv_variable])
        mod_family  <- util.recode(
                            dv_var_type,
                            names(ds.helper$map_dv_to_glm_family),
                            unlist(ds.helper$map_dv_to_glm_family)
                        )
    }
    if( ! mod_family %in% ds.helper$map_dv_to_glm_family ){
        util.warn("Unsupported model family requested: " %+% mod_family )
        mod_obj <- NULL
    } else{
        mod_obj <- glm( formula , data, family=mod_family )
    }
    return(mod_obj)
}

ds.glms <- function( formulas, data, family=NULL ){
    # run each formula on data.frame supplied in data argument
    # use the outcome type to select the model, or override with family
    models <- list()
    if( is.null( family ) ){
        for( i in 1:length(formulas)){
            models[[i]] <- ds.glm( formulas[i], data=data )
        }        
    } else{
        for( i in 1:length(formulas)){
            models[[i]] <- ds.glm( formulas[i], data=data, family=family )
        }    
    }
    return(models)
}

ds.helper$glm1_features_full <- c(
    "dv", "iv_apa", "int_apa",
    "iv", "mod", "covs", "aic", "family", "n", "df",
    "dv_mean" ,
    "iv_coef", "iv_se", "iv_stat", "iv_p", 
    "mod_coef", "mod_se", "mod_stat", "mod_p",
    "int_coef", "int_se", "int_stat", "int_p"
)
ds.helper$glm1_features_apa <- c(
    "dv","iv_apa","int_apa","covs"
)


ds.helper$validate_glm1_model <- function(glm_model){
    # validate that the model meets the ds.glm1 model spec
    # ds.glm1 only supports numerical or boolean IVs and MODs
    # this is simplicity of display
    varlist <- ds.helper$glm_formula_to_varlist(glm_model$formula)
    iv <- glm_model$model[,varlist$iv]
    iv_var_type <- ds.helper$variable_type(iv)
    is_valid <- FALSE
    
    # check iv suitability
    if( iv_var_type %in% c("boolean","numeric") ){
        is_valid <- TRUE
    } else{
        "ds.glm1 spec only allows ivs and moderators of type " %+% 
            "boolean or numeric. Supplied iv is: "  %+% iv_var_type %+%
            " in formula: " %+% glm_model$formula %>%
            util.warn()
    }
    if( ! is.na(varlist$mod) ){ 
        mod <- glm_model$model[,varlist$mod]
        mod_var_type <- ds.helper$variable_type(mod)
        if( ! mod_var_type %in% c("boolean","numeric") ){
            "ds.extract_glm1_stats only supports mods of type " %+% 
                "boolean or numeric. Supplied mod is: " %+% mod_var_type %+%
                " in formula: " %+% glm_model$formula %>%
                util.warn()
            is_valid <- FALSE
        }
    }
    return(is_valid)
}


ds.extract_glm1_stats <- function( glm_model ){
    # extracts frequently used statistics from glm_model
    
    if( ! ds.helper$validate_glm1_model(glm_model)){
        # glm1 only supports numerical or boolean IVs and MODs
        # others will issue a warning
        return(list())
    }
    
    fl <- list()  # fl = "feature list"
    varlist <- ds.helper$glm_formula_to_varlist(glm_model$formula)
    
    # populate the feature list
    fl$dv     <- varlist$dv
    fl$family <- glm_model$family$family
    fl$n   <- sum(!util.is_blank(glm_model$y))    
    fl$df  <- glm_model$df.residual
    fl$aic <- glm_model$aic
    fl$iv     <- varlist$iv
    fl$mod    <- varlist$mod
    fl$covs   <- paste(varlist$covs, collapse=", ")
    fl$dv_mean   <- mean(glm_model$y, na.rm=TRUE)
    
    # extract coefficients
    coefs <- summary(glm_model)$coefficients
    
    # use of column and row numbers seems dangerous;
    # however order should be consistent when 
    # ds.glm1 spec is used
    # also, irregularities between different model types
    # e.g., t vs z for guassian and binomial families
    # and level is appended to row.names when TRUE/FALSE
    fl$iv_coef <- coefs[2,1]
    fl$iv_se <- coefs[2,2]
    fl$iv_stat <- coefs[2,3]
    fl$iv_p <- coefs[2,4]
    
    # assemble apa string
    if(fl$family %in% "binomial"){
        fl$iv_apa <- 
            fl$iv %+% 
            ", OR=" %+% round(exp(fl$iv_coef),3) %+%
            ", z=" %+% round(fl$iv_stat,3) %+%
            ifelse(fl$iv_p < .001,", p<.001",", p=" %+% round(fl$iv_p,3))
            
    } else if(fl$family %in% "gaussian"){
        fl$iv_apa <- 
            fl$iv %+% 
            ", b=" %+% round(fl$iv_coef,3)  %+%
            ", t(" %+% fl$df %+% ")" %+% "=" %+% round(fl$iv_stat,3) %+%
           ifelse(fl$iv_p < .001,", p<.001",", p=" %+% round(fl$iv_p,3))
    }
    
    if( !is.na(fl$mod) ){
        fl$mod_coef <- coefs[3,1]
        fl$mod_se <- coefs[3,2]
        fl$mod_stat <- coefs[3,3]
        fl$mod_p <- coefs[3,4]
        
        int_row <- nrow(coefs)
        
        fl$int_coef <- coefs[int_row,1]
        fl$int_se <- coefs[int_row,2]
        fl$int_stat <- coefs[int_row,3]
        fl$int_p <- coefs[int_row,4]

        if(fl$family %in% "binomial"){
            fl$int_apa <- fl$mod %+% " x " %+% fl$iv %+%
                ", OR=" %+% round(exp(fl$int_coef),2) %+%
                ", z=" %+% round(fl$int_stat,3) %+%
                ifelse(fl$int_p < .001,", p<.001",", p=" %+% round(fl$int_p,3))
            
        } else if(fl$family %in% "gaussian"){
            fl$int_apa <- fl$mod %+% " x " %+% fl$iv %+%
                ", b=" %+% round(fl$int_coef,2)  %+%
                ", t(" %+% fl$df %+% ")" %+% "=" %+% round(fl$int_stat,3) %+%
                ifelse(fl$int_p < .001,", p<.001",", p=" %+% round(fl$int_p,3))
        }
    } else{
        fl$int_apa <- ""
    }
    
    return(fl)
}

ds.glm_table <- function( 
    models,
    extraction_func = ds.extract_glm1_stats,  # extracts glm features
    columns = ds.helper$glm1_features_apa
){
    
    fls <- lapply( models , function(glm_model){
        as.data.frame(extraction_func(glm_model))
    })
    
    # remove empty feature lists
    # (may happen if ds.glm1 spec is violated)
    sanitized_fls <- list()
    for( i in 1:length(fls) ){
        if( nrow(fls[[i]]) > 0 ){
            sanitized_fls[[length(sanitized_fls) + 1]] <- fls[[i]]
        }
    }
    if(length(sanitized_fls) > 0){
        feature_table <- util.rbind_union(sanitized_fls)
        return(feature_table[,columns])
    } else{
        return(data.frame())
    }
}


ds.helper.unit_test <- function(){
    
    assert_expected_output <- function( func_name , input, expected_output ){
        # warn if evaluated func_name(input) != expected_output
        # errors messages are not compared, but errors can be expected
        
        # func_name passed as string rathern than func itself
        # so the func_name can be included warnings
        func <- eval(parse(text=func_name))
        
        real_output <- tryCatch(
                            func(input),
                            error = function(e) e
                        )
        
        both_are_errors <-  any(class(real_output) %in% "error") & 
                            any(class(expected_output) %in% "error")
        outputs_identical <- identical(expected_output, real_output)
        
        error_message <- ""
        if("error" %in% class(real_output)){
            error_message <- "\nError raised in tested function: " %+%
                real_output$message
        }
        
        if( ! ( both_are_errors | outputs_identical ) ){
            "assert_expected_output failed in " %+%
                func_name %+% " when passed '" %+%
                paste(input,collapse=", ") %+% "'" %+% 
                ". \n" %+% error_message %>%
                util.warn()
        }
    }
    
    test__glm_formula_to_varlist <- function(){
        
        input_output_map <- list(
            list(
                input  =  "dv ~ iv",
                output = list(dv="dv", iv="iv", mod=NA, covs=NA)
            ),
            list(
                input  = "dv ~ iv + cov1 + cov2",
                output = list(
                    dv="dv", iv="iv", mod=NA, covs=c("cov1","cov2")
                )
            ),
            list(
                input  =  "dv ~ iv * mod",
                output = list(
                    dv="dv", iv="iv", mod="mod", covs=NA
                )
            ),
            list(
                input  =  "dv ~ iv * mod + cov1 + cov2",
                output = list(
                    dv="dv", iv="iv", mod="mod", covs=c("cov1","cov2")
                )
            ),
            list(
                input  =  "dv~iv*mod+cov1+cov2",
                output = list(
                    dv="dv", iv="iv", mod="mod", covs=c("cov1","cov2")
                )
            ),
            list(
                input  =  "dv ~ iv * mod * cov1 + cov2",
                output = simpleError("bad input formula")
            ),
            list(
                input  =  "",
                output = simpleError("bad input formula")
            ),
            list(
                input  =  "dv ~ iv + mod + cov1 * cov2",
                output = simpleError("bad input formula")
            ),
            list(
                input  =  "dv ~ ",
                output = simpleError("bad input formula")
            ),
            list(
                input  =  " ~ a",
                output = simpleError("bad input formula")
            )
        )
        
        for( io in input_output_map ){
            assert_expected_output( 
                "ds.helper$glm_formula_to_varlist",
                io$input,
                io$output
            )
        }
        
    }

    test__variable_type <- function(){
        input_output_map <- list(
            list(
                input = c("dog","dog","dog"),
                output = "invariant"
            ),
            list(
                input = c(1,"dog","cat"),
                output = "categorical"
            ),
            list(
                input = c(1,0,0),
                output = "boolean"
            ),
            list(
                input = c(1,TRUE,0),
                output = "boolean"
            ),
            list(
                input = c(1,20,0),
                output = "numeric"
            ),
            list(
                input = "cat" %+% rep(1:21) ,
                output = "multitudinous"
            ),
            list(
                input = c("",NA),
                output = "invariant"
            )
        )
        
        for( io in input_output_map ){
            assert_expected_output( 
                "ds.helper$variable_type",
                io$input,
                io$output
            )
        }
    }


    test__glm_formula_to_varlist()   
    test__variable_type()   
}

ds.helper.unit_test()








