# Check for, and install if missing, packages used by gymnast.

gymnast_install <- function () {
    dependencies <- c(
        "dplyr", "reshape2", "ggplot2",
        "grid", "psych","scales",
        "xtable", "stargazer", "stringr",
        "Hmisc", "knitr", "digest",
        "lme4", "lmerTest", "formatR"
    )
    for(lib_name in dependencies){
        message(paste0("Loading... ",lib_name))
        # Require returns FALSE if packages failed
        if( !require(lib_name, character.only = TRUE) ){
            install.packages(lib_name)
            # library raises error if installation failed
            library(lib_name, character.only = TRUE)
        }
    }
}
gymnast_install()
