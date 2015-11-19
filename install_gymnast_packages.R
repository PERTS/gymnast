# Check for, and install if missing, packages used by gymnast.

gymnast_install <- function () {
    dependencies <- c(
        "dplyr", "reshape2", "ggplot2",
        "grid", "scale", "psych",
        "xtable", "stargazer", "stringr",
        "Hmisc", "knitr", "digest",
        "lme4", "lmerTest", "stargazer",
        "formatR"
    )
    for (lib_name in dependencies) {
        print(lib_name)
        # Require will not raise an error if the package isn't installed,
        # rather it will just return false.
        if (!require(lib_name, character.only = TRUE)) {
            install.packages(lib_name)
            # Using library here will raise an error if the installation didn't
            # work.
            library(lib_name, character.only = TRUE)
        }
    }
}
