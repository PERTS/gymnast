# When used as a submodule, bootstrap gymnast in top-level code like this:
#
# bootstrap <- modules::use("gymnast/R/bootstrap.R")
# bootstrap$install_dependencies(gymnast_base_path = "gymnast")
# bootstrap$install_module_imports() # import_module() now in global env

dep_checker <- function(row) {
  tryCatch(
    pkgload::check_dep_version(row[["package"]], row[["version"]]),
    warning = function(w) {
      modules::depend(row[["package"]], row[["version_number"]])
    }
  )
}

modules::export("install_dependencies")
install_dependencies <- function(gymnast_base_path = NULL) {
  ops_original <- options(
    repos = "http://cran.rstudio.com/", # needed to install any packages
    install.packages.check.source = "no" # prefer binary packages
  )

  # Bootstrap `modules` and `jsonlite` so we can install dependencies.
  if (!"modules" %in% utils::installed.packages()) {
    utils::install.packages("modules")
  }
  modules::depend("jsonlite")
  modules::depend("pkgload")

  if (!is.null(gymnast_base_path)) {
    deps_df <- pkgload::pkg_desc(gymnast_base_path)$get_deps()
    imports_df <- deps_df[deps_df$type %in% "Imports", ]
    imports_df$version_number <- substring(imports_df$version, 4)
    apply(imports_df, 1, dep_checker)
  }

  # Install the correct version of all dependencies.
  if (!file.exists("package.json")) {
    warning("Can't install dependencies for local project, missing package.json.")
  } else {
    deps <- unlist(jsonlite::read_json("package.json")$R$dependencies)
    for (package in names(deps)) {
      modules::depend(package, deps[[package]])
    }
  }

  options(ops_original)
}

modules::export("install_module_imports")
install_module_imports <- function() {
  # Creates a specialized function, `import_module`, in the global environment.
  # This function has two important features:
  # 1. It will search for a file matching what you requested in a list of
  #    directories. By default it looks in the current working directory and the
  #    `R/` subdirectory. If you have the R_PATH environment variable set, or if
  #    you define "importPaths" in your package.json file, you can have it
  #    search additional places.
  # 2. It copies itself into the scope/environment of every module it loads
  #    (normally the `modules` package creates a totally isolated environment)
  #    so that code can in turn load more modules.
  r_path_raw <- Sys.getenv("R_PATH")
  if (r_path_raw %in% "") {
    r_path <- character()
  } else {
    r_path <- strsplit(r_path_raw, ":")[[1]]
  }

  import_paths <- NULL
  if (file.exists("package.json")) {
    import_paths <- jsonlite::read_json("package.json")$R$importPaths
  }
  if (is.null(import_paths)) {
    import_paths <- character()
  }

  all_paths <- c(".", "R", r_path, import_paths)

  import_module <- function(module_path) {
    resolved_paths <- character()
    for (base_path in all_paths) {
      p <- file.path(base_path, module_path)
      if (file.exists(p)) {
        resolved_paths <- c(p, resolved_paths)
      }
      p.R <- paste0(p, ".R")
      if (file.exists(p.R)) {
        resolved_paths <- c(p.R, resolved_paths)
      }
    }

    if (length(resolved_paths) == 0) {
      stop(paste0(
        "Could not resolve path: ",
        module_path,
        "; R_PATH is: \"",
        paste(all_paths, collapse = ":"),
        "\""
      ))
    }

    if (length(resolved_paths) > 1) {
      stop(paste0(
        "Could not resolve path: ",
        module_path,
        "; found multiple matches: ",
        paste(resolved_paths, collapse = ", ")
      ))
    }

    resolved_path <- resolved_paths # now guaranteed to be length 1

    # Import the found module file, but also inject the import function
    # from the global environement. The `amodule` function makes the immediate
    # parent environment available, so we have access to `resolved_path`.
    modules::amodule({
      import_module <- globalenv()$import_module
      eval(parse(resolved_path))
    })
  }

  assign("import_module", import_module, envir = globalenv())
}
