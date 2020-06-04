options(
  repos = "http://cran.rstudio.com/", # needed to install any packages
  install.packages.check.source = "no" # prefer binary packages
)

if (!"modules" %in% utils::installed.packages()) {
  # Without the INSTALL_opts here, some environments using this .Rprofile file
  # can get in a recursive loop. R starts a new process to compile/build
  # packages (see [source code lines 603-604][1]), which may run this .Rprofile
  # script again. Specifying --use-vanilla tells R to ignore this file.
  #
  # [1]: https://www.rdocumentation.org/packages/utils/versions/3.6.2/source
  utils::install.packages("modules", INSTALL_opts="--use-vanilla")
}

if (!exists("import_module")) {
  modules::use("R/bootstrap.R")$install_module_imports()
  if (Sys.getenv('RSTUDIO') == "1") {
    print("rserve/.Rprofile installed `import_module`.")
  }
}
