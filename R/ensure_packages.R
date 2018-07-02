ensure_packages <- function (libs, prefer_type = "binary") {
  # Install `libs` if they aren't installed already.
  #
  # Args:
  #   libs - character, packages to maybe install
  #   prefer_type - character, default "binary", or "source", sets
  #     options(install.packages.check.source) which is an indirect way of
  #     controlling whether we use binary or source installation, since the
  #     `type` argument of install.packages() is platform-dependent and thus
  #     unreliable.
  if (prefer_type == "binary") {
    # Prefer binary packages, even if the source version is farther ahead.
    # This is default because it's so much faster to install a binary.
    options(install.packages.check.source = "no")
  } else if (prefer_type == "source") {
    options(install.packages.check.source = NULL)
  }

  installed <- utils::installed.packages()[,"Package"]
  to_install <- libs[!(libs %in% installed)]

  if (length(to_install)) {
    utils::install.packages(
      to_install,
      repos = 'http://cran.us.r-project.org'
      # type = 'binary'  # doesn't work for some platforms, e.g. linux
    )
  }
}
