.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my package")
}

.onLoad <- function(libname, pkgname) {
  options(digits.secs=3)
  options(scipen=999)
}

