.onLoad <- function (libname, pkgname) {
  #Create a environment in the package environment for storing the translation tables 
  assign(".tables.env", new.env(), envir= asNamespace(pkgname))
}
