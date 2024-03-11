#' Check if packages are required and install if not.
#'
#' Utility function.
#' @param package List of packages to be installed.
#' @export

check_packages <- function(package){

  new.packages <- package[!(package %in% installed.packages()[,"Package"])]
  if (length(new.packages)) 
    install.packages(new.packages, dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}
