#' Copy any object
#' 
#' Copy any object with all environments. Saves the object and loads it again in an empty environment. These steps are neccessary to copy all environments and environment bindings. 
#' 
#' @param object object to be copied
#' @export
copy_object <- function(object){
  tmp <- tempfile("RData")
  save(object, file = tmp, compress = "gzip")
  tmp_env <- new.env(parent=emptyenv())
  load(file = tmp, envir = tmp_env)
  object.copy <- getElement(tmp_env, ls(tmp_env))
  rm(tmp_env)
  return(object.copy)
}