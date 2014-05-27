#' Get element from object
#' 
#' Get element from vectors, matrices, arrays, lists, environments, reference class objects, etc. 
#' 
#' @param object object from which to extract element(s) or in which to replace element(s)
#' @param name A literal character string or a name (possibly backtick quoted) or an index
#' @export
get_element <- function (object, name) {
  if (is.environment(object) && !.hasSlot(object, name)) {
    return(get(name, envir = object))
  } else {
    return(getElement(object, name))
  }
}