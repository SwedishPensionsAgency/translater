#' Get element from object
#' 
#' Get element from vectors, matrices, arrays, lists, environments, reference class objects, etc. 
#' 
#' @param object object from which to extract element(s) or in which to replace element(s)
#' @param name A literal character string or a name (possibly backtick quoted) or an index
#' @export
get_element <- function (object, name) {
  if (typeof(object) == "list") {
    class(object) <- "list"
  }
  if (is.environment(object) && !.hasSlot(object, name)) {
    if (exists(name, envir = object, inherits = FALSE)) {
      # non initialized variables in function environmnets exists but can be returned by 'get'
      return.value <- try(get(name, envir = object), silent=TRUE)
      if ("try-error" %in% class(return.value)) {
        return(NULL)
      } else {
        return(return.value)
      }
    } else {
      return(NULL)
    }
    
  } else if (is.numeric(name)) {
    if (isS4(object)) {
      return(slot(object, name))
    } else {
      return(object[[name]])
    }
  } else {
    return(getElement(object, name))
  }
}