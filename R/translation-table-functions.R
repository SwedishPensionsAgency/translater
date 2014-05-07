#' Set/get translation table
#' 
#' Set/get translation table to use with \code{\link{get_translation}}
#' 
#' @param translation.table data frame with at least three columns: a source language, a target language, and domain. 
#' @details The three columns of the translation table must have the names of the source language, e.g. \code{sv}, the target language, e.g. \code{en}, and \code{domain}. The \code{domain} makes each row unique together with one of the language columns. You can provide as many language columns as you need. 
#' @family translation table
#' @rdname translation.table
#' @export
set_translation_table <- function(translation.table){
  options(translater.translation.table = translation.table)
}

#' @family translation table
#' @rdname translation.table
#' @export
get_translation_table <- function(){
  return(getOption("translater.translation.table", default=NULL))
}
