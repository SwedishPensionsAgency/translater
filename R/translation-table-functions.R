#' Set/get translation table
#' 
#' Set/get translation table to use with \code{\link{get_translation}}
#' 
#' @param translation.table data frame with at least three columns: a source language, a target language, and object.name. 
#' @details The three columns of the translation table must have the names of the source language, e.g. \code{sv}, the target language, e.g. \code{en}, and \code{object.name}. The \code{object.name} makes each row unique together with one of the language columns. You can provide as many language columns as you need. 
#' @family translation table
#' @rdname translation.table
#' @export
set_translation_table <- function (translation.table) {
  translation.table <- as.data.frame(translation.table)
  for (i in 1:ncol(translation.table)) {
    if ("factor" %in% class(translation.table[[i]])) {
      translation.table[[i]] <- as.character(translation.table[[i]])
    }
  }
  .tables.env$translation.table <- translation.table
}

#' @family translation table
#' @rdname translation.table
#' @export
get_translation_table <- function () {
  if (is.null(.tables.env$translation.table)) {
    stop("There is no translation table available. Please provide a table with set_translation_table(your.translation.table).")
  }
  return(.tables.env$translation.table)  
}


#' Add missing translation string to a table of missing translations
#' 
#' Internal function to keep the strings that have no translation. 
#' 
#' @param string string in the source language
#' @param source.language source language (abbreviation) of the string
#' @param object.name name of the object to be translated, to identify the object in the translation table, makes each entry of the translation table unique
record_missing_translations <- function (string, source.language, object.name) {
  
  if (is.null(.tables.env$missing.translations)) {
    .tables.env$missing.translations <- .tables.env$translation.table[0,]
  }
  object.name <- ifelse(!is.null(object.name), object.name, "")
  if (!is_in_missing_translations(string, source.language, object.name)) {
    missing.translation <- as.list(rep("", ncol(.tables.env$missing.translations)))
    names(missing.translation) <- names(.tables.env$missing.translations)
    missing.translation[[source.language]] <- string
    missing.translation[["object.name"]] <- object.name
    .tables.env$missing.translations <- rbind(.tables.env$missing.translations, as.data.frame(missing.translation))
  }
}

#' Is the string already in the table for missing translations? 
#' 
#' @param string string in the source language
#' @param source.language source language (abbreviation) of the string
#' @param object.name name of the object to be translated, to identify the object in the translation table, makes each entry of the translation table unique
#' 
is_in_missing_translations <- function (string, source.language, object.name) {
  if (is.null(.tables.env$missing.translations)) {
    return(FALSE)
  }
  object.name <- ifelse(!is.null(object.name), object.name, "")
  return(nrow(.tables.env$missing.translations[
    .tables.env$missing.translations[[source.language]] == string & .tables.env$missing.translations[["object.name"]] == object.name, ]
  ) > 0)
}


#' Get missing translations as a table 
#' @param delete should the translation table for missing translations being deleted after returning it? 
#' @export
#' @family translation table
get_missing_translations <- function (delete = FALSE) {
  return.value <- .tables.env$missing.translations
  if (delete) {
    rm(missing.translations, envir = .tables.env)
  }
  return(return.value)
}