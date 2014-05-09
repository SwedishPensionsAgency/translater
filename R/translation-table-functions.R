#' Set/get translation table
#' 
#' Set/get translation table to use with \code{\link{get_translation}}
#' 
#' @param translation.table data frame with at least three columns: a source language, a target language, and domain. 
#' @details The three columns of the translation table must have the names of the source language, e.g. \code{sv}, the target language, e.g. \code{en}, and \code{domain}. The \code{domain} makes each row unique together with one of the language columns. You can provide as many language columns as you need. 
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
#' @param string string in the source language
#' @param source.language source language (abbreviation) of the string
#' @param domain domain to identify the object in the translation table, makes each entry of the translation table unique
#' @family translation table
record_missing_translations <- function (string, source.language, domain) {
  if (is.null(.tables.env$missing.translations)) {
    .tables.env$missing.translations <- .tables.env$translation.table[0,]
  }
  #browser()
  domain <- ifelse(!is.null(domain), domain, "")
  if (nrow(.tables.env$missing.translations[
    .tables.env$missing.translations[[source.language]] == string & .tables.env$missing.translations[["domain"]] == domain, ]
           ) == 0) {
    missing.translation <- as.list(rep("", ncol(.tables.env$missing.translations)))
    names(missing.translation) <- names(.tables.env$missing.translations)
    missing.translation[[source.language]] <- string
    missing.translation[["domain"]] <- domain
    .tables.env$missing.translations <- rbind(.tables.env$missing.translations, as.data.frame(missing.translation))
  }
}


#' Get missing translations as a table 
#' @export
#' @family translation table
get_missing_translations <- function () {
  return(.tables.env$missing.translations)
}