#' Get translation for a string
#' 
#' Get translation for a string from a translation table. See \code{\link{set_translation_table}} for more information about translation tables. 
#' 
#' @param string string to be translated
#' @param source.language the language of the given string, column name in the translation table
#' @param target.language target language, column name in the translation table
#' @param object.name name of the object to be translated, to identify the object in the translation table, makes each entry of the translation table unique
#' @export
get_translation <- function (
  string, 
  source.language = "sv", 
  target.language = "en", 
  object.name = NULL) {
  
  # if the string contains only a number, no translation is necessary
  if (!is.na(suppressWarnings(as.numeric(string)))) {
    return(string)
  }
  
  translation.table <- get_translation_table()
  translation <- translation.table[(translation.table[[source.language]] == string & 
                                      translation.table[["object.name"]] == object.name), ]
  if (nrow(translation) > 0 && as.character(translation[[target.language]])[1] != "") {
    if (nrow(translation) > 1) {
      warning("There is more than one translation that matches your string. The first in the translation table is returnd.", "(string: '", string, "', source.language: '", source.language, "', target.language: '", target.language, "', object.name: '", object.name, "')")
    }
    return(as.character(translation[[target.language]])[1])
  } else {
    if (!is_in_missing_translations(string, source.language, object.name)) {
      warning("There is no translation for '", string, "'. Please add one to the translation table.", "(string: '", string, "', source.language: '", source.language, "', target.language: '", target.language, "', object.name: '", object.name, "')")
      record_missing_translations(string, source.language, object.name)
    }
    
    return(string)
  }
}
