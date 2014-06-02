#' Translate any string in an object
#' 
#' @param object object that contains strings to translate
#' @param source.language the language of the given string, column name in the translation table
#' @param target.language target language, column name in the translation table
#' @param object.name name of the object to be translated, to identify the object in the translation table, makes each entry of the translation table unique
#' @param all.names logical, should all elements be translated, hidden (names begining with '.') elements included? Passed to \link{\code{ls}}
#' @param skip character vector, containing names of object that should not be translated
#' @param verbose a logical. If TRUE, additional diagnostics are printed
#' @param level used for verbose purpose to keep track of the nesting in the object
#' @details If you would like to translate the strings in an object containing references to environments, e.g. a ggplot object, you should use \code{\link{copy_object}} to copy the original object. Otherwise the original object may not behave as you would expect. 
#' @export
translate_object <- function (
  object, 
  source.language = "sv", 
  target.language = "en", 
  object.name = NULL, 
  all.names = FALSE, 
  skip = c("required_aes", "objname", ".self", ".refClassDef"), 
  verbose = FALSE, 
  level = 1
) {
  get_translation_table()
  #browser()
  if (is.atomic(object) && !is.factor(object)) {
    if (is.character(object)) {
      # two methods to translate identify and translate strings, 
      # test performance of the two methods
      
      # 1. with subsetting
      unique.strings <- unique(object)
      if (verbose) {
        message(rep(" ", level-1), "Translated strings '", source.language, "'' : '", target.language, "'")
      }
      for (string in unique.strings) {
        browser()
        if (isTRUE(string != "")) {
          string.translation <- get_translation(string, 
                                                source.language, 
                                                target.language, 
                                                object.name)
          object[object == string] <- string.translation
        } else {
          string.translation <- string
        }
        if (verbose) {
          message(rep(" ", level), "'", string, "' : '", string.translation, "'")
        }  
      }
      
      # 2. looping through the whole vector
      #       for (i in 1:length(object)) {
      #         object[i] <- translate.string(object[i], target.language, object.name)
      #       }
    }
    
  } else if (!is.function(object) && !is.symbol(object)) {
    object.slotNames <- slotNames(object)
    if (isS4(object) && length(object.slotNames) > 0) {
      object.content <- object.slotNames
    } else if (length(object) > 0) {
      if (is.environment(object)) {
        if (environmentName(object) != environmentName(.GlobalEnv)){
          #           browser()
          #           object.envir.copy <- as.environment(as.list(object, all.names=all.names))
          #           attributes(object.envir.copy) <- attributes(object)
          object.content <- ls(object, all.names = all.names)
        } else {
          object.content <- NULL
        }
      } else if (is.factor(object)) {
        object.content <- c(1)
      } else {
        object.content <- 1:length(object)
      }
      if (verbose) {
        message(rep(" ", level-1), "### level ", level, " ###")
      }
    }
    if (exists("object.content")) {
      for (i in object.content) {
        if (verbose) {
          message(rep(" ", level-1), 
                  "---", 
                  i, 
                  ", ", 
                  ifelse(!isS4(object), ifelse(!is.null(names(object)[i]), names(object)[i], ""), ""), 
                  "---")
          if (is.null(get_element(object, i))) {
            message(rep(" ", level), "Is NULL.")
          }
          if (i %in% skip | isTRUE(names(object)[i] %in% skip)) {
            message(rep(" ", level), "Skipped.")
          }
        }
        if (!is.null(get_element(object, i)) && !(i %in% skip | isTRUE(names(object)[i] %in% skip))) {
          if (is.factor(object)) {
            object.to.translate <- levels(object)
          } else {
            object.to.translate <- get_element(object, i)
          }
          object.i.translation <- translate_object(object.to.translate, 
                                                   source.language = source.language, 
                                                   target.language = target.language, 
                                                   object.name = object.name, 
                                                   skip = skip, 
                                                   verbose = verbose, 
                                                   level = level + 1)
          if (isS4(object)) {
            slot(object, i) <- object.i.translation
          } else if (is.factor(object)) {
            levels(object) <- object.i.translation
          } else {
            object[[i]] <- object.i.translation
          }
        }
      }
    } else {
      if (verbose) {
        message(rep(" ", level), "Is of length zero (0).")
      }
    }
    
  }
  return(object)
}
