\name{get_translation}
\alias{get_translation}
\title{Get translation for a string}
\usage{
get_translation(string, source.language = "sv", target.language = "en",
  domain = NULL)
}
\arguments{
  \item{string}{string to be translated}

  \item{source.language}{the language of the given string,
  column name in the translation table}

  \item{target.language}{target language, column name in
  the translation table}

  \item{domain}{domain to identify the object in the
  translation table, makes each entry of the translation
  table unique}
}
\description{
Get translation for a string from a translation table. See
\code{\link{set_translation_table}} for more information
about translation tables.
}

