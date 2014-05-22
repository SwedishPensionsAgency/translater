\name{record_missing_translations}
\alias{record_missing_translations}
\title{Add missing translation string to a table of missing translations}
\usage{
record_missing_translations(string, source.language, domain)
}
\arguments{
  \item{string}{string in the source language}

  \item{source.language}{source language (abbreviation) of
  the string}

  \item{domain}{domain to identify the object in the
  translation table, makes each entry of the translation
  table unique}
}
\description{
Internal function to keep the strings that have no
translation.
}

