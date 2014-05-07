\name{set_translation_table}
\alias{get_translation_table}
\alias{set_translation_table}
\title{Set/get translation table}
\usage{
set_translation_table(translation.table)

get_translation_table()
}
\arguments{
  \item{translation.table}{data frame with at least three
  columns: a source language, a target language, and
  domain.}
}
\description{
Set/get translation table to use with
\code{\link{get_translation}}
}
\details{
The three columns of the translation table must have the
names of the source language, e.g. \code{sv}, the target
language, e.g. \code{en}, and \code{domain}. The
\code{domain} makes each row unique together with one of
the language columns. You can provide as many language
columns as you need.
}

