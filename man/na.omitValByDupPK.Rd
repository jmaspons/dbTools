% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/primaryKey.R
\name{na.omitValByDupPK}
\alias{na.omitValByDupPK}
\title{Omit \code{NA}s in values for duplicated registers by PK and return a \code{data.frame} with a unique register for each PK if possible.}
\usage{
na.omitValByDupPK(x, pk, collapse)
}
\arguments{
\item{x}{a \code{data.frame}}

\item{pk}{columns with the primary key}

\item{collapse}{a \code{character}. If no missing, duplicated values are collapsed in a string separated by \code{collapse} character.}
}
\value{
Return \code{x} removing duplicated rows by PK omitting \code{NA}s. If \code{collapse} is missing, duplicated rows by PK and different values
are kept.
}
\description{
Omit \code{NA}s in values for duplicated registers by PK and return a \code{data.frame} with a unique register for each PK if possible.
}
