
#' Find registers with missing Foreign Key in the related table
#'
#' @param x a \code{data.frame} with a Foreign Key \code{fk} linking to \code{y}.
#' @param y a \code{data.frame} with a Primary Key \code{pk} linking to \code{x}.
#' @param fk columns containing the Foreign Key (column names or indexes) for \code{x}.
#' @param pk columns containing the Primary Key (column names or indexes) for \code{y}.
#' @param searchCandidates if \code{TRUE}, use \code{\link{agrep}} to find similar PKs for the missing FKs.
#' @param ... parameters passed to \code{\link{agrep}}.
#'
#' The order of \code{fk} and \code{pk} must match.
#' @return a \code{data.frame} with the registers in \code{x} with a missing Foreign Key. If \code{searchCandidates}
#'   is \code{TRUE}, the result have an attribute candidates with suggested FKs.
#' @export
#'
#' @examples
missingFK<- function(x, y, fk, pk, searchCandidates=FALSE){
  if (missing(fk))
    fk<- intersect(names(x), names(y))

  if (missing(pk))
    pk<- fk

  if (length(fk) > 1){
    fkString<- do.call("paste", x[, fk])
    pkString<- do.call("paste", y[, pk])
  }else{
    fkString<- x[, fk]
    pkString<- y[, pk]
  }
  misFK<- unique(fkString[!fkString %in% pkString])
  out<- x[fkString %in% misFK, ]

  if (searchCandidates){
    candidates<- lapply(misFK, function(x){
      fkCandidate<- agrep(x, pkString, value=TRUE, ...)
    })
    names(candidates)<- misFK
    attr(out, "candidates")<- candidates
  }

  return(out)
}
