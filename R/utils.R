#' Check non numeric values
#'
#' Show the values that can not be casted to numeric
#'
#' @param x an atomic vector.
#'
#' @return A character vector with the values from the input vector that can't be casted to numeric.
#' @export
non.numeric<- function(x){
  if (is.numeric(x)) return(character())

  tmp<- suppressWarnings(as.numeric(as.character(x)))
  sel<- which(!is.na(x) & is.na(tmp))

  if (length(sel) == 0){
    return(character())
  }else{
    return(unique(x[sel]))
  }
}

#' Change data type to numeric if no data is lost
#'
#' @param x a variable of type \code{data.frame}, \code{list} or atomic.
#' @param warning if `TRUE`, throw a warning indicating which values can not be casted to numeric.
#'
#' @return returns the same variable as a numeric type only if there is no data lost. For \code{data.frame} and \code{list},
#'  it change the data type per column/element.
#' @export
as.numeric_noDataLost<- function(x, warning=FALSE) UseMethod("as.numeric_noDataLost")

#' @export
as.numeric_noDataLost.numeric<- function(x, warning=FALSE){
  return(x)
}

#' @export
as.numeric_noDataLost.factor<- function(x, warning=FALSE){
  tmp<- suppressWarnings(as.numeric(as.character(x)))
  as.numeric_noDataLost(tmp, warning=warning)
}

#' @export
as.numeric_noDataLost.character<- function(x, warning=FALSE){
  tmp<- suppressWarnings(as.numeric(x))
  sel<- which(!is.na(x) & is.na(tmp))

  if (length(sel) == 0) return(tmp)
  else {
    if (warning) warning("Non numeric values: ", paste(unique(tmp[sel]), collapse=", "))
    return(x)
  }
}

#' @export
as.numeric_noDataLost.data.frame<- function(x, warning=FALSE){
  data.frame(lapply(x, as.numeric_noDataLost, warning=warning), stringsAsFactors=FALSE, check.names=FALSE)
}

#' @export
as.numeric_noDataLost.list<- function(x, warning=FALSE){
  lapply(x, as.numeric_noDataLost, warning=warning)
}


#' \code{rbind} adding non-matching columns filled with \code{NA}s
#'
#' @param ... \code{data.frame}s.
#' @inheritParams base::cbind
#'
#' @return returns a \code{data.frame} with all the columns and rows
#' @export
rbind_addColumns<- function(...) UseMethod("rbind_addColumns")

#' @export
rbind_addColumns.data.frame<- function(..., deparse.level=1, make.row.names=TRUE, stringsAsFactors=FALSE){
  input<- list(...)
  colNameRes<-  unique(unlist(lapply(input, names)))
  template<- as.data.frame(matrix(NA, nrow=1, ncol=length(colNameRes), dimnames=list(NULL, colNameRes)))

  res<- lapply(input, function(x){
    misCols<- setdiff(colNameRes, names(x))
    misCols<- structure(as.list(rep(NA, length(misCols))), names=misCols)
    out<- data.frame(c(x, misCols), stringsAsFactors=stringsAsFactors, check.names=FALSE)
    out<- out[, colNameRes]
  })

  res<- c(res, list(deparse.level=deparse.level, make.row.names=make.row.names, stringsAsFactors=stringsAsFactors))
  res<- do.call(rbind, res)

  return(res)
}


#' Merge a list of \code{data.frame}s by matching column names
#'
#' @param x a list of \code{data.frame}s.
#' @param all if `TRUE`, then extra rows will be added to the output, one for each row in not present in the other
#'  ´data.frame`s that has no matching rows. These rows will have NAs in those columns that are usually filled with
#'   values from other ´data.frame`s.
#' @param ... other parameters to [merge()].
#'
#' @details WARNING: the order of the data.frames in the list could modify the output if all=FALSE
#' @return returns a \code{data.frame} with all the merged columns and rows
#' @export
merge_recursive<- function(x, all=TRUE, ...){
  if (length(x) < 2) return(x[[1]])

  res<- x[[1]]

  for (i in 2:length(x)){
    res<- merge(res, x[[i]], all=all, ...)
  }

  return(res)
}

## set operations with registers in a data.frame ----

#' setdiff for data.frames with rows as a element
#'
#' @param x a \code{data.frame}.
#' @param y a \code{data.frame}.
#'
#' @return a \code{data.frame} with the rows in \code{x} not present in \code{y}.
#' @export
setdiff.data.frame <- function(x, y) {
  x.p<- do.call("paste", x)
  y.p<- do.call("paste", y)
  x[!x.p %in% y.p, ]
}
