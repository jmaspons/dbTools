#' Change data type to numeric if no data is lost
#'
#' @param x a variable of type \code{data.frame}, \code{list} or atomic.
#'
#' @return returns the same variable as a numeric type only if there is no data lost. For \code{data.frame} and \code{list},
#'  it change the data type per column/element.
#' @export
as.numeric_noDataLost<- function(x, warning=FALSE) UseMethod("as.numeric_noDataLost")

#' @export
as.numeric_noDataLost.vector<- function(x, warning=FALSE){
  if (is.numeric(x)) return(x)

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
  data.frame(lapply(x, as.numeric_noDataLost.vector, warning=warning), stringsAsFactors=FALSE, check.names=FALSE)
}

#' @export
as.numeric_noDataLost.list<- function(x, warning=FALSE){
  lapply(x, as.numeric_noDataLost.vector, warning=warning)
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
    misCols<- structure(as.list(rep(NA, length(misCols))), names= misCols)
    out<- data.frame(c(x, misCols), stringsAsFactors=stringsAsFactors, check.names=FALSE)
    out<- out[, colNameRes]
  })

  res<- c(res, list(deparse.level=deparse.level, make.row.names=make.row.names, stringsAsFactors=stringsAsFactors))
  res<- do.call(rbind, res)

  return(res)
}


#' Merge a list of \code{data.frame}s by matching column names. No use case yet
#'
#' @param x a list of \code{data.frame}s.
#'
#' @details WARNING: the order of the data.frames in the list could modify the output.
#' @return returns a \code{data.frame} with all the merged columns and rows
#' @export
merge_recursive<- function(x, all=TRUE){
  if (length(input) < 2) return(x[[1]])

  res<- x[[1]]

  for (i in 2:length(x)){
    res<- merge(res, x[[i]], all=TRUE)
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
