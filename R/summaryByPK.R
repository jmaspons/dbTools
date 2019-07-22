#' Summarize values by Primary Key
#'
#' @param x a \code{data.frame}
#' @param pk columns containing the Primary Key (column names or indexes)
#' @param collapse characters to collapse non numeric values passed to \code{\link[base]{paste}} function
#'
#' @return a \code{data.frame} with a unique value for each PK. Mean for numeric values and a collapsed values for no numeric variables.
#' @export
summarizeByPK<- function(x, pk, collapse="|"){
  if (missing(pk)) stop("Missing pk.")

  dup<- which(duplicated(x[, pk]))

  if (length(dup) == 0){
    return(x) # No dublicates in pk. Nothing to summarize
  }

  pkString<- do.call("paste", x[, pk, drop=FALSE])
  dupPkString<- do.call("paste", x[dup, pk, drop=FALSE])
  # dup<- x[pkString %in% dupPkString, ]
  # dup<- dup[order(do.call("paste", dup[, pk])), ] # order by PK

  selDup<- pkString %in% dupPkString
  xDupL<- split(x[selDup, ], do.call("paste", x[selDup, pk, drop=FALSE]))

  x.summary<- lapply(xDupL, function(y){
    resPK<- y[1, pk, drop=FALSE]
    res<- lapply(y[, setdiff(names(y), names(resPK)), drop=FALSE], function(z){
      if (is.numeric(z)){
        z<- mean(z)
      }else{
        z<- sort(unique(z), na.last=TRUE)
        z<- paste0(z, collapse=collapse)
      }
      z
    })

    data.frame(resPK, res, stringsAsFactors=FALSE)
  })

  x.summary<- do.call(rbind, x.summary)
  out<- rbind(x[!selDup, ], x.summary[, names(x)])

  # Order rows as in x
  outPkString<- do.call("paste", out[, pk, drop=FALSE])
  ord<- match(unique(pkString), outPkString)
  out<- out[ord, ]
# n<- 20
# cbind(out[1:n, pk], x[1:n, pk], pkString[1:n], outPK=outPkString[ord[1:n]])

  return(out)
}

