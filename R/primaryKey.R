#' Find the minimum fields which make a valid Primary Key
#'
#' @param x a \code{data.frame} equivalent to a table (no duplicated registers, \code{unique(x)}).
#' @param fieldOrder a character vector with the sorted preferences in the fields part of the PK.
#' @param excludeFields columns which will be excluded from the potential primary key. Can be the index or the column
#'   names.
#' @param maxFields maximum number of fields in th primary key.
#' @param minFieldSet columns that will be forced to be included in the primary key.
#'
#' @return
#' @export
#'
#' @examples
minimumPK<- function(x, fieldOrder=character(), excludeFields=character(), maxFields=ncol(x), minFieldSet=character()){
  cols<- names(x)[!names(x) %in% c(excludeFields, minFieldSet)]

  ord<- match(cols, fieldOrder)
  cols<- c(fieldOrder[stats::na.omit(ord)], cols[is.na(ord)])

  pk<- sapply(x[cols,], function(y) !any(duplicated(y))) # check single columns
  pkCandidate<- names(pk)[pk]

  if (length(pkCandidate) > 0){
    return(pkCandidate)
  }

  if (length(minFieldSet) > 0){
    i0<- 0 # no column if there is no minFieldSet -> no duplicates
  } else{ # single columns already checked
    i0<- 2
  }

  for (i in i0:min(c(maxFields, length(cols)))){
    comb<- utils::combn(cols, i, simplify=FALSE)

    message("Try whith ", i + length(minFieldSet), " fields. ", length(comb), " combinations.", appendLF=FALSE)

    pk<- sapply(comb, function(y){
      length(which(duplicated(x[, c(minFieldSet, y)])))
    })

    if (any(sel<- pk == 0)){
      message()
      res<- lapply(comb[sel], function(y) c(minFieldSet, y))

      return(res)
    }else{
      sel<- which.min(pk)

      message(" Minimum number of duplicates=", pk[sel], "\tPK: ", paste(c(minFieldSet, comb[[sel]]), collapse=", "))
    }
  }

  return(NA)
}


#' Find registers with a duplicated Primary Key
#'
#' @param x a \code{data.frame}
#' @param pk columns containing the Primary Key (column names or indexes)
#'
#' @return a \code{data.frame} with the duplicated registers or \code{NA} if there are no duplicated PK values after \code{unique(x)}.
#' @export
duplicatedPK<- function(x, pk){
  if (missing(pk)) pk<- colnames(x)

  x<- unique(x)

  dup<- which(duplicated(x[, pk]))

  if (length(dup) > 0){
    if (length(pk) > 1){
      pkString<- do.call("paste", x[, pk])
      dupPkString<- do.call("paste", x[dup, pk])
      # pkString<- gsub("\\s+", "", pkString)
      # dupPkString<- gsub("\\s+", "", dupPkString)
      dup<- x[pkString %in% dupPkString, ]
      dup<- dup[order(do.call("paste", dup[, pk])), ] # order by PK
    }else{
      pkString<- x[, pk]
      dupPkString<- x[dup, pk]
      dup<- x[pkString %in% dupPkString, ]
      dup<- dup[order(dup[, pk]), ] # order by PK
    }
  }else{ dup<- NA }

  return(dup)
}


#' Omit \code{NA}s in values for duplicated registers by PK and return a \code{data.frame} with a unique register for each PK if possible.
#'
#' @param x a \code{data.frame}
#' @param pk columns with the primary key
#' @param collapse a \code{character}. If no missing, duplicated values are collapsed in a string separated by \code{collapse} character.
#'
#' @return Return \code{x} removing duplicated rows by PK omitting \code{NA}s. If \code{collapse} is missing, duplicated rows by PK and different values
#' are kept.
#' @export
#'
#' @examples
na.omitValByDupPK<- function(x, pk, collapse){
  dup<- duplicatedPK(x, pk=pk)

  if (!inherits(dup, "data.frame") && is.na(dup)) return(x)

  nonDup<- unique(setdiff.data.frame(x, dup))

  if (length(pk) > 1){
    pkString<- do.call("paste", dup[, pk])
  }else{
    pkString<- dup[, pk]
  }

  outL<- by(dup, pkString, function(y, collapse=collapse){
    y<- unique(y)
    if (nrow(y) == 1) return(y)

    outL<- lapply(y, function(z){
      z<- unique(z)

      if (length(z) == 1)
        return(z)

      return(stats::na.omit(z))
    })

    len<- sapply(outL, length)
    selDup<- which(len > 1)

    if (length(selDup) > 0 & !missing(collapse)){
      outL[selDup]<- lapply(outL[selDup], function(z) paste(z, collapse=collapse))
    }

    outL
  })

  selDup<- sapply(outL, function(y) any(sapply(y, length) > 1))

  outDF<- lapply(outL[!selDup], function(y) data.frame(y, stringsAsFactors=FALSE, check.names=FALSE))
  outDF<- do.call("rbind", outDF)

  if (any(selDup)){
    warning(sum(selDup), "sets of PK values with more than 1 row: ", paste(names(selDup)[selDup], collapse=", "))

    dupPKstring<- names(which(selDup))
    outDF<- rbind(dup[pkString %in% dupPKstring, ], outDF)
  }

  return (rbind(outDF, nonDup))
}


#' Lump duplicated registers by PK
#'
#' For duplicated rows, omit \code{NA}s and take unique values in columns when at least one value exists to reduce the
#' duplicates.
#'
#' @param x a \code{data.frame}.
#' @param pk columns with the primary key.
#' @param collapse a \code{character}. If no missing, duplicated values are collapsed in a string separated by \code{collapse} character.
#' @param tryNumeric if \code{TRUE}, convert values to numeric if no data is lost and take the mean if values are similar.
#' @param tolerance relative tolerance passed to \code{all.equal} when comparing values if \code{tryNumeric} is \code{TRUE}.
#'
#' @return a named `list` with a `df` item for the x rows without duplicates and a `dup` item with duplicated rows.
#' @export
#'
#' @examples
lumpDuplicatedByPK<- function(x, pk, collapse, tryNumeric=FALSE, tolerance=sqrt(.Machine$double.eps)){
  if (missing(pk)) pk<- colnames(x)
  if (missing(collapse)) collapse<- NULL

  if (length(pk) > 1){
    pkString<- do.call("paste", x[, pk])
  }else{
    pkString<- x[, pk]
  }

  outL<- by(x, pkString, function(y){
    y<- unique(y)
    if (nrow(y) == 1)
      return(y)

    columnL<- lapply(y, function(z){ # Loop columns
      z<- unique(z)

      if (length(z) == 1)
        return(z)

      if (length(stats::na.omit(z)) == 1)
        return(stats::na.omit(z))

      if (tryNumeric){ ## TODO: allow to select which columns (tryNumeric<- vector)
        vals<- as.numeric_noDataLost(z)
        equal<- FALSE
        if (is.numeric(vals)){
          if (length(vals) == 2){
            tolerance<- mean(vals) * tolerance
            equal<- isTRUE(do.call(all.equal, c(as.list(vals), list(tolerance=tolerance))))
          }
        }
        if (equal){
          vals<- mean(vals)
          if (is.character(z)){
            z<- as.character(vals)
          } else if (is.factor(z)){
            z<- as.factor(vals)
          } else if (is.logical(z)){
            z<- as.logical(vals)
          } else if (is.numeric(z)){
            z<- vals
          }
        }
        if (length(z) == 1)
          return(z)
      }

      if (!is.null(collapse)){
        z<- paste(z, collapse=collapse)
      }

      return(z)
    })

    columnL
  })

  selDup<- sapply(outL, function(y) any(sapply(y, length) > 1))

  if (any(selDup)){
    warning("Non unique register for pk value: ", paste(names(selDup)[selDup], collapse=", "))
  }

  outDF<- lapply(outL[!selDup], function(y) data.frame(y, stringsAsFactors=FALSE, check.names=FALSE))
  outDF<- do.call("rbind", outDF)

  outDF_dup<- lapply(outL[selDup], function(y) data.frame(y, stringsAsFactors=FALSE, check.names=FALSE))
  outDF_dup<- do.call("rbind", outDF_dup)

  return (list(df=outDF, dup=outDF_dup))
}


#' Find values with differences for an expected unique PK value
#'
#' @param x a \code{data.frame}.
#' @param pk columns with the primary key.
#' @param includeNA treat NA as a value or omit them
#' @param aggregateFields if \code{TRUE}, aggregate by fields with non-unique values instead of by PK values.
#'
#' @return
#' @export
#'
#' @examples
nonUniqueValuesByPK<- function(x, pk, includeNA=FALSE, aggregateFields=FALSE){
  na.last<- ifelse(includeNA, TRUE, NA)

  nonPKfields<- setdiff(names(x), pk)
  dup<- duplicatedPK(x, pk=pk)

  # dup<- dup[, c(pkCandidate, setdiff(names(dup), pkCandidate))] # sort columns with pk first
  # dupPKunique<- unique(dup[, pkCandidate])

  if (length(pk) > 1){
    pkString<- do.call("paste", dup[, pk])
  } else {
    pkString<- dup[, pk]
  }
  # nonPKfieldsTmp<- intersect(names(dup), nonPKfields)

  out<- by(dup, pkString, function(x){
    lapply(x[, nonPKfields], function(y){
      res<- sort(unique(y), na.last=na.last) # na.last=NA/TRUE to omit or not NA
      if (length(res) == 1) res<- NULL
      res
    })
  })

  out<- lapply(out, function(x){
    x[!sapply(x, function(y) is.null(y) | length(y) == 0)]
  })
  out<- out[sapply(out, length) > 0]

  message("Number of PK values with different values in the fields that should be unique:")
  nonUniqueFields<- lapply(out, names)
  print(diffFields<- sort(table(unlist(nonUniqueFields))))

  if (aggregateFields){
    out<- lapply(names(diffFields), function(y){
      res<- lapply(out, function(z){
        if (y %in% names(z)){
          res<- z[[y]]
        }else{
          res<- NULL
        }
        return(res)
      })
      res[!sapply(res, is.null)]
    })
    names(out)<- names(diffFields)
  }

  return(out)
}
