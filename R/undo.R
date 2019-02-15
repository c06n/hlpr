## get rid of rownames
unrowname <- function(x) {

  rownames(x) <- NULL
  x
}

## factor to character
unfactor <- function(df){
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
}
