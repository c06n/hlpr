# select variables

vars <- function(pattern, names_df, names = T) {

  idx <- unlist(lapply(pattern, grep, names_df))

  if (names) {names_df[idx]} else {idx}

}
