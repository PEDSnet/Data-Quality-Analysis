###Additional Utilities

quickdf <- function(df) {
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(df[[1]]))
  df
} 