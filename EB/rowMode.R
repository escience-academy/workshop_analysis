rowMode <- function(x, ties = NULL, include.na = FALSE) {
  # input checks data
  if ( !(is.matrix(x) | is.data.frame(x)) ) {
    stop("Your data is not a matrix or a data.frame.")
  }
  # input checks ties method
  if ( !is.null(ties) && !(ties %in% c("random", "first", "last")) ) {
    stop("Your ties method is not one of 'random', 'first' or 'last'.")
  }
  # set ties method to 'random' if not specified
  if ( is.null(ties) ) ties <- "random"
  
  # create row frequency table
  rft <- table(c(row(x)), unlist(x), useNA = c("no","ifany")[1L + include.na])
  
  # get the mode for each row
  colnames(rft)[max.col(rft, ties.method = ties)]
}