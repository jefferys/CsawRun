#' Program welcome banner
#'
#' Prints a welcome banner to STDOUT when called.
#'
#' @return N/A
printBanner <- function() {
  cat( "csawRun v", as.character( utils::packageVersion( "CsawRun" )))
}
