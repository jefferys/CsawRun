
#' Run csaw with command line parameters
#'
#' This is the main function that runs csaw. Intended to be called from an
#' executable script and to recieve the command line parameters that script
#' was called with. For the meaning of arguments, see parseCli().
#'
#' @param cli The command line arguments specifying how to run csaw. By
#' default this is the trailing arguments from the script that calls this.
#'
#' @return N/A
#' @export
csawRun <- function( cli=commandArgs( trailingOnly= TRUE )) {
  parseCli( cli )
  printBanner()
}
