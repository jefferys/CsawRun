

#' Parse options and arguments for running csaw
#'
#' @param cli The command line arguments specifying how to run csaw. By
#' default this is the trailing arguments from a script this is run by.
#'
#' @return The parsed cli object
#' @export
parseCli <- function( cli=commandArgs( trailingOnly= TRUE )) {
  utils::str(cli)
}
