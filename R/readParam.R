
#' Create readParam object from options
#'
#' @param opts The parsed command line options
#'
#' @return A csaw::readParam object.
#'
#' @keywords internal
readParam <- function( opts ) {

    readPairingToPe <- list(
        paired="both",
        single="none",
        first="first",
        second="second"
    )

    readFilterStrandToForward <- list(
        both= NA,
        separate= NULL,
        forward= TRUE,
        reverse= FALSE
    )

    csaw::readParam(
        pe=       readPairingToPe[[ opts[["readPairing"]] ]],
        max.frag= opts[["readKeepMaxFragmentLength"]],
        dedup=    opts[["readDropDuplicate"]],
        minq=     opts[["readKeepMinQuality"]],
        forward=  readFilterStrandToForward[[ opts[["readKeepStrand"]] ]],
        restrict= opts[["readKeepChr"]],
        discard= opts[["readDropRegion"]],
        BPPARAM= opts[["biocParallelParamRds"]]
    )
}