#' Program welcome banner
#'
#' Prints a welcome banner to STDOUT when called.
#'
#' @return N/A
#'
#' @keywords internal
printBanner <- function( opts ) {
    cat( "csawRun v", as.character( utils::packageVersion( "CsawRun" )), "\n", sep= "" )
    if ( opts$debug ) {
        cat( "Running in debug mode.\n" )
    }
    else if ( opts$verbose ) {
        cat( "Running in verbose mode.\n" )
    }
}


#' Errors by name.
#'
#' @param name The name of the error. If this ends with a number, that is the
#'  number of extra parameters needed to format the error message.
#' @param ... Extra parameters used to format the error message
#'
#' @return N/A (exits with error)
#' @keywords internal
stopAs <- function( name, ... ) {
    # Error strings by names
    errors <- list(
        cli= "Command line error. Try 'csaw --help'.",
        internal= "Unexpected internal program error in function %s().",
        todo= "Unimplemented feature:"
    )
    errors <- c( errors, list(
        cli.required1= paste0( errors[["cli"]],
                " Missing required parameter: %s." ),
        cli.mutuallyExclusive1= paste0( errors[["cli"]],
                " More than one mutually exclusive parameter specified: %s." ),
        cli.inFile= paste0( errors[["cli"]],
                " '--inFile <FILE>' parameter error." ),
        cli.in= paste0( errors[["cli"]],
                " '-i, --in <FILE,FILE,...>' parameter error." ),
        cli.args= paste0( errors[["cli"]],
                " Arguments error." ),
        cli.readPairing= paste0( errors[["cli"]],
                " '-r, --readParing <STR>' parameter error." ),
        cli.readKeepMaxFragmentLength= paste0( errors[["cli"]],
                " '--readKeepMaxFragmentLength <INT>' parameter error." ),
        cli.readKeepStrand= paste0( errors[["cli"]],
                " '--readKeepStrand <STR>' parameter error." ),
        cli.readKeepMinQuality= paste0( errors[["cli"]],
                " '--readKeepMinQuality <INT>' parameter error." ),
        internal.warning= paste0( errors[["internal"]],
                " Trying to report a warning." ),
        internal.error= paste0( errors[["internal"]],
            " Trying to report an error" ),
        internal.badLogic2= paste0( errors[["internal"]],
            " Trying to report an error",
            " Unexpected logical condition reached.",
            " Was attempting to %s." ),
        todo.param1= paste0( errors[["todo"]],
            " Currently only the default value of the parameter '--%s' is supported." )
    ))
    errors <- c( errors, list(
        cli.inFile.mustExist1= paste0( errors[["cli.infile"]],
                " File not found or can not be read: %s." ),
        cli.inFile.allMustExist1= paste0( errors[["cli.inFile"]],
                " Includes a file that is missing or unreadable: %s." ),
        cli.in.allMustExist1 = paste0( errors[["cli.in"]],
                " Includes a file that is missing or unreadable: %s." ),
        cli.args.allMustExist1 = paste0( errors[["cli.args"]],
                " Includes a file that is missing or unreadable: %s." ),
        cli.readPairing.badChoice2 = paste0( errors[["cli.readPairing"]],
                " Illegal value: '%s'; expected one of: %s." ),
        cli.readKeepMaxFragmentLength.badValue1 = paste0( errors[["cli.readKeepMaxFragmentLength"]],
                " Illegal value: '%s'; expected an integer >= 0 or NA" ),
        cli.readKeepStrand.badChoice2 = paste0( errors[["cli.readKeepStrand"]],
                " Illegal value: '%s'; expected one of: %s." ),
        cli.readKeepMinQuality.badValue1 = paste0( errors[["cli.readKeepMinQuality"]],
                " Illegal value: '%s'; expected an integer > 0 or NA" ),
        internal.warning.unknown2 = paste0( errors[["internal.warning"]],
            " Unknown warning named '%s' with parameters: %s." ),
        internal.error.unknown2 = paste0( errors[["internal.error"]],
            " Unknown error named '%s' with parameters: %s." )
    ))

    if (is.null(errors[[ name ]])) {
        stopAs( "internal.error.unknown2", name, stringify(list(...)))
    }
    errorObj <- structure(
        class= c( name, "error", "condition" ),
        list( message= paste0( sprintf( errors[[ name ]], ... ), "\n"),
              call= NULL )
    )

    stop( errorObj )
}

warnAs <- function( name, ... ) {
    # Error strings by names
    warnings <- list(
        cli= "Command line warning. For details see 'csaw --help'."
    )
    warnings <- c( warnings, list(
        cli.readKeepMaxFragmentLength= paste0( warnings[["cli"]],
            " '--readKeepMaxFragmentLength <INT>' parameter warning." ),
        cli.readKeepStrand= paste0( warnings[["cli"]],
            " '--readKeepStrand <CHOICE>' parameter warning." ),
        cli.unknownParam1= paste0( warnings[["cli"]],
            " One or more arguments look like unknown parameters: %s.")
    ))
    warnings <- c( warnings, list(
        cli.readKeepMaxFragmentLength.paramConflict4= paste0( warnings[["cli.readKeepMaxFragmentLength"]],
            " Setting of '%s' replaced with '%s'.",
            " Changed because '%s' is '%s'." ),
        cli.readKeepMaxFragmentLength.ignored1 = paste0( warnings[["cli.readKeepMaxFragmentLength"]],
            " Ignoring value '%s' as readPairing='single'." ),
        cli.readKeepStrand.paramConflict4= paste0( warnings[["cli.readKeepStrand"]],
            " Setting of '%s' replaced with '%s'.",
            " Changed because '%s' is '%s'." )
    ))

    if (is.null(warnings[[ name ]])) {
        stopAs( "internal.warning.unknown2", name, stringify(list(...)))
    }
    warningObj <- structure(
        class= c( name, "warning", "condition" ),
        list( message= paste0( sprintf( warnings[[ name ]], ... ), "\n"),
              call= NULL )
    )

    warning( warningObj )
}
