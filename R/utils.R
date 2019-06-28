#' Convert string "NA" to NA_character_
#'
#' Convert the "NA" elements in a character vector into missing (NA) values.
#'
#' @param x A character vector with string elements that should be NA.
#' @param naSymbol The string to convert to NA when found as an element.
#'
#' @return A character vector with missing values (NA) in place of strings
#'   encoding them, e.g. "NA"
#'
#' @examples
#' naDecode( c("N", "NA", "NAN") )
#' #> [1] "N"   NA  "NAN"
#' naDecode( c("A", "B", ".", "D"), naSymbol= "." )
#' #> [1] A  B  NA  D
#'
#' @export
naDecode <- function ( x, naSymbol = "NA" ) {
    x[ x == naSymbol ] <- NA_character_
    x
}

#' Convert an R object to a string
#'
#' Generic to converts the specified R object to a string for printing,
#' preserving names. If a name is not a valid R name, it will be quoted with
#' backticks like: `a name`. Double and integer values are processed by
#' format(...) before reporting, parameters for format can be specified and
#' will be passed through. Other values are reported as is, so empty strings
#' and strings containing separators may lead to confusing output; choose your
#' separators to avoid problems. Lists are processed recursively, with each
#' level delimited by { and } by default. Within a list, elements that are
#' vectors are delimited by ( and ) by default, unless they are just a single,
#' unnamed value.
#'
#' @param x The object to stringify.
#' @param nameSep If an element has a name, this separates the name and value.
#'   By default uses "= ", e.g. elements display as key= value, or just value,
#'   if have no name
#' @param elemSep Separator between elements. by default this is ",". For
#'   a less compact (one line per element) string representation, use \\n or
#'   something similar.
#' @param dropNames By default, names are reported. If this is set TRUE, no names
#'   will be used (and any name separator will be ignored).
#' @param prettyNum Set FALSE to prevent numeric vectors from being
#'   processed by prettyNum() when converting to strings. By default this is
#'   TRUE and parameters for prettyNum specified as ... are passed through.
#' @param empty String to use in place of an empty character value.
#'   Default is nothing. Value delimiters will be applied to this replaced
#'   value, if any are specified.
#' @param naSymbol Character string to use in place of missing values (NA). By
#'   default will be: NA
#' @param nullSymbol Character string to use in place of NULL values. By
#'   default will be: "NULL"
#' @param valueDelim Delimiter to use for values. By default nothing is used.
#'   If this is a single character value, it will be used at the start and end
#'   of the value. If this is a vector of length 2, the 1st character will be
#'   used at the start of every value and the 2nd at the end.
#' @param nameDelim Delimiter to use for names. By default nothing is used for
#'   valid R names, backticks (`) are used for invalid r names, e.g. those
#'   with spaced. If nameDelim is specified, all names will have the specified
#'   delimiter(s), regardless of valdity. If this is a single character value,
#'   it will be used at the start and end of the name. If this is a vector of
#'   length 2, the 1st character will be used at the start of every name and
#'   the 2nd at the end.
#' @param logicalSymbols Character vector of length 2 giving symbols to use
#'   for TRUE and for FALSE (in that order). Default is c( "TRUE", "FALSE" ).
#' @param listSymbols Character vector of length 2 giving symbols to use
#'   for opening and closing a list. Default is c( "{", "}" ). Needed to
#'   identify nesting with lists of one list.
#' @param vectorSymbols Character vector of length 2 giving symbols to use
#'   for opening and closing a vector with named elements or of length greater
#'   than 1. Default is c( "(", ")" ). Cleaner notation if have named list
#'   elements that are vectors with named elements. Not used for simple
#'   vectors, only when a vector is an element, e.g, in a list.
#' @param ... Additional options to be passed on to prettyNum(), or to
#'   specific methods.
#'
#' @return A single element character vector (string)
#' @export
#'
stringify <- function (x, nullSymbol= "NULL", ...) {
    if (is.null(x)) {
        nullSymbol
    }
    else {
        UseMethod("stringify", x)
    }
}

#' @rdname stringify
#' @export
#' @examples
#' x <- c( A="a", "B", empty="", "d")
#' stringify(x)
#' #> "A= a, B, empty= , d"
#' stringify(x, valueDelim="'")
#' #> "A= 'a', 'B', empty= '', 'd'"
#' stringify(x, empty="<>")
#' #> "A= a, B, empty= <>, d"
#' stringify(x, empty="<>", valueDelim=c("'"))
#' #> "A= 'a', 'B', empty= '<>', 'd'"
#'
stringify.character <- function( x,
    nameSep= "= ", elemSep= ", ", dropNames= FALSE, valueDelim= NULL,
    nameDelim= NULL, naSymbol= "NA", empty= "", ...
) {
    keys <- names(x)
    if ( empty != "") {
        x[ x == "" ] <- empty
    }
    if ( naSymbol != "NA" ) {
        x[ is.na(x) ] <- naSymbol
    }
    if ( length( valueDelim ) > 0 ) {
        if ( length(valueDelim) == 1 ) {
            valueDelim <- c( valueDelim, valueDelim )
        }
        else if ( length(valueDelim) > 2 ) {
            warning( "stringify() parameter 'valueDelim' has length > 2. Ignoring extra values.",
                     call.= FALSE )
        }
        valueDelim <- as.character( valueDelim[ 1:2 ] )
        x <- paste0( valueDelim[1], x, valueDelim[2] )
    }

    # Easy if has no names or want none
    if ( dropNames || is.null( keys )) {
        paste( x, collapse= elemSep )
    }
    else {

        # Have to handle any values without names (know at least one has a name)
        hasNamesSelect <- keys != ""

        if ( length( nameDelim ) > 0 ) {
            if ( length(nameDelim) == 1 ) {
                nameDelim <- c( nameDelim, nameDelim )
            }
            else if ( length(nameDelim) > 2 ) {
                warning( "stringify() parameter 'nameDelim' has length > 2. Ignoring extra values.",
                         call.= FALSE )
            }
            nameDelim <- as.character( nameDelim[ 1:2 ] )
            keys[hasNamesSelect] <- paste0( nameDelim[1], keys[hasNamesSelect], nameDelim[2] )
        }
        else {
            # Have to handle names that are not-valid R names (excludes no names)
            invalidNameSelect <- keys != make.names(keys) & hasNamesSelect

            # wrap invalid names with backticks: `...`
            keys[invalidNameSelect] <- paste0( "`", keys[invalidNameSelect], "`" )
        }

        # create elements <key><nameSep><value>, or <value> if no key.
        # <key> will be <valid_name>, or `<invalid name>`
        keys[hasNamesSelect] <- paste0( keys[hasNamesSelect], nameSep )

        elements <- paste0( keys, x )
        paste( elements, collapse= elemSep )
    }
}

#' @rdname stringify
#' @export
#' @examples
#' x <- c( A=TRUE, FALSE, NA, B=TRUE )
#' stringify( x )
#' #> "A= TRUE, FALSE, NA, B= TRUE"
#' stringify( x, logicalSymbols= c( "T", "F" ), naSymbol= "." )
#' #> "A= T, F, ., B= T"
#' stringify( x, logicalSymbols= c( "1", "0" ), naSymbol= "*", names=FALSE, elemSep="" )
#' #> "10*1"
#'
stringify.logical <- function( x, logicalSymbols=c( "TRUE", "FALSE" ), ... ) {
    keys <- names(x)
    x <- as.character( x )
    if ( length(logicalSymbols) < 2 ) {
        stop( "stringify() parameter 'logical.symbol' must be a vector of length 2.", call.= FALSE )
    }
    if ( length(logicalSymbols) > 2 ) {
        warning( "stringify() parameter 'logical.symbol' has length > 2. Ignoring extra values.",
                 call.= FALSE )
        logicalSymbols <- logicalSymbols[ 1:2 ]
    }
    if ( ! all( logicalSymbols == c( "TRUE", "FALSE" ))) {
        x[ x == "TRUE" ] <- logicalSymbols[1]
        x[ x == "FALSE" ] <- logicalSymbols[2]
    }
    names(x) <- keys
    stringify.character( x, ... )
}

#' @rdname stringify
#' @export
stringify.integer <- function( x, prettyNum= TRUE, ... ) {
    keys <- names(x)
    if (prettyNum) {
        isMissing <- is.na(x)
        x <- prettyNum( x, ... )
        x[ isMissing ] <- NA
    }
    else {
        x <- as.character(x)
    }
    names(x) <- keys
    stringify.character( x, ... )
}

#' @rdname stringify
#' @export
stringify.double <- function( x, prettyNum= TRUE, ... ) {
    keys <- names(x)
    isMissing <- is.na(x) & ! is.nan(x)
    if (prettyNum) {
        x <- prettyNum( x, ... )
    }
    else {
        x <- as.character(x)
    }
    x[ isMissing ] <- NA
    names(x) <- keys
    stringify.character( x, ... )
}

#' @rdname stringify
#' @export
stringify.complex <- function( x, prettyNum= TRUE, ... ) {
    keys <- names(x)
    real <- Re(x)
    img <- Im(x)
    isMissingRe <- is.na(real) & ! is.nan(real)
    isMissingImg <- is.na(img) & ! is.nan(img)
    isign <- rep.int( "+", length(x) )
    isign[ sign(img) == -1 ] <- "-"
    img <- abs(img)
    if (prettyNum) {
        real <- prettyNum( real, ... )
        img <- prettyNum( img, ... )
    }
    else {
        real <- as.character(real)
        img <- as.character(img)
    }


    x <- paste0( real, isign, img, "i" )
    x[ isMissingRe | isMissingImg ] <- NA
    names(x) <- keys
    stringify.character( x, ... )
}

#' @rdname stringify
#' @export
stringify.raw <- function( x, ... ) {
    keys <- names(x)
    x <- as.character(x)
    names(x) <- keys
    stringify.character( x, ... )
}

#' @rdname stringify
#' @export
stringify.list <- function( x,
        dropNames= FALSE, valueDelim= NULL, nullSymbol= "NULL",
        listSymbols= c( "{", "}" ), vectorSymbols=c( "(", ")" ), ...
) {
    if (is.null(x) || length(x) == 0) {
        return( paste0( listSymbols[1], nullSymbol, listSymbols[2] ))
    }
    if ( length(listSymbols) < 2 ) {
        stop( "stringify() parameter 'listSymbols' must be a vector of length 2.", call.= FALSE )
    }
    if ( length(listSymbols) > 2 ) {
        warning( "stringify() parameter 'listSymbols' has length > 2. Ignoring extra values.",
                 call.= FALSE )
        listSymbols <- listSymbols[ 1:2 ]
    }
    if ( length(vectorSymbols) < 2 ) {
        stop( "stringify() parameter 'vectorSymbols' must be a vector of length 2.", call.= FALSE )
    }
    if ( length(vectorSymbols) > 2 ) {
        warning( "stringify() parameter 'vectorSymbols' has length > 2. Ignoring extra values.",
                 call.= FALSE )
        vectorSymbols <- vectorSymbols[ 1:2 ]
    }

    elements <- sapply( x,
            FUN= function( x, ... ) {
                got <- stringify( x, dropNames=dropNames, valueDelim=valueDelim,
                                  listSymbols=listSymbols, vectorSymbols=vectorSymbols,
                                  nullSymbol= nullSymbol, ... )
                if ( ! is.list(x) && ( length(x) > 1 || ( length( names(x) > 1 ) && dropNames == FALSE ))) {
                    got <-  paste0( vectorSymbols[1], got, vectorSymbols[2] )
                }
                got
            }, ...
    )

    paste0( listSymbols[1],
            stringify.character( elements, dropNames=dropNames, ... ),
            listSymbols[2]
    )
}
