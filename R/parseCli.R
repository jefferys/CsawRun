optionDataList <- list(
    readPairingValues= c('paired', 'single', 'first', 'second'),
    readKeepStrandValues= c('both', 'forward', 'reverse', 'separate')
)

#'  Define the options for the cli
#'
#' @return A list with each element describing an option as an
#'   optparse::OptionParserOption object
#'
#' @keywords internal
optionRules <- function() {
    list(
        optparse::make_option(
            c( "--debug" ), action= "store_true",
            default= FALSE,
            help= "Turn on debug output [%default]. Implies --verbose."
        ),
        optparse::make_option(
            c( "-v", "--verbose" ), action= "store_true",
            default= FALSE,
            help= "Turn on extra output messages [%default]"
        ),
        optparse::make_option(
            c( "-i", "--in" ), action= "store",
            metavar= "BAM_FILE,BAM_FILE,...",
            help= paste0(
                "One or more bam files to read, comma separated without spaces.\n",
                "\t\tOne (and only one) of --in, --inFile, and args must be specified."
            )
        ),
        optparse::make_option(
            c( "--inFile" ), action= "store",
            metavar= "TEXT_FILE",
            help= paste0(
                "Text file listing bam files to read. One bam file per line,\n",
                "\t\tno leading or trailing space, empty lines are ignored.\n",
                "\t\tOne (and only one) of --in, --inFile, and args must be specified."
            )
        ),
        optparse::make_option(
            c( "--readPairing" ), action= "store",
            default= 'paired', metavar = "SELECT",
            help= paste0(
                "One of { ", stringify( optionDataList[["readPairingValues"]] ), " }.\n",
                "\t\t 'paired' - Count reads in pairs (default value).\n",
                    "\t\t\tAfter filtering individually for primacy, quality, duplicates, etc,\n",
                    "\t\t\t'paired' mode only keeps remaining proper oriented\n",
                    "\t\t\tnon-chimeric pairs.\n",
                "\t\t 'single' - Count reads separately, even if paired.\n",
                "\t\t 'first'  - Count only the first read of pairs.\n",
                "\t\t 'second' - Count only the second read of pairs."
            )
        ),
        optparse::make_option(
            # Note: NULL is used to signal no user setting and hence the need
            # to set the (conditional) default value.
            c( "--readKeepMaxFragmentLength" ), action= "store",
            default= NULL, metavar = "INT", type= "character",
            help= paste0(
                "When 'readPairing' is 'paired', drop all fragments longer than this.\n",
                "\t\tFragment length is the read pair lengths plus insert as aligned.\n",
                "\t\tDefault is 500 when 'readPairing' is 'paired', 'NA' if not.\n",
                "\t\tIgnored with a warning if not NA when '--readPairing'\n",
                "\t\tis anything other than 'paired'."
            )
        ),
        optparse::make_option(
            c( "--readKeepStrand" ), action= "store",
            default= "both", metavar= "SELECT",
            help= paste0(
                "One of { ", stringify( optionDataList[["readKeepStrandValues"]] ), " }.\n",
                "\t\tDefault is 'both'. Any other setting is ignored when\n",
                "\t\t'--readPairing paired' is set.\n",
                "\t\t 'both' - Keep reads from both strands (default value).\n",
                "\t\t 'forward' - Count reads only from forward strand.\n",
                "\t\t 'reverse' - Count reads only from reverse strand.",
                "\t\t 'separate' - Count reads strand specifically."
            )
        ),
        optparse::make_option(
            c( "--readDropDuplicate" ), action= "store_true",
            default= FALSE,
            help= paste0(
                "Skip reads flagged as duplicates.\n",
                "\t\tDefault is not to remove duplicates.\n",
                "\t\tThe input bam may already have been filtered to remove duplicates."
            )
        ),
        optparse::make_option(
            c( "--readKeepMinQuality" ), action= "store",
            default= NA_character_, metavar= "INT", type="character",
            help= paste0(
                "Skip reads with map quality less than this.\n",
                "\t\tThe default value NA includes all reads.\n",
                "\t\tThe input bam may already have been filtered for (some) quality."
            )
        ),
        optparse::make_option(
            c( "--readKeepChr" ), action= "store",
            default= NULL, metavar= "chr1,chr2,...",
            help= paste0(
                "Include only the chromosomes listed, default is to keep all of them.\n",
                "\t\tIf specified, chromosome names should be separated by\n",
                "\t\tcommas (no space), and must match the chromosome names used\n",
                "\t\tin the reference the bam files were aligned to.\n",
                "\t\tOnly one of --chr or --chrFile may be specified."
            )
        ),
        optparse::make_option(
            c( "--readKeepChrFile" ), action= "store",
            default= NULL, metavar= "TEXT_FILE",
            help= paste0(
                "Include only the chromosomes listed in the specified file.\n",
                "\t\tOne chromosome per line, no leading or trailing space,\n",
                "\t\tempty lines are ignored. The default is to keep everything.\n",
                "\t\tChromosome must match the chromosome names used\n",
                "\t\tin the reference the bam files were aligned to.\n",
                "\t\tOnly one of --chr or --chrFile may be specified."
            )
        ),
        optparse::make_option(
            c( "--readDropRegion" ), action= "store",
            default= NULL, metavar= "CHR:START:STOP,CHR:START:STOP,...",
            help= paste0(
                "Regions to exclude as one or more comma separate locations, no spaces.\n",
                "\t\tEach location is specified as a chromosme, a start base, and a stop base.\n",
                "\t\t(1 based positions) separated by colons, no spaces.\n",
                "\t\tOnly reads wholly contained within a given region will be skipped."
            )
        ),
        optparse::make_option(
            c( "--readDropRegionBed" ), action= "store",
            default= NULL, metavar= "BED_3_FILE",
            help= paste0(
                "Bed-3 file giving regions not to count, default is to count everything.\n",
                "\t\tOnly reads wholly contained within a given region will be skipped.\n",
                "\t\tNote: Bed files use 0 based start coordinates."
            )
        ),
        optparse::make_option(
            c( "--readDropRegionRds" ), action= "store",
            default= NULL, metavar= "RDS_FILE",
            help= paste0(
                "R object (RDS) file giving a GRanges object that specifies regions\n",
                "\t\tnot to count, default is to count everything.\n",
                "\t\tOnly reads wholly contained within a given region will be skipped."
            )
        ),
        optparse::make_option(
            c( "--biocParallelParamRds" ), action= "store",
            default= NULL, metavar= "RDS_FILE",
            help= paste0(
                "File in R's RDS format giving a BiocParallelParam object.\n",
                "\t\tDefault is to execute serially using a default 'SerialParam()' object."
            )
        )
    )
}

#' Extract the arguments from the parsed cli
#'
#' @return A character vector of the arguments, or NULL if none. Will extract
#' arguments mixed in with options, but does not recognize "-" as a signal
#' to stop identifying options and use rest as arguments.
#'
#' @keywords internal
extractArgs <- function( parsedCli ) {
  args <- NULL

  # If have args, they are listed as element "args"
  # at top level in list from parse_args
  if ( length( parsedCli[["args"]] != 0 )) {
    args <- parsedCli[["args"]]
  }

  args
}

#' Extract the options from the parsed cli
#'
#' @return A list of the option names and their values, or NULL if no options.
#' Will extract options mixed in with arguments, but does not recognize "-" as
#' a signal to stop identifying options and use rest as arguments.
#'
#' @keywords internal
extractOptions <- function( parsedCli ) {
  # remove any args so only options left in list from parse_args
  parsedCli[["args"]] <- NULL

  # options may be listed under element "options", or at base level in list.
  opts <- parsedCli[["options"]]
  if ( is.null( opts )) {
    opts <- parsedCli
  }
  if ( length( opts ) == 0) {
    opts <- NULL
  }

  opts
}

validateIn <- function( opts, args ) {
    opts[["in"]] <- strsplit( opts[["in"]], split= ",", fixed= TRUE)[[1]]
    for (file in opts[["in"]]) {
        ok <- checkmate::testFileExists( file, access='r' )
        if (! ok) {
            stopAs( "cli.in.allMustExist1", file )
        }
    }

    opts
}

validateInFile <- function( opts, args ) {
    ok <- checkmate::testFileExists( opts[["inFile"]], access='r' )
    if (! ok) {
        stopAs( "cli.inFile.mustExist1", opts[["inFile"]] )
    }
    opts[["in"]] <- readLines( opts[["inFile"]], warn= FALSE )
    # Drop blank lines
    opts[["in"]][ opts[[ "in" ]] == "" ] <- NULL
    for (file in opts[["in"]]) {
        ok <- checkmate::testFileExists( file, access='r' )
        if (! ok) {
            stopAs( "cli.inFile.allMustExist1", file )
        }
    }

    opts
}

validateArgs <- function( opts, args ) {

    opts[["in"]] <- args
    for (file in opts[["in"]]) {
        ok <- checkmate::testFileExists( file, access='r' )
        if (! ok) {
            stopAs( "cli.args.allMustExist1", file )
        }
    }

    opts
}

validateDebug <- function( opts, args ) {

    opts[["debug"]] <- checkmate::assertFlag( opts[["debug"]], .var.name="option --debug" )

    opts
}

validateVerbose <- function( opts, args ) {
    opts[["verbose"]] <- checkmate::assertFlag( opts[["verbose"]], .var.name="option --verbose" )
    opts
}

validateReadPairing <- function ( opts, args ) {
    ok <- checkmate::testChoice( opts[["readPairing"]],
                                 choices = optionDataList[["readPairingValues"]] )
    if (! ok) {
        stopAs( "cli.readPairing.badChoice2", opts[["readPairing"]],
                paste( optionDataList[["readPairingValues"]], collapse= ", " ) )
    }

    opts
}

validateReadKeepMaxFragmentLength <- function ( opts, args ) {
    if (is.null( opts[["readKeepMaxFragmentLength"]] )) {
        opts[["readKeepMaxFragmentLength"]] <- 500L
    }
    else {
        tryCatch(
            opts[["readKeepMaxFragmentLength"]] <- checkmate::assertInteger(
                as.integer( naDecode( opts[["readKeepMaxFragmentLength"]] )),
                lower= 0L, len= 1
            ),
            error= function(e) {
                stopAs( "cli.readKeepMaxFragmentLength.badValue1", opts[["readKeepMaxFragmentLength"]] )
            },
            warning= function(e) {
                stopAs( "cli.readKeepMaxFragmentLength.badValue1", opts[["readKeepMaxFragmentLength"]] )
            }
        )
    }

    opts
}

validateReadKeepStrand <- function ( opts, args ) {

    tryCatch(
        opts[["readKeepStrand"]] <- checkmate::assertChoice( opts[["readKeepStrand"]],
            choices = optionDataList[["readKeepStrandValues"]]
        ),
        error= function(e) {
            stopAs( "cli.readKeepStrand.badChoice2", opts[["readKeepStrand"]],
                paste( optionDataList[["readKeepStrandValues"]], collapse= ", " )
            )
        },
        warning= function(e) {
            stopAs( "cli.readKeepStrand.badChoice2", opts[["readKeepStrand"]],
                paste( optionDataList[["readKeepStrandValues"]], collapse= ", " )
            )
        }
    )

    opts
}

validateReadDropDuplicate <- function( opts, args ) {
    # Only use asserts for flags as these are set by the option framework.
    checkmate::assertFlag( opts[["readDropDuplicate"]], .var.name="option --readDropDuplicate" )

    opts
}

validateReadKeepMinQuality <- function( opts, args ) {
    tryCatch(
        opts[["readKeepMinQuality"]] <- checkmate::assertInteger(
            as.integer( naDecode( opts[["readKeepMinQuality"]] )),
            lower= 0L, len= 1
        ),
        error= function(e) {
            stopAs( "cli.readKeepMinQuality.badValue1", opts[["readKeepMinQuality"]] )
        },
        warning= function(e) {
            stopAs( "cli.readKeepMinQuality.badValue1", opts[["readKeepMinQuality"]] )
        }
    )

    opts
}

validateReadKeepChr <- function( opts, args ) {
    stopAs( "todo.param1", "readKeepChr" )
    opts
}

validateReadKeepChrFile <- function( opts, args ) {
    stopAs( "todo.param1", "readKeepChrFile" )

    opts
}

validateReadDropRegion <- function( opts, args ) {
    stopAs( "todo.param1", "readDropRegion" )
    opts
}

validateReadDropRegionBed <- function( opts, args ) {
    stopAs( "todo.param1", "readDropRegionBed" )
    opts
}

validateReadDropRegionRds <- function( opts, args ) {
    stopAs( "todo.param1", "readDropRegionRds" )
    opts
}

validateBiocParallelParamRds <- function( opts, args ) {

    if ( is.null( opts[["biocParallelParamRds"]] )) {
        opts[["biocParallelParamRds"]] <- BiocParallel::SerialParam()
    }
    else {
        stopAs( "todo.param1", "biocParallelParamRds" )
    }

    opts
}

# args, -i/--in, and --inFile
validateGroupInputFiles <- function( opts, args ) {

    # Require that one and only one be specified
    inParamCount <- sum( ! is.null(args), ! is.null(opts[["in"]]), ! is.null(opts[["inFile"]]) )
    if ( inParamCount == 0 ) {
        stopAs( "cli.required1", "One of: '--in', '--inFile', arguments" )
    }
    else if (inParamCount > 1 ) {
        stopAs( "cli.mutuallyExclusive1", "'--in', '--inFile', arguments" )
    }

    # Parse files fron --in, --inFile, or args. Know only one is specified.
    if (! is.null( args )) {
        opts <- validateArgs( opts, args )
    }
    else if (! is.null( opts[["in"]] )) {
        opts <- validateIn( opts, args )

    }
    else if (! is.null( opts[["inFile"]] )) {
        opts <- validateInFile( opts, args )
    }
    else {
        stopAs( "internal.badLogic2", "valiaateCli", "process parameters --in, --inFile and args" )
    }

    # Check that parsing didn't reduce file list to empty.
    if (length(opts[["in"]]) < 1) {
        stopAs( "cli.required1", "'--in', '--inFile', arguments" )
    }

    opts
}

##--debug & -v/--verbose
validateGroupConsoleIO <- function( opts, args ) {

    opts <- validateDebug( opts, args )
    opts <- validateVerbose( opts, args )

    if ( opts[["debug"]] ) {
        opts[["verbose"]] <- TRUE
    }

    opts
}

# --readPairing, --readKeepMaxFragmentLength, and --readFilterStrand
validateGroupPairing <- function( opts, args ) {

    opts <- validateReadPairing( opts, args )
    opts <- validateReadKeepStrand( opts, args )

    if (    opts[["readPairing"]] != "single"
         && opts[["readKeepStrand"]] != 'both'
    ) {
        warnAs( "cli.readKeepStrand.paramConflict4", opts[["readKeepStrand"]],
                "both", "--readPairing", opts[["readPairing"]] )
        opts[["readKeepStrand"]] <- "both"
    }

    if ( ! is.null( opts[["readKeepMaxFragmentLength"]] )
         && opts[["readPairing"]] != "paired"
    ) {
        warnAs( "cli.readKeepMaxFragmentLength.ignored1",
                opts[["readKeepMaxFragmentLength"]] )
    }
    opts <- validateReadKeepMaxFragmentLength( opts, args )

    opts
}

# --readKeepChr, --readKeepChrFile
validateGroupKeepChr <- function( opts, args ) {

    if (    ! is.null( opts[["readKeepChr"]] )
         && ! is.null( opts[["readKeepChrFile"]] )
    ) {
        stopAs( "cli.mutuallyExclusive1", "'--readKeepChr', '--readKeepChrFile'" )
    }
    else if ( ! is.null( opts[["readKeepChr"]] )) {
        opts <- validateReadKeepChr( opts, args )
    }
    else if ( ! is.null( opts[["readKeepChrFile"]] )) {
        opts <- validateReadKeepChrFile( opts, args )
    }

    opts
}

# --readDropRegion, --readDropRegionBed, --readDropRegionRds
validateGroupDropRegion <- function( opts, args ) {

    paramCount <- sum( ! is.null( opts[["readDropRegion"]] ),
                       ! is.null( opts[["readDropRegionBed"]] ),
                       ! is.null( opts[["readDropRegionRds"]] )
    )
    if ( paramCount > 1 ) {
        stopAs( "cli.mutuallyExclusive1", "'--readDropRegion',
                '--readDropRegionBed', --readDropRegionRds'" )
    }
    else if ( paramCount == 0 ) {
        opts[["readDropRegion"]] <- GenomicRanges::GRanges()
    }
    else if ( ! is.null( opts[["readDropRegion"]] )) {
        opts <- validateReadDropRegion( opts, args )
    }
    else if (! is.null( opts[["readDropRegionBed"]] )) {
        opts <- validateReadDropRegionBed( opts, args )
    }
    else if (! is.null( opts[["readDropRegionRds"]] )) {
        opts <- validateReadDropRegionRds( opts, args )
    }
    else {
        stopAs( "internal.badLogic2", "validateGroupDropRegion", paste0(
            "process parameters --readDropRegion, --readDropRegionBed,",
            " and --readDropRegionRds."
        ))
    }
    opts
}

#' Validate the options ande arguments from the parsed cli
#'
#' @param opts Options as a key=value list.
#' @param args Arguments as a character vector, in original order.
#' @return A list of key=value settings from validated options, arguments,
#' and defaults.
#'
#' @keywords internal
validateCli <- function( opts, args ) {

    if ( length(args) > 0 && any( startsWith( args, "-" ))) {
        warnAs( "cli.unknownParam1", paste(
            args[ startsWith( args, "-" ) ], collapse=", ")
        )
    }
    opts <- validateGroupInputFiles( opts, args )
    opts <- validateGroupConsoleIO( opts, args )
    opts <- validateGroupPairing( opts, args )
    opts <- validateReadDropDuplicate( opts, args )
    opts <- validateReadKeepMinQuality( opts, args )
    opts <- validateGroupKeepChr( opts, args )
    opts <- validateGroupDropRegion( opts, args )
    opts <- validateBiocParallelParamRds( opts, args )

    opts
}

#' Parse options (and arguments) for running csaw
#'
#' @param cli The command line arguments specifying how to run csaw. By
#' default this is the trailing arguments from a script this is run by.
#' Any arguments are loaded into the appropriate option value.
#'
#' @return A list of options, by name.
#' @export
parseCli <- function( cli= commandArgs( trailingOnly= TRUE )) {
    parser <- optparse::OptionParser(
        option_list= optionRules(),
        usage= "./csaw.Rscript [options] [FILE FILE ...]",
        epilogue= paste0(
            "\nArguments:\n",
            "\tFILE FILE ...\n",
            "\t\tOne or more files to read, space separated.\n",
            "\t\tOne (and only one) of --in, --inFile, and args must be specified.\n"
        )
    )
    parsedCli <- optparse::parse_args( parser, args= cli, positional_arguments= TRUE )

    options <- extractOptions( parsedCli )
    args <- extractArgs( parsedCli )

    if ( options[["debug"]] ) {
        message( paste0( "DEBUG: Raw command line parameters: ", stringify( cli )))
    }

    opt <- validateCli( options, args )

    if ( options[["debug"]] ) {
        message( paste( "DEBUG: validated parameters: ", stringify( opt )))
    }

    opt
}
