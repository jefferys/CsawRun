context( "Cli option and argument parsing")

describe( "parseCli()", {

    files <- tempfile( c("testFile1_", "testFile2_", "testFile3_", "testFile4_" ), fileext=".bam" )
    inFile1 <- tempfile( "fileWithFile_", fileext=".txt")
    inFile4 <- tempfile( "fileWithFiles_", fileext=".txt")
    writeLines( files[1], inFile1 )
    writeLines( files, inFile4 )
    file.create( files )
    noSuchFile <- tempfile( "noSuchFile_", fileext=".txt" )
    inFileBad <- tempfile( "fileWithFile_", fileext=".txt")
    inFileMultiBad <- tempfile( "fileWithFiles_", fileext=".txt")
    writeLines( noSuchFile, inFileBad )
    writeLines( c(files, noSuchFile), inFileMultiBad )

    describe( "test file setup for testing parseCli()", {
        it( "Creates files that need to exist for testing", {
            for (file in files) {
                expect_true( file.exists( file ))
            }
            expect_true( file.exists( inFile1 ))
            expect_true( file.exists( inFile4 ))
        })
        it( "Does not create files that need to not exist for testing", {
            expect_false( file.exists( noSuchFile ))
        })
        describe( "File of files contains filenames", {
            contents <- readLines( inFile1 )
            expect_equal( length( contents ), 1 )
            expect_true( file.exists( contents ))

            contents <- readLines( inFile4 )
            expect_equal( length( contents ), 4 )
            for (file in contents) {
                expect_true( file.exists( file ))
            }

            contents <- readLines( inFileBad )
            expect_equal( length( contents ), 1 )
            expect_false( file.exists( contents ))

            contents <- readLines( inFileMultiBad )
            expect_equal( length( contents ), 5 )
            for (file in contents[1:4]) {
                expect_true( file.exists( file ))
            }
            expect_false( file.exists( contents[5] ))
        })
    })

    describe( "Script console output options", {
        describe( "--debug and --verbose", {
            describe( "--verbose", {
                it( "Is false by default", {

                })
                it( "It can be set true.", {

                })
            })
            describe( "--debug", {
                it( "Is false by default", {

                })
                it( "It can be set true.", {

                })
            })
            it ("--debug implies --verbose", {

            })
        })
    })
    describe( "Parameters for readParam()", {
        describe( "--in, --inFile, and arguments; Used to set input file list", {
            it( "Throws error without required input file parameter", {
                wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                                  " Missing required parameter:",
                                  " One of: '--in', '--inFile', arguments." )
                expect_error( parseCli(), wantRE )
            })
            it( "Throws error if specify input files in more than one way", {
                wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                                  " More than one mutually exclusive parameter specified:",
                                  " '--in', '--inFile', arguments." )
                expect_error( parseCli( c( "--in", "file", "--inFile", "file2" )), wantRE )
                expect_error( parseCli( c( "-i", "file", "--inFile", "file2" )), wantRE )
                expect_error( parseCli( c( "--in", "file", "bob1", "bob2" )), wantRE )
                expect_error( parseCli( c( "-i", "file", "bob1", "bob2" )), wantRE )
                expect_error( parseCli( c( "--inFile", "file", "bob1", "bob2" )), wantRE )
            })
            describe( "Parses input with '--in' into correct list of files", {
                it( "Parses --in option with one file", {
                    opts <- parseCli( c( "--in", files[1] ))
                    expect_equal( opts[["in"]], files[1])
                })
                it( "Parses --in option with multiple file string", {
                    opts <- parseCli( c( "--in", paste( c( files[1:3] ), collapse= "," )))
                    expect_equal( opts[["in"]], c( files[1:3] ))
                })
                it( "Throws error if any --in file does not exist", {
                    wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                            " '-i, --in <FILE,FILE,...>' parameter error." ,
                            " Includes a file that is missing or unreadable: ",
                            noSuchFile, "." )
                    expect_error( parseCli( c("--in", noSuchFile )), wantRE )
                    fileInput <- paste( paste( files, collapse="," ), noSuchFile, sep= "," )
                    expect_error( parseCli( c("--in", fileInput )), wantRE )
                })
            })
            describe( "Parses input with '-i' into correct list of files", {
                it( "Parses -i option with one file", {
                    opts <- parseCli( c( "-i", files[1] ))
                    expect_equal( opts[["in"]], files[1])
                })
                it( "Parses -i option with multiple file string", {
                    opts <- parseCli( c( "-i", paste( c( files[1:3] ), collapse= "," )))
                    expect_equal( opts[["in"]], c( files[1:3] ))
                })
                it( "Throws error if any --i file does not exist", {
                    wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                                      " '-i, --in <FILE,FILE,...>' parameter error." ,
                                      " Includes a file that is missing or unreadable: ",
                                      noSuchFile, "." )
                    expect_error( parseCli( c("-i", noSuchFile )), wantRE )
                    fileInput <- paste( paste( files, collapse="," ), noSuchFile, sep= "," )
                    expect_error( parseCli( c("-i", fileInput )), wantRE )
                })
            })
            describe( "Parses input from arguments into correct list of files", {
                it( "Parses argument of one file", {
                    opts <- parseCli( files[1] )
                    expect_equal( opts[["in"]], files[1])
                })
                it( "Parses arguments of multiple file", {
                    opts <- parseCli( files[1:3] )
                    expect_equal( opts[["in"]], files[1:3] )
                })
                it( "Throws error if any arg file does not exist", {
                    wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                                      " Arguments error." ,
                                      " Includes a file that is missing or unreadable: ",
                                      noSuchFile, "." )
                    expect_error( parseCli( c( noSuchFile )), wantRE )
                    expect_error( parseCli( c( files, noSuchFile )), wantRE )
                })
            })
            describe( "Parses input from file of files", {
                it( "Parses file of files with one file", {
                    opts <- parseCli( c("--inFile", inFile1) )
                    expect_equal( opts[["in"]], files[1])
                })
                it( "Parses file of files with multiple files", {
                    opts <- parseCli( c("--inFile", inFile4) )
                    expect_equal( opts[["in"]], files )
                })
                it( "Throws error if any inFile file does not exist", {
                    wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                            " '--inFile <FILE>' parameter error.",
                            " Includes a file that is missing or unreadable: ",
                            noSuchFile, "." )
                    expect_error( parseCli( c( "--inFile", inFileBad )), wantRE )
                    expect_error( parseCli( c("--inFile", inFileMultiBad )), wantRE )
                })
            })
        })
        describe( "--readPairing, --readKeepMaxFragmentLength, and --readKeepStrand", {
            describe( "--readPairing", {
                it ("Is an error if not in listing", {
                    wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                            " '-r, --readParing <STR>' parameter error.",
                            " Illegal value: 'Bob'; expected one of: ",
                            paste( optionDataList[["readPairingValues"]], collapse= ", " ),
                            "." )
                    expect_error( parseCli( c("--readPairing", "Bob", files) ), wantRE )

                })
                it( "Parses --readPairing to default if not specified", {
                    opts <- parseCli( c( "-i", files[1] ))
                    expect_equal( opts[["readPairing"]], "paired")
                })
                it( "Parses --readPairing to value if set", {
                    opts <- parseCli( c( "--readPairing", "single", files[1] ))
                    expect_equal( opts[["readPairing"]], "single" )
                })
            })
            describe( "--readKeepMaxFragmentLength (when paired)", {
                it( "Parses --readKeepMaxFragmentLength to default if not specified", {
                    opts <- parseCli( c( "-i", files[1] ))
                    expect_equal( opts[["readKeepMaxFragmentLength"]], 500 )
                })
                it( "Parses --readKeepMaxFragmentLength to value if set", {
                    opts <- parseCli( c( "--readKeepMaxFragmentLength", "1000", files[1] ))
                    expect_equal( opts[["readKeepMaxFragmentLength"]], 1000 )
                })
                it( "Reports error if set to wrong value", {
                    cli <- c( "--readKeepMaxFragmentLength=-1", files[1] )
                    wantErrorRe <- paste0(
                        "Command line error. Try 'csaw --help'.",
                        " '--readKeepMaxFragmentLength <INT>' parameter error.",
                        " Illegal value: '-1'; expected an integer >= 0 or NA."
                    )
                    expect_error( parseCli( cli ), wantErrorRe )
                })
            })
            describe( "--readKeepStrand", {
                it ("Is an error if not in listing", {
                    wantRE <- paste0( "Command line error. Try 'csaw --help'.",
                                      " '--readKeepStrand <STR>' parameter error.",
                                      " Illegal value: 'Bob'; expected one of: ",
                                      paste( optionDataList[["readKeepStrandValues"]], collapse= ", " ),
                                      "." )
                    cli <- c( "--readPairing", "single", "--readKeepStrand", "Bob", files )
                    expect_error( parseCli( cli ), wantRE )
                })
                it( "Parses --readKeepStrand to default if not specified", {
                    opts <- parseCli( c( "-i", files[1] ))
                    expect_equal( opts[["readKeepStrand"]], "both")
                    opts <- parseCli( c( "-i", files[1], "--readPairing", "single" ))
                    expect_equal( opts[["readKeepStrand"]], "both")
                })
                it( "Parses --readKeepStrand to value if set", {
                    cli <- c( "--readPairing", "single", "--readKeepStrand", "forward", files[1] )
                    opts <- parseCli( cli )
                    expect_equal( opts[["readKeepStrand"]], "forward" )
                })
            })
            describe( "Mutually incompatible settings", {
                it ("Warns if --readPairing not 'paired' and incompatable --readKeepStrand", {
                    wantRE <- paste0(
                        "Command line warning. For details see 'csaw --help'.",
                        " '--readKeepStrand <CHOICE>' parameter warning.",
                        " Setting of 'forward' replaced with 'both'.",
                        " Changed because '--readPairing' is 'paired'."
                    )
                    cli <- c( "--readPairing", "paired", "--readKeepStrand", "forward", files )
                    expect_warning( parseCli( cli ), wantRE )
                })
                it( "Parses --readKeepMaxFragmentLength to default 500 if not --readPairing 'paired'", {
                    opts <- parseCli( c( "-i", files[1], "--readPairing", "single" ))
                    expect_equal( opts[["readKeepMaxFragmentLength"]], 500)
                })
                it ("Warns if --readPairing not 'paired' and --readKeepMaxFragmentLength set", {
                    wantRE <- paste0(
                        "Command line warning. For details see 'csaw --help'.",
                        " '--readKeepMaxFragmentLength <INT>' parameter warning.",
                        " Ignoring value '100' as readPairing='single'."
                    )
                    cli <- c( "--readPairing", "single", "--readKeepMaxFragmentLength", "100", files )
                    expect_warning( parseCli( cli ), wantRE )
                })
            })
        })
        describe( "--readDropDuplicate", {
            it ("Is false by default", {
                cli <- c( files )
                expect_false( parseCli( cli )[["readDropDuplicate"]] )
            })
            it ("Can be set true", {
                cli <- c( "--readDropDuplicate", files )
                expect_true( parseCli( cli )[["readDropDuplicate"]] )
            })
        })
        describe( "--readKeepMinQuality (when paired)", {
            it( "Parses --readKeepMinQuality to default if not specified", {
                opts <- parseCli( c( "-i", files[1] ))
                expect_true( is.na( opts[["readKeepMinQuality"]] ))
            })
            it( "Parses --readKeepMinQuality to value if set", {
                opts <- parseCli( c( "--readKeepMinQuality", "10", files[1] ))
                expect_equal( opts[["readKeepMinQuality"]], 10 )
            })
            it( "Reports error if set to wrong value", {
                cli <- c( "--readKeepMinQuality", "-1", files[1] )
                wantErrorRe <- paste0(
                    "Command line error. Try 'csaw --help'.",
                    " '--readKeepMinQuality <INT>' parameter error.",
                    " Illegal value: '-1'; expected an integer > 0 or NA"
                )
                it( "Reports error if set to wrong value", {
                    cli <- c( "--readKeepMinQuality", "BOB", files[1] )
                    wantErrorRe <- paste0(
                        "Command line error. Try 'csaw --help'.",
                        " '--readKeepMinQuality <INT>' parameter error.",
                        " Illegal value: 'BOB'; expected an integer > 0 or NA"
                    )
                    expect_error( parseCli( cli ), wantErrorRe )
                })
            })
        })

        # optparse::make_option(
        #     c( "--chr" ), action= "store",
        #     default= NULL, metavar= "chr1,chr2,...",
        #     help= paste0(
        #         "Include only the chromosomes listed, default is to keep all of them.\n",
        #         "\t\tIf specified, chromosome names should be separated by\n",
        #         "\t\tcommas (no space), and must match the chromosome names used\n",
        #         "\t\tin the reference the bam files were aligned to.\n",
        #         "\t\tOnly one of --chr or --chrFile may be specified."
        #     )
        # ),
        # optparse::make_option(
        #     c( "--chrFile" ), action= "store",
        #     default= NULL, metavar= "TEXT_FILE",
        #     help= paste0(
        #         "Include only the chromosomes listed in the specified file,\n",
        #         "\t\tone per line. The default is to keep all chromosomes.\n",
        #         "\t\tIf specified, chromosome must match the chromosome names used\n",
        #         "\t\tin the reference the bam files were aligned to.\n",
        #         "\t\tOnly one of --chr or --chrFile may be specified.\n"
        #     )
        # ),
        # optparse::make_option(
        #     c( "--readFilterRegionFile" ), action= "store",
        #     default= NULL, metavar= "BED_3_FILE",
        #     help= paste0(
        #         "Bed-3 file giving regions not to count, default is to count everything.\n",
        #         "\t\tOnly reads wholly contained within a given region will be skipped.\n"
        #     )
        # )
    })

})