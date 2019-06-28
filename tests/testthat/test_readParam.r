context( "Testing functions in readParam.R" )

describe( "readParam()", {

    describe( "The default readParam object", {
        it( "Returns an object of class readParam", {
            cli <- "../testData/test10.bam"
            opts <- parseCli( cli )
            got <- readParam( opts )
            expect_is( got, "readParam" )
        })
        it( "Returns an object with the expected default data.", {
            cli <- "../testData/test10.bam"
            opts <- parseCli( cli )
            obj <- readParam( opts )
            got <- list(
                pe= obj@pe, max.frag= obj@max.frag, dedup= obj@dedup,
                forward= obj@forward, minq= obj@minq, restrict= obj@restrict,
                discard= obj@discard, BPPARAM= obj@BPPARAM
            )
            # Restrict is internally converted to character(0) from NULL
            want <- list(
                pe= "both", max.frag= 500, dedup= FALSE,
                forward= NA, minq= NA_integer_, restrict= character(0),
                discard= GenomicRanges::GRanges(),
                BPPARAM= BiocParallel::SerialParam()
            )
            expect_equal( got, want )
        })
    })
    describe( "Can change readParam settings", {
        it( "pe= setting changes", {
            cli <- c( "--in", "../testData/test10.bam", "--readPairing", "single" )
            opts <- parseCli( cli )
            got <- readParam( opts )@pe
            want <- "none"
            expect_equal( got, want )
        })
        it( "max.frag= setting changes", {
            cli <- c( "--in", "../testData/test10.bam", "--readKeepMaxFragmentLength", "1000" )
            opts <- parseCli( cli )
            got <- readParam( opts )@max.frag
            want <- 1000
            expect_equal( got, want )
        })
        it( "dedup= setting changes", {
            cli <- c( "--readDropDuplicate", "../testData/test10.bam" )
            opts <- parseCli( cli )
            got <- readParam( opts )@dedup
            want <- TRUE
            expect_equal( got, want )
        })
        it( "forward= setting changes", {
            cli <- c( "--in", "../testData/test10.bam", "--readPairing=single", "--readKeepStrand=forward")
            opts <- parseCli( cli )
            got <- readParam( opts )@forward
            want <- TRUE
            expect_equal( got, want )
        })
        it( "minq= setting changes", {
            cli <- c( "--in", "../testData/test10.bam", "--readKeepMinQuality=21" )
            opts <- parseCli( cli )
            got <- readParam( opts )@minq
            want <- 21
            expect_equal( got, want )
        })
        it( "restrict= setting changes", {
            skip( "Feature not implemented" )

            cli <- c( "--in", "../testData/test10.bam" )
            opts <- parseCli( cli )
            got <- readParam( opts )@restrict
            want <- ""
            expect_equal( got, want )
        })
        it( "discard= setting changes", {
            skip( "Feature not implemented" )

            cli <- c( "--in", "../testData/test10.bam" )
            opts <- parseCli( cli )
            got <- readParam( opts )@discard
            want <- ""
            expect_equal( got, want )
        })
        it( "BPPARAM= setting changes", {
            skip( "Feature not implemented" )

            cli <- c( "--in", "../testData/test10.bam" )
            opts <- parseCli( cli )
            got <- readParam( opts )@BPPARAM
            want <- ""
            expect_equal( got, want )
        })
    })
})