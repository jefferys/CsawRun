context( "Console messages, warnings, and errors")

describe( "printBanner()", {
    it( "prints a message to STDOUT.", {
        opts <- list( debug= FALSE, verbose= FALSE )
        wantRE <- '^csawRun v'
        expect_output( printBanner( opts ), wantRE )
  })
})

