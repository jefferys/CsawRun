context( "Console messages, warnings, and errors")

describe( "printBanner()", {
  it( "prints a message to STDOUT.", {
    wantRE <- '^csawRun v'
    expect_output( printBanner(), wantRE )
  })
})
