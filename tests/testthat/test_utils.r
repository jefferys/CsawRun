context( "Utility functions" )

describe( "naDecode", {
    it( "Decodes 'NA' in character vectors by default", {
        got <- naDecode( c("N", "NA", "NAN") )
        want <- c("N", NA, "NAN")
        expect_equal( got, want )

        got <- naDecode( c("NA") )
        want <- c(NA_character_)
        expect_equal( got, want )
    })
    it( "Can specify what character string gets decoded", {
        got <- naDecode( c("A", "B", ".", "D", "."), naSymbol= "." )
        want <-  c("A", "B", NA, "D", NA)
        expect_equal( got, want )

        got <- naDecode( c("I am missing!"), naSymbol="I am missing!" )
        want <- c(NA_character_)
        expect_equal( got, want )
    })
})
describe( "stringify()", {
    describe( "stringify()", {
        describe( "Using default parameters with vectors", {
            it( "Handles character vectors", {
                x <- c( A= "Alice", "Bob", "C or c"= "Carol", D= "", NA, "", End= 1 )
                got <- stringify( x )
                want <- "A= Alice, Bob, `C or c`= Carol, D= , NA, , End= 1"
                expect_equal( got, want )
            })
            it( "Handles logical vectors", {
                x <- c( A= FALSE, `in`= TRUE, missing= NA, F, last= T )
                got <- stringify( x )
                want <- "A= FALSE, `in`= TRUE, missing= NA, FALSE, last= TRUE"
                expect_equal( got, want )
            })
            it( "Handles integer vectors", {
                x <- c( A= 0L, `in`= -123L, missing= NA, 100L )
                got <- stringify( x )
                want <- "A= 0, `in`= -123, missing= NA, 100"
                expect_equal( got, want )
            })
            it( "Handles numeric (double) vectors", {
                x <- c( A= 0.0, `in`= 0L, missing= NA_real_, -0.100, 0.0001, inf= Inf, -Inf, noNum= NaN )
                got <- stringify( x )
                want <- "A= 0, `in`= 0, missing= NA, -0.1, 1e-04, inf= Inf, -Inf, noNum= NaN"
                expect_equal( got, want )
            })
            it( "Handles complex vectors", {
                # numeric values
                x <- complex( real= c( 0, 101, 0, -1.1 ),
                              imaginary= c( 0, 0, -0.1, 2.2 ))
                names(x) <- c("zero", "bad name", "", "")
                got <- stringify( x )
                want <- "zero= 0+0i, `bad name`= 101+0i, 0-0.1i, -1.1+2.2i"
                expect_equal( got, want )
                # Non-numeric values
                x <- complex( real= c( 1.1, -1.1, 1.1, Inf, -Inf, NaN, Inf, -Inf, NaN ),
                              imaginary= c( Inf, -Inf, NaN, 1.1, -1.1, 1.1, Inf, -Inf, NaN ))
                names(x) <- c( "first", "bad name", rep.int( "", length(x) - 2 ))
                got <- stringify( x )
                want <- paste0( "first= 1.1+Infi, `bad name`= -1.1-Infi,",
                                " 1.1+NaNi, Inf+1.1i, -Inf-1.1i, NaN+1.1i,",
                                " Inf+Infi, -Inf-Infi, NaN+NaNi" )
                expect_equal( got, want )
            })
            it( "Handles raw vectors", {
                x <- as.raw( c(0, 9, 10, 15, 16, 159, 160, 255 ))
                names(x) <- c( "first", "bad name", rep.int( "", length(x) - 2 ))
                got <- stringify( x )
                want <- paste0( "first= 00, `bad name`= 09,",
                                " 0a, 0f, 10, 9f, a0, ff" )
                expect_equal( got, want )
            })
            it( "Handles null", {
                got <- stringify( NULL )
                want <- "NULL"
                expect_equal( got, want )
            })
        })
        describe( "Using default parameters with lists", {
            it( "Handles a simple list that looks like a character vector", {
                x <- list( A= "Alice", "Bob", "C or c"= "Carol", D= "", NA, "", End= "1", null= NULL )
                got <- stringify( x )
                want <- "{A= Alice, Bob, `C or c`= Carol, D= , NA, , End= 1, null= NULL}"
                expect_equal( got, want )
            })
            it( "Handles a nested list that looks like a list of character vectors", {
                x <- list( A= "Alice", "Bob", `C or c`= "Carol", D= "", NA, "", End= "1",
                           L1= list( A= "Alice", "Bob", `C or c`= "Carol", D= "", NA, "", End= "1", null= NULL ),
                           list( A= "one element list" ),
                           list( "one element list" ),
                           L4= list( L5= list(L6= list( "deepList" )))
                )
                got <- stringify( x )
                want <- paste0( "{A= Alice, Bob, `C or c`= Carol, D= , NA, , End= 1,",
                                " L1= {A= Alice, Bob, `C or c`= Carol, D= , NA, , End= 1, null= NULL},",
                                " {A= one element list},",
                                " {one element list},",
                                " L4= {L5= {L6= {deepList}}}}" )
                expect_equal( got, want )
            })
            it( "Handles mixed-type lists", {
                x <- list(
                    charVec= structure( as.character( c( "ABC", "", NA )),
                                             names= c("A", "", "")),
                    intVec= structure( as.integer( c( 0L, -2L, NA )),
                                             names= c("A", "", "")),
                    logVec= structure( as.logical( c( TRUE, FALSE, NA )),
                                       names= c("", "", "")),
                    doubleVec= structure( as.double( c( 1.1, NA, NaN, Inf, -Inf )),
                                          names= c( "A", "B", "C", "D", "E" )),
                    complexVec= structure( complex( real= c( 1.1, NA, NA, NaN ),
                                                    imaginary= c( -1.1, -1.1, NA, NaN )),
                                           names= c( "A", "", "C", "" )),
                    rawVec= as.raw( c( 1, 16, 255 ) ),
                    null= as.list( NULL ),
                    list( A= list( list ( A= 42 )))

                )
                got <- stringify( x )
                want <- paste0( "{charVec= (A= ABC, , NA),",
                                " intVec= (A= 0, -2, NA),",
                                " logVec= (TRUE, FALSE, NA),",
                                " doubleVec= (A= 1.1, B= NA, C= NaN, D= Inf, E= -Inf),",
                                " complexVec= (A= 1.1-1.1i, NA, C= NA, NaN+NaNi),",
                                " rawVec= (01, 10, ff),",
                                " null= {NULL},",
                                " {A= {{A= 42}}}}" )
                expect_equal( got, want )
            })
        })
        describe( "Non-default parameters with character vectors", {
            describe( "Non-default parameters with character vectors", {
                x <- as.character( c( "one", "two", "", "", NA, NA ))
                names(x) <- c("A", "", "B", "", "C C", "")
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                         empty= "''", naSymbol= "." )
                    want <- "A: one; two; B: ''; ''; `C C`: .; ."
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- "*A*: |one|; |two|; *B*: |''|; |''|; *C C*: |.|; |.|"
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- "[A]: <one>; <two>; [B]: <''>; <''>; [C C]: <.>; <.>"
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- "one; two; ''; ''; .; ."
                    expect_equal( got, want )
                })
            })
            describe( "Non-default parameters with integer vectors", {
                x <- as.integer( c( 1L, -1L, 0L, 1000000L, NA, NA ))
                names(x) <- c("A", "", "B", "", "C C", "")
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= "." )
                    want <- "A: 1; -1; B: 0; 1000000; `C C`: .; ."
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- "*A*: |1|; |-1|; *B*: |0|; |1000000|; *C C*: |.|; |.|"
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- "[A]: <1>; <-1>; [B]: <0>; <1000000>; [C C]: <.>; <.>"
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- "1; -1; 0; 1000000; .; ."
                    expect_equal( got, want )
                })
                it( "prettyNum= and parameters", {
                    got <- stringify( x, big.mark= "," )
                    want <- "A= 1, -1, B= 0, 1,000,000, `C C`= NA, NA"
                    expect_equal( got, want )
                    got <- stringify( x, big.mark= ",", prettyNum=FALSE )
                    want <- "A= 1, -1, B= 0, 1000000, `C C`= NA, NA"
                    expect_equal( got, want )
                })
            })
            describe( "Non-default parameters with double vectors", {
                x <- as.double( c( 1.1, -1.10, -0.01, 0.01, NA, NA, NaN, NaN ))
                names(x) <- c("A", "", "B", "", "C C", "", "D", "")
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= "." )
                    want <- "A: 1.1; -1.1; B: -0.01; 0.01; `C C`: .; .; D: NaN; NaN"
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- "*A*: |1.1|; |-1.1|; *B*: |-0.01|; |0.01|; *C C*: |.|; |.|; *D*: |NaN|; |NaN|"
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- "[A]: <1.1>; <-1.1>; [B]: <-0.01>; <0.01>; [C C]: <.>; <.>; [D]: <NaN>; <NaN>"
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- "1.1; -1.1; -0.01; 0.01; .; .; NaN; NaN"
                    expect_equal( got, want )
                })
                it( "prettyNum= and parameters", {
                    got <- stringify( x, decimal.mark= ",", small.interval=1, small.mark="_" )
                    want <- "A= 1,1, -1,1, B= -0,0_1, 0,0_1, `C C`= NA, NA, D= NaN, NaN"
                    expect_equal( got, want )
                    got <- stringify( x, decimal.mark= ",", small.interval=1, small.mark="_", prettyNum= FALSE )
                    want <- "A= 1.1, -1.1, B= -0.01, 0.01, `C C`= NA, NA, D= NaN, NaN"
                    expect_equal( got, want )
                })

            })
            describe( "Non-default parameters with complex vectors", {
                x <- complex( real= c(NaN, NaN, NaN, 100, 1.1, 1, NA, NA, NA),
                              imaginary= c(NaN, -1.1, NA, NaN, 100, NA, NaN, -1.1, NA))
                names(x) <- c("A", "", "", "B", "", "", "C C", "", "")
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= "." )
                    want <- "A: NaN+NaNi; NaN-1.1i; .; B: 100+NaNi; 1.1+100i; .; `C C`: .; .; ."
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- paste0( "*A*: |NaN+NaNi|; |NaN-1.1i|; |.|;",
                                    " *B*: |100+NaNi|; |1.1+100i|; |.|;",
                                    " *C C*: |.|; |.|; |.|" )
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- paste0( "[A]: <NaN+NaNi>; <NaN-1.1i>; <.>;",
                                    " [B]: <100+NaNi>; <1.1+100i>; <.>;",
                                    " [C C]: <.>; <.>; <.>" )
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- "NaN+NaNi; NaN-1.1i; .; 100+NaNi; 1.1+100i; .; .; .; ."
                    expect_equal( got, want )
                })
                it( "prettyNum= and parameters", {
                    got <- stringify( x, decimal.mark= ",", big.interval=2, big.mark="_" )
                    want <- "A= NaN+NaNi, NaN-1,1i, NA, B= 1_00+NaNi, 1,1+1_00i, NA, `C C`= NA, NA, NA"
                    expect_equal( got, want )
                    got <- stringify( x, decimal.mark= ",", small.interval=1, small.mark="_", prettyNum= FALSE )
                    want <- "A= NaN+NaNi, NaN-1.1i, NA, B= 100+NaNi, 1.1+100i, NA, `C C`= NA, NA, NA"
                    expect_equal( got, want )
                })
            })
            describe( "Non-default parameters with logical vectors", {
                x <- as.logical( c(TRUE, TRUE, FALSE, FALSE, NA, NA))
                names(x) <- c("A", "", "B", "", "C C", "")
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= "." )
                    want <- "A: TRUE; TRUE; B: FALSE; FALSE; `C C`: .; ."
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- "*A*: |TRUE|; |TRUE|; *B*: |FALSE|; |FALSE|; *C C*: |.|; |.|"
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- "[A]: <TRUE>; <TRUE>; [B]: <FALSE>; <FALSE>; [C C]: <.>; <.>"
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- "TRUE; TRUE; FALSE; FALSE; .; ."
                    expect_equal( got, want )
                })
                it( "logicalSymbols=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[", "]"), valueDelim="|",
                                      logicalSymbols=c( "T", "F" ))
                    want <- "[A]: |T|; |T|; [B]: |F|; |F|; [C C]: |.|; |.|"
                    expect_equal( got, want )
                })
            })
            describe( "Non-default parameters with raw vectors", {
                x <- as.raw( c( 1, 10, 255 ))
                names(x) <- c("A", "", "C C")
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= "." )
                    want <- "A: 01; 0a; `C C`: ff"
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- "*A*: |01|; |0a|; *C C*: |ff|"
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- "[A]: <01>; <0a>; [C C]: <ff>"
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- "01; 0a; ff"
                    expect_equal( got, want )
                })
            })
            it( "Non default parameters with null", {
                got <- stringify( NULL, nullSymbol= "<null>" )
                want <- "<null>"
                expect_equal( got, want )
            })
        })
        describe( "Non-default parameters with lists", {
            describe( "Simple list that looks like a character vector", {
                x <- list( A= "Alice", "Bob", "C or c"= "Carol", D= "", NA, "", End= "1", null= NULL )
                it( "Respects nameSep=, elemSep=, empty=, nullSymbol=, and naSymbol= parameters",{
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", nullSymbol= "_null_" )
                    want <- "{A: Alice; Bob; `C or c`: Carol; D: ''; .; ''; End: 1; null: _null_}"
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", nullSymbol= "_null_",
                                      nameDelim="*", valueDelim="|")
                    want <- "{*A*: |Alice|; |Bob|; *C or c*: |Carol|; *D*: |''|; |.|; |''|; *End*: |1|; *null*: _null_}"
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", nullSymbol= "_null_",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- "{[A]: <Alice>; <Bob>; [C or c]: <Carol>; [D]: <''>; <.>; <''>; [End]: <1>; [null]: _null_}"
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ", nullSymbol= "_null_",
                                      empty= "''", naSymbol= ".", dropName= TRUE )
                    want <- "{Alice; Bob; Carol; ''; .; ''; 1; _null_}"
                    expect_equal( got, want )
                })
                it( "listSymbols=; vectorSymbols=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", nullSymbol= "_null_",
                                      listSymbols=c("<",">"), vectorSymbols=c("[","]"))
                    want <- "<A: Alice; Bob; `C or c`: Carol; D: ''; .; ''; End: 1; null: _null_>"
                    expect_equal( got, want )
                })
            })
            describe( "Nested list that looks like a list of character vectors", {
                x <- list( A= "Alice", "Bob", "C or c"= "Carol", D= "", NA, "", End= "1",
                           L1=list(A= "Alice", "Bob", "C or c"= "Carol", D= "", NA, "", End= "1"),
                           list(A= "one element named"),
                           list("one element unnamed"),
                           L4=list(L5=list(L6=list("deep")))
                )
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters",{
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= "." )
                    want <- paste0( "{A: Alice; Bob; `C or c`: Carol; D: ''; .; ''; End: 1;",
                                    " L1: {A: Alice; Bob; `C or c`: Carol; D: ''; .; ''; End: 1};",
                                    " {A: one element named};",
                                    " {one element unnamed};",
                                    " L4: {L5: {L6: {deep}}}}" )
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- paste0( "{*A*: |Alice|; |Bob|; *C or c*: |Carol|; *D*: |''|; |.|; |''|; *End*: |1|;",
                                    " *L1*: {*A*: |Alice|; |Bob|; *C or c*: |Carol|; *D*: |''|; |.|; |''|; *End*: |1|};",
                                    " {*A*: |one element named|};",
                                    " {|one element unnamed|};",
                                    " *L4*: {*L5*: {*L6*: {|deep|}}}}" )
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- paste0( "{[A]: <Alice>; <Bob>; [C or c]: <Carol>; [D]: <''>; <.>; <''>; [End]: <1>;",
                                    " [L1]: {[A]: <Alice>; <Bob>; [C or c]: <Carol>; [D]: <''>; <.>; <''>; [End]: <1>};",
                                    " {[A]: <one element named>};",
                                    " {<one element unnamed>};",
                                    " [L4]: {[L5]: {[L6]: {<deep>}}}}" )
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- paste0( "{Alice; Bob; Carol; ''; .; ''; 1;",
                                    " {Alice; Bob; Carol; ''; .; ''; 1};",
                                    " {one element named};",
                                    " {one element unnamed};",
                                    " {{{deep}}}}" )
                    expect_equal( got, want )
                })
                it( "listSymbols=; vectorSymbols=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      listSymbols=c("<",">"), vectorSymbols=c("[","]"))
                    want <- paste0( "<A: Alice; Bob; `C or c`: Carol; D: ''; .; ''; End: 1;",
                                    " L1: <A: Alice; Bob; `C or c`: Carol; D: ''; .; ''; End: 1>;",
                                    " <A: one element named>;",
                                    " <one element unnamed>;",
                                    " L4: <L5: <L6: <deep>>>>" )
                    expect_equal( got, want )
                })
            })
            describe( "Mixed-type lists", {
                x <- list(
                    charVec= structure( as.character( c( "ABC", "", NA )),
                                        names= c("A A", "", "")),
                    intVec= structure( as.integer( c( 0L, -20L, NA )),
                                       names= c("A", "", "")),
                    logVec= structure( as.logical( c( TRUE, FALSE, NA )),
                                       names= c("", "", "")),
                    doubleVec= structure( as.double( c( 10.1, NA, NaN, Inf, -Inf )),
                                          names= c( "A", "B", "C", "D", "E" )),
                    complexVec= structure( complex( real= c( 10.1, NA, NA, NaN ),
                                                    imaginary= c( -10.1, -10.1, NA, NaN )),
                                           names= c( "A", "", "C", "" )),
                    rawVec= as.raw( c( 1, 16, 255 ) ),
                    list( A= list( list ( A= 42 )))
                )
                it( "Respects nameSep=, elemSep=, empty=, and naSymbol= parameters",{
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= "." )
                    want <- paste0( "{charVec: (`A A`: ABC; ''; .);",
                                    " intVec: (A: 0; -20; .);",
                                    " logVec: (TRUE; FALSE; .);",
                                    " doubleVec: (A: 10.1; B: .; C: NaN; D: Inf; E: -Inf);",
                                    " complexVec: (A: 10.1-10.1i; .; C: .; NaN+NaNi);",
                                    " rawVec: (01; 10; ff);",
                                    " {A: {{A: 42}}}}" )
                    expect_equal( got, want )
                })
                it( "nameDelim=; valueDelim=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim="*", valueDelim="|")
                    want <- paste0( "{*charVec*: (*A A*: |ABC|; |''|; |.|);",
                                    " *intVec*: (*A*: |0|; |-20|; |.|);",
                                    " *logVec*: (|TRUE|; |FALSE|; |.|);",
                                    " *doubleVec*: (*A*: |10.1|; *B*: |.|; *C*: |NaN|; *D*: |Inf|; *E*: |-Inf|);",
                                    " *complexVec*: (*A*: |10.1-10.1i|; |.|; *C*: |.|; |NaN+NaNi|);",
                                    " *rawVec*: (|01|; |10|; |ff|);",
                                    " {*A*: {{*A*: |42|}}}}" )
                    expect_equal( got, want )
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[","]"), valueDelim=c("<",">"))
                    want <- paste0( "{[charVec]: ([A A]: <ABC>; <''>; <.>);",
                                    " [intVec]: ([A]: <0>; <-20>; <.>);",
                                    " [logVec]: (<TRUE>; <FALSE>; <.>);",
                                    " [doubleVec]: ([A]: <10.1>; [B]: <.>; [C]: <NaN>; [D]: <Inf>; [E]: <-Inf>);",
                                    " [complexVec]: ([A]: <10.1-10.1i>; <.>; [C]: <.>; <NaN+NaNi>);",
                                    " [rawVec]: (<01>; <10>; <ff>);",
                                    " {[A]: {{[A]: <42>}}}}" )
                    expect_equal( got, want )
                })
                it( "Drops names with dropName= TRUE", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".", dropName=TRUE )
                    want <- paste0( "{(ABC; ''; .);",
                                    " (0; -20; .);",
                                    " (TRUE; FALSE; .);",
                                    " (10.1; .; NaN; Inf; -Inf);",
                                    " (10.1-10.1i; .; .; NaN+NaNi);",
                                    " (01; 10; ff);",
                                    " {{{42}}}}" )
                    expect_equal( got, want )
                })
                it( "listSymbols=; vectorSymbols=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      listSymbols=c("<",">"), vectorSymbols=c("[","]"))
                    want <- paste0( "<charVec: [`A A`: ABC; ''; .];",
                                    " intVec: [A: 0; -20; .];",
                                    " logVec: [TRUE; FALSE; .];",
                                    " doubleVec: [A: 10.1; B: .; C: NaN; D: Inf; E: -Inf];",
                                    " complexVec: [A: 10.1-10.1i; .; C: .; NaN+NaNi];",
                                    " rawVec: [01; 10; ff];",
                                    " <A: <<A: 42>>>>" )
                    expect_equal( got, want )
                })
                it( "logicalSymbols=", {
                    got <- stringify( x, nameSep= ": ", elemSep= "; ",
                                      empty= "''", naSymbol= ".",
                                      nameDelim=c("[", "]"), valueDelim="|",
                                      logicalSymbols=c( "T", "F" ))
                    want <- paste0( "{[charVec]: ([A A]: |ABC|; |''|; |.|);",
                                    " [intVec]: ([A]: |0|; |-20|; |.|);",
                                    " [logVec]: (|T|; |F|; |.|);",
                                    " [doubleVec]: ([A]: |10.1|; [B]: |.|; [C]: |NaN|; [D]: |Inf|; [E]: |-Inf|);",
                                    " [complexVec]: ([A]: |10.1-10.1i|; |.|; [C]: |.|; |NaN+NaNi|);",
                                    " [rawVec]: (|01|; |10|; |ff|);",
                                    " {[A]: {{[A]: |42|}}}}" )
                    expect_equal( got, want )
                })
                it( "prettyNum= and parameters", {
                    got <- stringify( x, decimal.mark= ",", big.interval=1, big.mark="_" )
                    want <- paste0( "{charVec= (`A A`= ABC, , NA),",
                                    " intVec= (A= 0, -2_0, NA),",
                                    " logVec= (TRUE, FALSE, NA),",
                                    " doubleVec= (A= 1_0,1, B= NA, C= NaN, D= Inf, E= -Inf),",
                                    " complexVec= (A= 1_0,1-1_0,1i, NA, C= NA, NaN+NaNi),",
                                    " rawVec= (01, 10, ff),",
                                    " {A= {{A= 4_2}}}}" )
                    expect_equal( got, want )
                    got <- stringify( x, decimal.mark= ",", big.interval=1, big.mark="_", prettyNum= FALSE )
                    want <- paste0( "{charVec= (`A A`= ABC, , NA),",
                                    " intVec= (A= 0, -20, NA),",
                                    " logVec= (TRUE, FALSE, NA),",
                                    " doubleVec= (A= 10.1, B= NA, C= NaN, D= Inf, E= -Inf),",
                                    " complexVec= (A= 10.1-10.1i, NA, C= NA, NaN+NaNi),",
                                    " rawVec= (01, 10, ff),",
                                    " {A= {{A= 42}}}}" )
                    expect_equal( got, want )
                })
            })
        })
    })
})
