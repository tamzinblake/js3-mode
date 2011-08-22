// To run these tests, left-justify then indent each line and visually
// observe that the indentation level has not changed and no emacs errors
// were thrown.  Be sure to check 'Messages' before exiting.
// These tests should be run with the recommended settings for all 3 lazy-modes

//an alternate comma-first style supported by js3-mode

//The assumption is that you will consistently not put the first item in the
//list on the same line as the brace, and then want the commas / operators
//2 spaces back from there.

//normal cases:

function lazyCommaFirst () {

    // test lc1
    // 'prop2' should line up directly under 'prop1'
    // and 'prop3' should line up directly under 'prop2'
    // issue # 8
    var test = {
        prop1: "value1"
      , prop2: "value2"
      , prop3: "value3"
    }

    // test lc2
    // "value2" should line up directly under "value1"
    // and "value3" should line up directly under "value2"
    var y = [
        "value1"
      , "value2"
      , "value3"
    ]

    // test lc3
    // big nested thing - it should work as advertised
    // issue #9
    var test2 = {
        test1: {
            test2: {
                test3: 'this is ok'
            }
          , test4: {
              test5: 'this is weird'
            , test6: 'this is weird also'
          }
        }
    }

    // test lc4
    // should work as advertised
    // issue #37
    var logStream = fs.createWriteStream(argv.log, {
        flags: 'a'
      , mode: 0644
    })}

function lazyOperatorFirst () {

    // test lo1
    // should line up "value2" directly under "value1"
    // and "value3" directly under "value2"
    var z = (
        "value1"
      + "value2"
      + "value3"
    )

    // test lo2
    // "test2" should line up directly under "test1"
    // and "test3" should line up directly under "test2"
    // issue #10
    function test2 () {
        var x
        x.report(
            "test1"
          + "test2"
          + "test3"
        )
    }
}

function lazyDotFirst () {

    //test ld1
    //should indent line beginning with dot one indent
    function moo() {
        foo.bar()
          .baz()
    }

    //test ld2
    //should indent line beginning with dot one indent
    foo.bar()
      .baz()

    //test ld3
    //should indent line beginning with dot one indent
    function foo (bar) {
        bar.baz
          .beep()
    }
}
