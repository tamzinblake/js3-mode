// To run these tests, left-justify then indent each line and visually
// observe that the indentation level has not changed and no emacs errors
// were thrown.  Be sure to check 'Messages' before exiting.
// Tests assume default settings for js3-mode

function commaFirstStyle () {
  /* cases for comma-first, operator-first, and dot-first style */

  /* BASIC CASES */

  //test c1
  //commas line up under 'r' in 'var'
  var a = "ape"
    , b = "bat"
    , c = "cat"
    , d = "dog"
    , e = "elf"
    , f = "fly"
    , g = "gnu"
    , h = "hat"
    , i = "ibu"
    , j = "joe"
    , k = "kit"
    , l = "lay"
    , m = "man"
    , n = "now"
    , aPrettyLongVariableName

  //test c2
  //operators line up under '='
  a = b
    + c
    - d
    * e
    / f
    % g
    < h
    > i
    & j
    ^ k
    | l
    ? m
    : n

  //test c3
  //operators and ')' line up under '('
  a = ( b
      + c
      )

  //test c4
  //commas and '}' line up under '{'
  a = { b : c
      , d : e
      }

  //test c5
  //dot lines up under dot
  a.b
   .c()

  //test c6
  //commas lined up under 'n' in 'return'.
  //please don't ever do this
  if (a) {
    return 1
         , 2
         , 3
         , 4 // returns the last value, 4
  }

  //test c7
  //commas and ']' lined up under '['
  if (b) {
    return [ 1
           , 2
           , 3
           , 4
           ]
  }

  //test c8
  //function call - line up commas and ')' under '('
  a( b
   , c
   )

  /* OTHER CASES */
  /* This section contains cases which have historically been problematic,
   * or which illustrate past issues
   */

  //test c9
  //Double-quoted strings containing problematic characters
  a( aPrettyLongVariableName
   , "A string, which \"has some useful information"
   , "If you put these all together, it'd be too long"
   , { a: "is for antelope", b: "is for bat" }
   , 42
   )

  //test c10
  //Single-quoted strings containing problematic characters
  a( aPrettyLongVariableName
   , 'A string, which \'has some useful information'
   , 'If you put these all together, it\'d be too long'
   , { a: 'is for antelope', b: 'is for bat' }
   , 42
   )

  //test c11
  //line up with correct dot
  a.b( c.d )
   .e()

  //test c12
  //line up with correct brace
  a = { b: { c: d
           , e: f
           }
      , c: d
      }

  //test c13
  //line up with correct paren
  a = ( b + ( c
            / d
            )
      * e
      )

  //test c14
  //2-dimensional array
  a = [ [ b
        , c
        ]
      , [d, e]
      , f
      ]

  //test c15
  //skip multiline comment and line up comma and '}' under '{'
  //issue #6
  a = { b: c /*multiline { ,
               comment*/
      , d: e
      }

  //test c16
  //line up operators under first operator on line in same subexpression
  //issue #7
  var zoomThumbHtml = '<a target="_blank" id="zoomThumbnailLink"'
                    + '   title="Click to see full-size"'
                    + '   href="' + Ext.BLANK_IMAGE_URL + '">'
                    + '<img src="' + Ext.BLANK_IMAGE_URL + '"'

  //test c16
  //line up operators without trailing spaces
  //issue #11
  function test() {
    var x
    x.report("test1"
            +"test2"
            )
  }

  //test c17
  //properly ignoring '{' in strings
        //issue #12
  var foo = { prop: '{baz'
            }
    , bar

  //test c18
  //'+ e' should line up with '+ d'
  //issue #21
  a = { b : c + d
      + e
      }

  //test c19
  //function expression in object literal
  //function body should be indented one step from beginning of property name
      //issue #22
  a = { f: function () {
    var a = g
          + h

    return [ a
           , g
           ]
  }
      }

  //test c20
  //a big jumble of different things
  var rv = { a: [ b
                , ( c
                  + d
                  )
                , e
                ]
           , f: function ( g /* silly multiline
                                comment */
                         , h
                         ) {
             var a = g
                   + h

             return [ a
                    , g
                    ]
           }
           }
    , rv2 = a

  return 1
}

function commaLastStyle () {
                 /* Cases for comma-last, operator-last, and dot-last style */
                 /* BASIC CASES */

                 //test s1
                 //indent to one space after 'var', so 'a' lines up with 'b'
  var a = "ape",
      b = "bat",
      c = "cat",
      d = "dog",
      e = "elf",
      f = "fly",
      g = "gnu",
      h = "hat",
      i = "ibu",
      j = "joe",
      k = "kit",
      l = "lay",
      m = "man",
      n = "now",
      aPrettyLongVariableName

  //test s2
  //indent continued expressions by one level
  a = b +
    c -
    d *
    e /
    f %
    g <
    h >
    i &
    j ^
    k |
    l ?
    m :
    n

  //test s3
  //continued statements are indented one level,
  //and ')' lines up with beginning of line containing '('
  a = (
    b +
      c
  )

  //test s4
  //object literals are indented one level
  //and '}' lines up with beginning of line containing '{'
  a = {
    b : c,
    d : e
  }

  //test s5
  //continued dot expression indents one level
  a.b.
    c()

  //test s6
  //in square brackets, indent one level
  //and ']' lines up with beginning of line containing '['
  if (a) {
    return [
      1,
      2,
      3,
      4
    ]
  }

  //test s7
  //function call - indent one level
  //and ')' lines up with line containing '('
  a(
    b,
    c
  )

  /* OTHER CASES */
  /* This section contains cases which have historically been problematic,
      * or which illustrate past issues
      */

  //test s8
  //the second line should not even temporarily be indented past the first line
  //issue #18
  var a1
  var b1

  return 1
}
