//This file describes the js-lazy-commas, js-lazy-operators, and
// js-lazy-dots config options.
//It is recommended that if you use js-lazy-commas or js-lazy-operators,
// then you should use both, due to the funky indentation it causes otherwise

/*

// To turn on all 3, add the following to your .emacs file:

(custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js-lazy-commas t)
 '(js-lazy-operators t)
 '(js-lazy-dots t)
 '(js-expr-indent-offset 2)
 '(js-paren-indent-offset 2)
 '(js-square-indent-offset 2)
 '(js-curly-indent-offset 2))
*/

/*
*   js-lazy-commas
*/

//By default, js-mode supports the following comma-first style:

var obj1 = { prop1: { prop1: "val1"
                    , prop2: "val2"
                    }
           , prop2: "val3"
           }

//By turning on js-lazy-commas with the recommended settings, you can use
// the following style:

var obj2 = {
    prop1: {
        prop1: "val1"
      , prop2: "val2"
    }
  , prop2: "val3"
}

//Turn this on by putting the following in your .emacs file:
/*

(custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js-lazy-commas t)
 '(js-expr-indent-offset 2)
 '(js-paren-indent-offset 2)
 '(js-square-indent-offset 2)
 '(js-curly-indent-offset 2))
*/

/*
*   js-lazy-operators
*/

//By default, js-mode supports the following operator-first style:

var result1 = ( 5
              + 7
              / 2
              * 5
              )

//By turning on js-lazy-operators with the recommended settings, you can use
// the following style:

var result2 = (
    5
  + 7
  / 2
  * 5
)

//Turn this on by putting the following in your .emacs file:
/*

(custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js-lazy-operators t)
 '(js-expr-indent-offset 2)
 '(js-paren-indent-offset 2)
 '(js-square-indent-offset 2)
 '(js-curly-indent-offset 2))
*/

/*
*   js-lazy-dots
*/

//By default, js-mode supports the following dot-first style:
//(indent to the previous dot)

foo.bar()
   .baz()

//By turning on js-lazy-dots, you can use the following style:
//(indent once)

foo.bar()
  .baz()

//Turn this on by putting the following in your .emacs file:
/*

(custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js-lazy-dots t)
*/

