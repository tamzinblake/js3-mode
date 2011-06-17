//This file describes the js3-lazy-commas, js3-lazy-operators, and
// js3-lazy-dots config options.
//It is recommended that if you use js3-lazy-commas or js3-lazy-operators,
// then you should use both, due to the funky indentation it causes otherwise

/*

// To turn on all 3, add the following to your .emacs file:

(custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js3-lazy-commas t)
 '(js3-lazy-operators t)
 '(js3-lazy-dots t)
 '(js3-expr-indent-offset 2)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(js3-curly-indent-offset 2))
*/

/*
*   js3-lazy-commas
*/

//By default, js3-mode supports the following comma-first style:

var obj1 = { prop1: { prop1: "val1"
                    , prop2: "val2"
                    }
           , prop2: "val3"
           }

//By turning on js3-lazy-commas with the recommended settings, you can use
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
 '(js3-lazy-commas t)
 '(js3-expr-indent-offset 2)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(js3-curly-indent-offset 2))
*/

/*
*   js3-lazy-operators
*/

//By default, js3-mode supports the following operator-first style:

var result1 = ( 5
              + 7
              / 2
              * 5
              )

//By turning on js3-lazy-operators with the recommended settings, you can use
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
 '(js3-lazy-operators t)
 '(js3-expr-indent-offset 2)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(js3-curly-indent-offset 2))
*/

/*
*   js3-lazy-dots
*/

//By default, js3-mode supports the following dot-first style:
//(indent to the previous dot)

foo.bar()
   .baz()

//By turning on js3-lazy-dots, you can use the following style:
//(indent once)

foo.bar()
  .baz()

//Turn this on by putting the following in your .emacs file:
/*

(custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js3-lazy-dots t)
*/

