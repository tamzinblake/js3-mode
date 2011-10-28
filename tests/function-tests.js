function functionTests () {
  /* cases for function indentation */

  //test f1
  // basic function declaration
  //function body should be indented one step past 'function'
  function test() {
    var x
  }

  //test f2
  //function expression in object literal
  //function body should be indented one step from beginning of property name
  var a = { function_name: function () {
              var b = 1
            }
          , other_function: function () {
              var c = 1
            }
          }

  //test f3
  //function on first line of method call
  //function body should be indented one step
  a.addEvent('click', function (e) {
    var c = 1
  })

  //test f4
  //function on subsequent line of method call
  //function body should be indented one step from beginning of function keyword
  a.addEvent( 'click'
            , function (e) {
                var c = 1
              }
            )

  //test f5
  //just making sure this still works - silly regex
  //function body should be indented one step from function keyword
  var text = text.replace( /https?\:\/\/[^"\s\<\>]*[^.,;'">\:\s\<\>\)\]\!]/g
                         , function (wholeMatch,matchIndex) {
                             var left = text.slice(0, matchIndex)
                           }
                         )

  //test f6
  //var keyword special case
  //compromise: function body is indented one step past function name
  var function_name = function () {
        return 1
      }
    , function_2 = function () {
        return 2
      }

  //test f7
  //function expression
  //In pretty much all other cases, function expressions should indent
  // one step from the beginning of the 'function' keyword
  ;(function () {
      var a = 1
    })()
  = (function () {
       var b = 2
     })()

  //end function tests

  return 1;
}
