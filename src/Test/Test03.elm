module Test.Test03 exposing (script)

script = """
  
  fn fib(n) {

    if n < 2 {
      return n;

    } else {

      return fib(n-1) + fib(n-2);
    }
  }


let result = fib(13);  //233
  
"""


