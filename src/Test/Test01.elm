module Test.Test01 exposing (script)

script = """

use strjoin;
  
  enum City {
     Tokyo,
     Kobe,
     Osaka,
     Nara,
  };
  
  let z = City::Kobe;
  let x = False;

  if z == City::Kobe {
    x = True;
  }

  let a = 1;
  let b = 0;
  let c = 0;
  fn test ( a, b, c) {
     a = a + b + c;
     return a;
  }
  fn str ( l, r ) {
     return strjoin(l, r) ;
  }
  if a > 1 {
     b = 1;
  } else {
     let b = 2;
     c = 1;
  }
   a = 100;
  c = strjoin("ABC", "abc"); //lib
  let e1 = "abc";
  let e2 = "ABC";
  let e = strjoin(e1, e2); //lib
  let d = test(1,2,3);       //user func
  let ss = str("xyz", "1XYZ");
  return a;
  
"""


