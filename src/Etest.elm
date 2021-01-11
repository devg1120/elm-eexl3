module Etest exposing (..)


import Array
import Exec exposing (..)
import Expr exposing (..)
import Stdlib exposing (..)





test script_name =
    let
        --ast = run script script_name
        ast =
            stmt_parse script_name

        result =
            case ast of
                Err err ->
                    Debug.toString err

                Ok stmts ->
                    let
                        context =
                            empty
                                |> addFunction "strjoin" strjoin

                        --context = init_context
                        userenv =
                            userenvEmpty

                        ( userenv_2, ans ) =
                            eval userenv context stmts

                        ans2 =
                            case ans of
                                Context a ->
                                    a

                        env =
                            Debug.toString userenv_2

                        ans3 =
                            ans2.constants

                        log =
                            ans2.log

                        --ans4 = Debug.toString  (Dict.toList ans3)
                        --           |> String.replace "\"" ""
                        --           |> String.replace "OFloat" ""
                        --           |> String.replace "[" ""
                        --           |> String.replace "]" ""
                        --           |> String.replace "),(" ") ("
                        --ans4 = Debug.toString  (ans3)
                        ans4 =
                            Debug.toString ans2
                                |> String.replace "\"" ""
                                |> String.replace "OFloat" ""
                                |> String.replace "[" ""
                                |> String.replace "]" ""
                                |> String.replace "," " ="
                                |> String.replace ") =(" " , "

                        --|> String.replace "(" ""
                        --|> String.replace ")" ""
                        --|> String.replace "Stack Dict.fromList " ""
                    in
                    ans4 ++ " -> " ++ log ++ "[" ++ env
    in
    result



------------------------------------------------------


script3 =
    """
  
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

script1 = """

   let total = 0;
   let aaa = 100;
   let bbb = 200;
   let ccc = 300;
   let aa = 0;
   let bb = 0;
   let cc = 0;
   if aaa < bbb{
      aa = 1;
   } else {
      bb=  1;
   }
   if aaa < bbb {
      aa = aa + 2;
   }
   let za = 100;
   let zb = 200;
   let zc = 300;
   let zd = 400;
   let z = 0;

   if za > zb {
      z = 1;
    } elsif  za > zc {
      z=  2;
    } elsif  za > zd {
      z=  3;
    } else {
      z=  4;
   }
   let xa = 350;
   let xb = 200;
   let xc = 300;
   let xd = 400;
   let x = 0;
   if xa < xb {
      x = 1;
    } elsif  xa < xc {
      x=  2;
    } elsif  xa < xd {
      x=  3;
   }

   let ca = 6;
   let re = 0;
   case ca {
       0:
          re = 1;
       1:
          re = 2;
       2:
          re = 3;
   
       default:
        re = 5;
   }


"""
script2 = """
   let za = 150;
   let zb = 400;
   let zc = 300;
   let zd = 200;
   let z = 0;
   if za > zb {
      z = 1;
    } elsif  za > zc {
      z=  2;
    } elsif  za > zd {
      z=  3;
    } else {
      z=  4;
   }

"""
script4 = """
      let total = 0;
      let ok = 0;

      for ok in [1,2,3,4,5,6,7,8,9] {
             total = total + ok;
      }
  
"""
script5 = """ 
   let total = 0;
   let ok = 0;
   while total < 45 {
             //total = total + 1;
        if total > 9 {
               break;
        }
        total = total + 1;
   }

"""
script6 = """ 
  let a = 1;
  let b = 0;
  let c = 0;
  fn test ( a, b, c) {
        a = a + b + c;
        if a > 1 {
                    a = 0;
        }
         return a;
  }
  fn test2 (a) {
       if a > 1 {
                    a = 100;
         }
         return a;
  }
  fn str( l, r ) {
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
   let d2 = test2(2);       //user func
   let ss = str("xyz", "1XYZ");

"""
script7 = """ 

  fn fib(n) {
      if n < 2 {
           return n;
      } else {
           return fib(n-1) + fib(n-2);
      }
  }

 let result = fib(13);
"""
