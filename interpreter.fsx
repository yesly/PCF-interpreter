(* An interpreter that processes programs from the PCF language.
The parser.fsx code is used to parse the input into an abstract tree.
The F# function interp takes an abstract syntax tree represented as a term and returns the result of evaluating it,
which will also be a term.
*)
  module Interp_script =
  
  // Skeleton file for PCF interpreter
  
  // This sets F# to read from whatever directory contains this source file.
  System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;
  
  #load "parser.fsx"
  
  // This lets us refer to "Parser.Parse.parsefile" simply as "parsefile",
  // and to constructors like "Parser.Parse.APP" simply as "APP".
  open Parser_script.Parse
  
  //  Substitution for PCF. Takes a term e, a string x representing an identifier, and a term t, and returns e with all free occurrences of ID x replaced by t.
  let rec subst e x t =
    match e with
    | ID(n) -> if n = x then t else e
    | APP (e1, e2) -> APP(e1, subst e2 x t)
    | IF (e1, e2, e3) -> IF(e1, (subst e2 x t), (subst e3 x t))
    | FUN (e1, e2) -> if e1 = x then FUN (e1, subst e2 x t) else FUN (e1, e2)
    | _ -> e

  let rec interp = function
  | NUM n -> NUM n // Rule (1)
  | BOOL true  -> BOOL true // Rule (2)
  | BOOL false -> BOOL false // Rule (2)
  | SUCC -> SUCC // Rule (3)
  | PRED -> PRED // Rule (3)
  | ISZERO -> ISZERO // Rule (3)
  | FUN (e1, e2)-> FUN (e1, e2)
  | IF(e1, e2, e3) ->
      match (interp e1) with
      | BOOL true -> interp e2
      | BOOL false -> interp e3
      | v -> ERROR (sprintf "'if' needs bool argument, not '%A'" v)
  | APP (e1, e2) ->
      match (interp e1, interp e2) with
      | (ERROR s, _)  -> ERROR s        // ERRORs are propagated
      | (_, ERROR s)  -> ERROR s
      | (SUCC, NUM n) -> NUM (n+1)      // Rule (6)
      | (SUCC, v)     -> ERROR (sprintf "'succ' needs int argument, not '%A'" v)
      | (PRED, NUM n) -> if n > 0 then NUM (n-1) else NUM (0)     // Rule (7)
      | (PRED, v)     -> ERROR (sprintf "'pred' needs int argument, not '%A'" v)
      | (ISZERO, NUM n) -> if n = 0 then BOOL(true) else BOOL(false)
      | (ISZERO, v) -> ERROR (sprintf "'iszero' needs int argument, not '%A'" v)
      | (FUN (e3, e4) , NUM n) ->
            match subst (FUN (e3, e4)) e3 (NUM n) with
            | FUN (e3, e4) -> interp e4
  | _ -> ERROR "not yet implemented"
 

  subst (ID "a") "a" (NUM 3)
  subst (NUM 6) "a" (NUM 3)
  subst (BOOL true) "a" (NUM 3)
  subst SUCC "a" (NUM 3)
  subst (APP(SUCC, ID "a")) "a" (NUM 3)
  subst (IF (BOOL true, FUN ("a", APP (SUCC, ID "a")), FUN ("b", APP (SUCC, ID "a")))) "a" (NUM 3)

  // Two convenient abbreviations for using the interpreter.
  let interpfile filename = filename |> parsefile |> interp
  
  let interpstr sourcecode = sourcecode |> parsestr |> interp

  // Examples.
  interpstr "succ 0" // returns 1
  interpstr "succ 1" // returns 2
  interpstr "pred 0" // returns 0, because we don't deal with negative numbers
  interpstr "pred 10" // returns 9
  interpstr "succ (succ (succ 0))" // returns 3
  interpstr "iszero succ" // returns error
  interpstr "iszero 0" // returns true
  interpstr "succ pred 7" // returns error
  interpstr "succ (pred 7)" // returns 7
  interpfile "if.txt" // works. returns 8
  interpfile "complex1.txt" // works. returns 1
  interpfile "complex2.txt" // works. returns 2
  interpfile "complex3.txt" // works. returns 3
  interpfile "complex4.txt" // works. returns 4
  interpstr "(fun x -> succ x) 4" // works. returns 5