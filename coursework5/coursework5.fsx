(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: Performing queries on FSharpON data structures

  ------------------------------------
  Name: Jan VLADAR
  Tallinn University of Technology Student ID
  or Uni-ID: 214328IV
  ------------------------------------


  Answer the questions below. You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework5/coursework5.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.
*)


(*

For introduction to FSharpON please check coursework4.fsx for references.

In this coursework we continue with the topic of trees and object notation. This
time the task is, given a description of a set of values in an Ecma,
how to retrieve, modify and delete those values. This is a bit similar
to questions 6 and 7 in coursework 4.

The following material of basic definitions is taken from CW4. You are
free to use your solution to CW4 or try something different.

*)



type Name = string


// 0. Define data structure(s) for representing FsharpON (same as in CW4)
//
type Ecma = Bool of bool
           | Number of float
           | String of string
           | List of List<Ecma>
           | Object of (Name * Ecma) list
           | Null

(*
Define the function

  mkObject : unit -> Ecma

that creates a representation for an empty object structure.
*)

let mkObject() : Ecma = Object []


(*
Define the function

  mkNumber : float -> Ecma

that creates a representation for the given floating-point number.
*)

let mkNumber (m : float) : Ecma = Number m

(*let rec unzip = function
       | []          -> ([],[])
       | (x,y)::rest -> let (xs,ys) = unzip rest
                        printfn "%A" rest
                        (x::xs,y::ys);
unzip [(1,"a");(2,"b")]


let reg1 = Map.ofList [("a1",("cheese",25));
                       ("a2",("herring",4));
                       ("a3",("soft drink",5))];;

Map.fold (fun cps ac (_,p) -> (ac,p)::cps) [] reg1;; // [("a3", 5); ("a2", 4); ("a1", 25)]

Map.foldBack (fun ac (_,p) cps -> (ac,p)::cps) reg1 [];; // [("a1", 25); ("a2", 4); ("a3", 5)]

List.fold (fun acc x -> x :: acc) [] [1; 2; 3; 4; 5]
// val it : int list = [5; 4; 3; 2; 1]

List.foldBack (fun x acc -> x :: acc) [1; 2; 3; 4; 5] [];;
// val it : int list = [1; 2; 3; 4; 5]


List.collect (fun x -> [[x+x]]) [2;3;5]*)
(*
Define the function

  mkBool : bool -> Ecma

that creates a representation for the given Boolean value.
*)

let mkBool (m : bool) : Ecma = Bool m
(*
Define the function

  mkString : string -> Ecma

that creates a representation for the given string value.
*)
let mkString (m : string) : Ecma = String m

(*
Define the function

 mkArray : Ecma list -> Ecma

that creates a representation for an array whose elements are
represented by the given list of `Ecma` values.
*)
let mkArray (m : Ecma list) : Ecma = List m

(*
Define the function

  mkNull : unit -> Ecma

that creates a representation of the ECMA-404 `null` value.
*)
let mkNull () = Null

// Define the function
//
//   addNameValue : Name * Ecma -> Ecma -> Ecma
//
// so that
//
//   addNameValue (n, v) e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an object representation
//
// - a representation for the object e extended with the name-value
//   pair (n, v), otherwise.

let addNameValue ( name : Name, v : Ecma) (e : Ecma) : Ecma =
  match e with 
  | Object kokot -> Object (kokot @ [name,v])
  | _ -> e


// Define the function
//
//   addValue : Ecma -> Ecma -> Ecma
//
// so that
//
//   addValue v e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an array representation
//
// - a representation for the array e with the value v added as the last
//   element, otherwise.


let addValue (v : Ecma) (e : Ecma) : Ecma =
  match e with 
  | List list -> List(list @ [v])
  | _ -> e


////////////////////////////////////////////////////////////////////////
// The content of this coursework begins here //////////////////////////


// You are given a type of expressions.

type BExpr = True
           | Not      of BExpr
           | And      of BExpr * BExpr
           | Or       of BExpr * BExpr
           | HasKey   of Name
           | HasStringValue of string
           | HasNumericValueInRange of (float*float)
           | HasBoolValue of bool
           | HasNull

(*

The type BExpr is just a discriminated union. The intended
interpretation of values of type BExpr is as predicates on values of
type Ecma.

 - True: evaluates to true on any Ecma

 - Not b: evaluates to true on precisely those Ecma for which b
          evaluates to false

 - And (b1, b2): evaluates to true on precisely those Ecma for which
                 both b1 and b2 evaluate to true

 - Or (b1, b2): evaluates to true on precisely those Ecma for which at least
                one of b1 and b2 evaluates to true

 - HasKey k: evaluates to true on precisely those Ecma that are objects and
             that contain a key k.

 - HasStringValue s: evaluates to true on precisely those Ecma that are 
            either Ecma strings with value s, objects that contain a value s,
            or arrays that contain a value s.

 - HasNumericValueInRange (xmin,xmax): evaluates to true on precisely those Ecma
               that either are
               numeric Ecma with value in closed range xmin,xmax,
               objects with a numeric value in closed range xmin,xmax,
               arrays with a numeric value in closed range xmin,xmax.

  - HasBoolValue b: evaluates to true on precisely those Ecma that are either
                    Boolean Ecma with value b,
                    objects that contain a Boolean value b,
                    arrays that contain a Boolean value b.
  - HasNull : evaluates to true on precisely those Ecma that are either
                    null Ecmas,
                    objects that conitain a null value,
                    arrays that contain a null value.

*)



// Here is a type of selector expressions.

type Selector = Match     of BExpr
              | Sequence  of Selector * Selector
              | OneOrMore of Selector


(*
The type Selector is just a discriminated union. The intended
interpretation of values of type Selector on values of type Ecma is as
sets of values in that Ecma. We also refer to the set of values
described by s : Selector as the set of values selected by s.

 - Match b: the singleton set consisting of the root value if the
            expression b evaluates to true and the empty set otherwise.
 
 - Sequence (s, s'): the set consisting of those values in the Ecma tree
                     that are selected by the selector s' starting from
                     any child value of a value that is selected by the
                     selector s (starting from the root value).

                     In other words, first determine the set of values
                     selected by s (starting from the root value). For
                     every child c of a value in this set, determine
                     the set of values selected by s' (starting from c)
                     and take the union of such sets as the result.

                     In other words, start from the root value with the
                     selector s. For the values that it selects,
                     continue with selector s' from their child values
                     and collect the results.
 
 - OneOrMore s: select the values selected by the selector s and, in
                addition, from the child values of the values in this
                set select the values selected by OneOrMore s.
 
                Thus, you can think of the values selected by OneOrMore s
                as the union of the following sets:
                - values selected by s
                - values selected by Sequence (s, OneOrMore s)
*)




// 1. Translate the following informal descriptions into values of
// type BExpr and Selector.
// 
// Define the values b1, b2 and b3 of type BExpr so that:
// 
//  - b1 evaluates to true on those Ecma that are object values
//    containing the keys "blue" and "left" but do not have the key "red".
// 
//  - b2 evaluates to true on those Ecma that are numeric values with
//    the value in the range [-5, 5).
// 
//  - b3 evaluates to true on those Ecma that have the string value "b3"
//    or that are object values which have the key "b3".
//
// Define the values s1, s2 and s3 of type Selector so that:
// 
//  - s1 selects all object values with key "abc" that are at depth 3
// 
//  - s2 selects all values v such that v is a child of some value
//    and all of the ancestors of v have the string value "xyz"
// 
//  - s3 selects all values v such that:
//    * v is a child of a value t
//    * t does not have a string value "xyz"
//    * t is the root value
// 
// We consider the root value to be at depth 1.

let b1 = And(And (HasKey "blue", HasKey "left"), Not(HasKey "red"))
let b2 = And((HasNumericValueInRange(-5.0,5.0)),Not(HasNumericValueInRange(5.0,5.0)))
let b3 = Or(HasStringValue("b3"),HasKey("b3"))
let s1 = Sequence(Match True,Sequence(Match True, Match (HasKey("abc"))))
let s2 = Sequence(OneOrMore(Match(HasStringValue("xyz"))), Match True)
let s3 = Sequence(Match(Not(HasStringValue("xyz"))), Match True)



(*2. Define the function

eval : BExpr -> Ecma -> bool

which evaluates the given expression on the given Ecma.

Evaluating a BExpr only considers properties of the root value of
the Ecma and its immediate child values that are leaves (if there are any).

In other words, for any  b : BExpr  and  e : Ecma

   eval b e = eval b e'

where e' is e with all of its non-leaf child values replaced
with the representation for null.)
*)

//let eval (bexpr : BExpr) (ecma : Ecma) : bool =

let rec eval (expression: BExpr) (e: Ecma) : bool =
    match expression with
    | True -> true
    | Not exp -> not (eval exp e)
    | And (firstE, secondE) -> (eval firstE e) && (eval secondE e)
    | Or (firstE, secondE) -> (eval firstE e) || (eval secondE e)
    | HasKey key ->
        match e with
        | Object object -> object |> List.exists (fun (name, _) -> name = key)
        | _ -> false
    | HasStringValue string ->
        match e with
        | String str -> string = str
        | List list -> list |> List.contains (String string)
        | Object object ->
            object
            |> List.exists
                (fun (_, value) ->
                    match value with
                    | String s -> s = string
                    | _ -> false)
        | _ -> false
    | HasNumericValueInRange (xmin, xmax) ->
        match e with
        | Number n -> n <= xmax && n >= xmin
        | List list ->
            list
            |> List.exists
                (fun item ->
                    match item with
                    | Number n -> n <= xmax && n >= xmin
                    | _ -> false)
        | Object object ->
            object
            |> List.exists
                (fun (_, value) ->
                    match value with
                    | Number n -> n <= xmax && n >= xmin
                    | _ -> false)
        | _ -> false
    | HasBoolValue bool ->
        match e with
        | Bool b -> bool = b
        | List list -> list |> List.contains (Bool bool)
        | Object object ->
            object
            |> List.exists
                (fun (_, value) ->
                    match value with
                    | Bool b -> b = bool
                    | _ -> false)
        | _ -> false
    | HasNull ->
        match e with
        | Null -> true
        | List list -> list |> List.contains Null
        | Object object ->
            object
            |> List.exists
                (fun (_, value) ->
                    match value with
                    | Null -> true
                    | _ -> false)
        | _ -> false
        
    (*match bexpr, ecma with 
    | True,_ -> true
    | Not n,_ -> true
    | And (first, second),_ -> true
    | Or (first, second),_ -> true
    | HasKey (name), Object object  -> true
    | HasStringValue (name), String text -> true
    | HasStringValue (name), Object object -> true
    | HasStringValue (name), List list -> true
    | HasNumericValueInRange (minim,maxim), Object object -> true
    | HasNumericValueInRange (minim,maxim), Number number -> true
    | HasNumericValueInRange (minim,maxim), List list -> true
    | HasBoolValue (boolean), Bool bool -> true 
    | HasBoolValue (boolean), Object object -> true
    | HasBoolValue (boolean), List list -> true 
    | HasNull, Null -> true
    | HasNull, Object object -> true
    | HasNull, List list -> true
    | _,_ -> false*)






type Description = Key   of string
                 | Index of int

type Path = Description list


// 3. Define the function
(*
select : Selector -> Ecma -> (Path * Ecma) list

that computes the set of values in the given Ecma described by the
given Selector. The result is a list of pairs where the second
component is a selected value and the first component is the full path
to this value.

The path to the root value is the empty list.

If you follow a child value of an object, then you add the key of
that value to the path. If you follow a child of an array, then you
add the index of that value in the array to the path (the oldest value
has index 0).

The order of values in the result list must respect the order of values
in the given Ecma. More precisely, in the result list:
- a value must appear before any of its children
- a value must not appear before its older siblings and their
  descendants

This task is similar to evaluating a BExpr on an Ecma. The difference is
that instead of a BExpr we have a Selector and instead of a bool we
compute a (Path * Ecma) list. In this case we also consider child
values.]
*)



let rec selectInner (s: Selector) (e: Ecma) (p: Path) : (Path * Ecma) list =
    printfn "objekt: %A a selektor: %A" e s

    match s with
    | Match expr -> if (eval expr e) then [ p, e ] else []
    | Sequence (s, s') ->
        let x = selectInner s e p

        match x with
        | [] -> []
        | arr ->
            arr
            |> List.fold
                (fun resultList (p', e') ->
                    resultList
                    @ match e' with 
                      | Object o ->
                          o
                          |> List.fold (fun acc (k2, v2) -> acc @ (selectInner s' v2 (p' @ [ Key(k2) ]))) []
                      | List eArr ->
                          fst (
                              eArr
                              |> List.fold
                                  (fun (arrInner, i) el -> (arrInner @ (selectInner s' el (p' @ [ Index(i) ])), i + 1))
                                  ([], 0)
                          )
                      | _ -> selectInner s' e' p'
                ) []

    | OneOrMore _ -> [] 

(*let policko = Object([ 
            "a", Number 1.0;
            "b", Object([ "b", String "b" ]);
            "c", List([ Object([ "a", String "a" ]);
                 Number 1.0;
                 Bool true;
                 List([ String "peler"; Object([ "p", String "p" ]);
                 Bool true ]);
                 Object([ "b", String "b" ]);
                 Bool false ])])*)

//let s = Sequence (Match (HasKey "c"), Match (HasBoolValue false))


let select (s:Selector) (e:Ecma) : (Path * Ecma) list =
  selectInner s e []

//let x = select s policko
//x





// 4. Define the function
//
// update :  (string -> string)
//        -> (float  -> float)
//        -> Selector
//        -> Ecma
// return
//        -> Ecma
// 
// such that
//
//    update su nu s e
//
// evaluates to an Ecma that is otherwise the same as e except that,
// for the values selected by s, the string values and numeric values
// of that value have been updated according to the functions su and nu.

let update (s : string) (f : float) (sel : Selector) (e: Ecma) : Ecma = 
  e






// 5. Define the function
//
// delete : Selector -> Ecma -> Ecma option
//
// which removes from the given Ecma all values that are selected by
// the given Selector. Removing a value means removing the entire
// subtree rooted at that value.
//
// The result should be `None` when after the delete operation there
// is no `Ecma` value left. Otherwise use `Some`.

let delete (s : Selector) (e: Ecma) : Ecma option = 
  None







// 6. Using the function update, define the functions
//
//   toZero : float -> Selector -> Ecma -> Ecma
//
// and
//
//   truncate : int -> Selector -> Ecma -> Ecma
//
// so that
//
//   toZero x s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their numeric values y replaced by 0
// if y is in the range [-x, x].
//
//   truncate n s e
//
// evaluates to an Ecma that is otherwise the same as e except that the
// values selected by s have each of their string values y truncated to
// length n.
//
// These functions should not be defined recursively; define them in
// terms of update.


let toZero (f : float) (s: Selector) (e: Ecma)  : Ecma = 
  e

let truncate (i : int) (s: Selector) (e: Ecma)  : Ecma = 
  e



// 7. Using the function update, define the function
// 
//   mapEcma : (string -> string) -> (float -> float) -> Ecma -> Ecma
//
// such that
//
//   mapEcma f g e
//
// evaluates to an Ecma obtained by updating every value in the
// given Ecma value according to f and g.
//
// This function should not be defined recursively; define it in
// terms of update.

let mapEcma (s : string) (f : float) (e : Ecma) : Ecma =
  e