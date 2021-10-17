(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: FSharpON

  ------------------------------------
  Name: Jan VLADAR
  Tallinn University of Technology Student ID
  or Uni-ID: 214328IV
  ------------------------------------

  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework4/coursework4.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.
  
*)


(*

The ECMA-404 standard specifies a textual syntax for structured data
interchange.

The specification is available here:
https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf

The goal of this coursework is to develop a partial implementation of
this specification. In particular, our first goal is to define in F#
the datatype(s) suitable for representing the abstract syntax tree of
this data interchange format. The second goal is to define some
operations on this representation.

*)

// We have the following type alias.

type Name = string

//// Task 1 ////

// Define the type `Ecma` for representing the possible values of the
// ECMA-404 interchange format.
//
// The type must satisfy the constraint `equality`.

type Ecma = Bool of bool
           | Number of float
           | String of string
           | List of List<Ecma>
           | Object of (Name * Ecma) list
           | Null


// Define the following functions for creating ECMA-404
// representations of the given data.

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



//// Task 2 ////

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


//// Task 3 ////

// Define the function
// 
//   countValues : Ecma -> int
// 
// that counts the number of ECMA values in the given representation.
// 
// Keep in mind that both objects and arrays are themselves values and
// may contain other values inside.
//
// Furthermore, the following should hold:
//
//   1 + countValues e <= countValues (addValue v e)             // if e is an array representation
//
//   1 + countValues e <= countValues (addNameValue (n, v) e)    // if e is an object representation

let rec countValues ( ecma:Ecma) : int = 
  match ecma with 
  | Object object -> List.fold(fun total (k,p) -> total + (countValues p)) 1 object
  | List list -> List.fold(fun total p -> total + (countValues p)) 1 list
  | _ -> 1


//// Task 4 ////

type Path = Name list


// Define the function
// 
//   listPaths : Ecma -> Path list
// 
// that computes the full path for all the values in the given ECMA
// representation.
// 
// A path is just a list of names that take us from the root of the
// representation to a particular value.
// 
// For arrays, we consider the same path to take us to the array and to
// all of the elements in the array. Thus, for an array, we include the
// path to it and its elements only once in the result. 
// 
// If `e : Ecma` represents the following structure
// 
//   {
//     "abc" : false,
//     "xs"  : [ { "a" : "a" }, 1.0, true, { "b" : "b" }, false ],
//     [ { "a" : "a" }, 1.0, true, { "b" : "b" }, false ],
//     "xyz" : { "a" : 1.0, b : { "b" : "b" } },
//     "ws"  : [ false ]
//   }
// 
// then  `listPaths e` should result in
// 
//   [
//     [];
//     ["abc"];
//     ["xs"];
//     ["xs"; "a"];
//     ["xs"; "b"];
//     ["xyz"];
//     ["xyz"; "a"];
//     ["xyz"; "b"];
//     ["xyz"; "b"; "b"];
//     ["ws"]
//   ]
// 
// The ordering of paths in the result list matters:
// - paths to (sub)values in an array respect the order of elements in
//   the array
// 
// - paths to values in an object respect the order in which the values
//   were added to the object (most recently added appears last).
//
// Note that the empty list denotes the path to the root object.
//let simplify    

let listPaths (e: Ecma) : Path list =
    let rec innerlistPaths (e: Ecma) (prefix: Path) : Path list =
        match e with
        | Object (object :: rest) ->
            let newPrefix: Path = prefix @ [ fst(object) ]
            [ newPrefix ] @ innerlistPaths (snd(object)) newPrefix @ innerlistPaths (Object rest) prefix
        | List (head :: rest) ->
            innerlistPaths head prefix @ innerlistPaths (List rest) prefix
        | _ -> []

    [] :: innerlistPaths e []


//printfn "%A" picovina

// listPaths (picovina)  


//// Task 5 ////

// Define the function
// 
//   show : Ecma -> string
// 
// that computes a string representation of the given ECMA representation
// in such a way that the ordering requirements from the previous task are
// respected.
// 
// The result should not contain any whitespace except when this
// whitespace was part of a name or a string value.

let rec show (ecma: Ecma) =
    match ecma with
    | Object o -> objectToJson o
    | List l -> listToJson l
    | Bool b -> b.ToString().ToLower()
    | Number n -> n.ToString()
    | String t -> "\"" + t + "\""
    | Null -> "null"

and objectToJson (object: list<Name * Ecma>) =
    match object with
    | [] -> "{}"
    | _ ->
        "{\""
        + fst object.Head
        + "\":"
        + show (snd (object.Head))
        + (object.Tail
           |> List.fold (fun res (name, e) -> res + "," + "\"" + name + "\":" + show e) "")
        + "}"

and listToJson (list: list<Ecma>) =
    match list with
    | [] -> "[]"
    | _ ->
        "["
        + show (list.Head)
        + (list.Tail
           |> List.fold (fun res e -> res + "," + show e) "")
        + "]"


//// Task 6 ////

// Define the function
// 
//   delete : Path list -> Ecma -> Ecma
// 
// so that
// 
//   delete ps e
// 
// evaluates to a representation `e'` that is otherwise the same as `e` but
// all name-value pairs with paths in the path list `ps` have been removed.
//
// When the user attempts to delete the root object, delete should throw
// an exception. Hint: use `failwith` in the appropriate case.

let delete (paths: Path list) (ecma : Ecma) : Ecma = mkObject()

//// Task 7 ////

// Define the function
// 
//   withPath : Path list -> Ecma -> Ecma list
// 
// so that
// 
//   withPath ps e
// 
// evaluates to a list of object representations consisting of those
// objects in `e` that are represented by a path from the list `ps`.
// 
// The result list must respect the ordering requirements from Task 4.

let withPath (path : Path list) (ecma : Ecma) : Ecma list = []

