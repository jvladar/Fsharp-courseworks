(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences, laziness and computation expressions

  ------------------------------------------------------------------------------
  Name: Jan Vladar
  Student ID: javlad
  ------------------------------------------------------------------------------


  Answer the questions below. Your answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework8.fsx in directory coursework8.


  The deadline for completing the above procedure is Sunday, December 5, 2021.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)




(*
  Task 1: Pascal's triangle

             1
            1 1
           1 2 1
          1 3 3 1
         1 4 6 4 1
        ...........
       .............
      ............... 
  

  Define the function

    next : int list -> int list

  that, given a row of the triangle, computes the next row. The
  function List.windowed may be useful here.


  Define the sequence

    triangle : int list seq

  which consists of the rows of Pascal's triangle (represented as int
  list). Do not use sequence expressions. Define this using
  Seq.unfold.


  Define the function

    evens : int -> int list

  so that

    evens n

  evaluates to a list (of length n) consisting of the sums of elements
  in the first n rows of Pascal's triangle that have an even number of
  elements.

*)

let next (arr:int list) : int list =
  if arr |> List.isEmpty then [1]
  else (arr |> List.windowed 2 |> List.fold (fun acc el -> (el.[0] + el.[1])::acc) [1] |> List.rev) @ [1]

// next([3;2;3])


(*
  Task 2

  Define the function

    generate : 'a list -> ('a list -> 'a) -> 'a seq

  so that

    generate xs f

  evaluates to a sequence consisting of the elements in xs followed by
  elements computed by the function f.

  More precisely, if List.length xs = n, then s_i (the i-th element in
  the sequence) is

  * the i-th element of the list xs   if i < n
  * f [s_{i - n}; ... ; s_{i - 1}]     otherwise

  Note that f must be applied to lists of same length as xs.

  You may assume that xs is not empty.

  Define this using sequence expressions.

  Make sure that the calculation of an element of the sequence uses
  the function f at most once.

  The function Seq.cache may be useful here.

*)
// let printSeq seq1 = Seq.iter (printf "%A ") seq1

// let cacheObserver (input:seq<'a>) : seq<'a> =
//   input |> Seq.map (fun x -> printfn "Cached"; x) |> Seq.cache

// let sequence = seq { 1 .. 10 }

// cacheObserver(sequence) |> printSeq


// let seqNumbers = [ 1.0; 1.5; 2.0; 1.5; 1.0; 1.5 ] :> seq<float>
// let seqWindows = Seq.windowed 3 seqNumbers
// let seqMovingAverage = Seq.map Array.average seqWindows

let generate (input: 'a list) (f: 'a list -> 'a): 'a seq = 
  let rec innerGenerate (list: 'a list): 'a seq = 
    let next = f list
    seq {
      next
      yield! (innerGenerate (list.Tail @ [next]))
    }
  seq {
    yield! input
    yield! innerGenerate input
  }


// let seqNum = [ 1.0; 1.5; 2.0]  
// printfn "%A" (generate seqNum List.sum)

(*
  Task 3: Longest common subsequence
  
  We have two arrays, xs and ys, and we wish to find the length of the
  longest common subsequence in these two arrays.
  
  Example:
  
  - xs = [| 1; 2; 3; 4 |]
  - ys = [| 5; 1; 6; 4 |]
  
  Length of the longest common subsequence is 2.
  
  This can be solved using dynamic programming.
  
  Let D be a two-dimensional array that holds the results of the
  subproblems:
  - D[i, j] is the length of the lcs of xs[0 .. i - 1] and ys[0 .. j - 1].
  
  Solving the subproblems:
  - if xs[i - 1] = ys[j - 1] then we follow one subproblem (shorten both sequences):
      D[i, j] = D[i - 1, j - 1] + 1
  
  - otherwise we take the maximum of two subproblems:
      D[i, j] = max D[i - 1, j] D[i, j - 1]
  
  - base cases:
      D[i, 0] = D[0, j] = 0
  
  
  Observation: it is not necessary to fill the entire table D to
  calculate D[i, j].
  
  If we decide to fill only those parts of the table that are necessary
  to compute D[i, j], then we need to be careful to not use the values
  in the unfilled parts in our calculation.
  
  However, we can use lazy values instead and let the runtime figure out
  which entries in the table and in which order need to be calculated.
  
  Define the function
  
    lcs : ((int * int) -> unit) -> 'a [] -> 'a [] -> Lazy<int> [,]
  
  so that
  
    lcs m xs ys
  
  evaluates to the table D for xs and ys except that the entries in the
  table are lazy. An entry in the table is computed only when we ask for
  the value (of the Lazy<int>) or the computation of another entry
  requires the value of this entry.
  
  The function m must be applied to (i, j) when the entry D[i, j] is
  actually computed. For example, you can use printfn as m to make the
  order of calculations visible.

*)

// let rec nItems n item = 
//   seq {
//     match n with
//     | n when n > 0 -> yield item; yield! nItems (n - 1) item
//     | _ -> ()
//   }

// nItems (2) (nItems (3) 0 |> Seq.toArray)

let lcs (p : ((int * int) -> unit)) (a1 :'a []) (a2: 'a []) : Lazy<int>[,] =
  let table = Array2D.create<Lazy<int>> (a1.Length+1) (a2.Length+1) (lazy 0)
  table.[*, 0] <- [|for _ in 1..(a1.Length+1) do lazy ( p (a1.Length, 0); 0)|]
  table.[0, *] <- [|for _ in 1..(a2.Length+1) do lazy ( p (0,a2.Length); 0)|]

  let a = [0..(a1.Length-1)] |> List.map (fun x ->  (a1.[x], x))
  let b = [0..(a2.Length-1)] |> List.map (fun x ->  (a2.[x], x))
  List.fold(fun _ (v,x)-> 
    List.fold (fun _ (v1,y) ->
      match compare v v1 with
      | 0 -> table.[x+1, y+1] <- lazy (
          p (x+1, y+1);
         (table.[x, y].Value + 1)
        )
      | _ -> table.[x+1, y+1] <- lazy (
          p (x+1, y+1);
          (max table.[x+1, y].Value table.[x, y+1].Value)
        )
    ) () b
  ) () a
  table



// let xs = [| 1; 2; 3; 4 |]
// let ys = [| 5; 1; 6; 4 |]
// lcs (fun (x,y) -> printfn "%A" x) xs ys
// // Array2D.create 5 5 (lazy 0)
// xs |> Array.mapi(fun i item -> printfn "%A %A" i item)
// let ys = ["d";"r";"t"]
// let ys = ["d";"r";"t"]
// let a = [0..(ys.Length-1)] |> List.map (fun x -> ys.[x], x+1)


// List.fold(fun _ (x,y)-> 
//       match compare x x with
//       | 0 -> 1
//       | _ -> 0
// ) a


// let ys = [5;1;6;4]
// let a = [0..(ys.Length-1)] |> List.map (fun x -> ys.[x], x)
// a
// let d = Seq.empty
// let d = Seq.append d [9]
// d

// let ys = [5;1;6;4]
// let xs = [5;2;6;4]   


(*
  Task 4:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    • the definition of a builder that lets you express reader computations
      using computation expressions

    • the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    • the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> Map<string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values.
  
  NB! Use computation expressions for reader computations in your implementation.
  
  Note that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.

  The expressions are a simplified subset based on
  Section 18.2.1 of the F# 4.1 specification:
  https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf

*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

let runReader = (<|)

type Expr =
  | Const  of int          // constant
  | Ident  of string       // identifier
  | Neg    of Expr         // unary negation, e.g. -1
  | Sum    of Expr * Expr  // sum 
  | Diff   of Expr * Expr  // difference
  | Prod   of Expr * Expr  // product
  | Div    of Expr * Expr  // division
  | DivRem of Expr * Expr  // division remainder as in 1 % 2 = 1
  | Let    of string * Expr * Expr // let expression, the string is the identifier.


let rec eval (e:Expr) : (Map<string, int> -> int) =
  reader {
      match e with
      | Const x -> return x
      | Ident s ->
          let! env = ask
          return env.Item s
      | Neg t ->
          let! n = eval t
          return -n
      | Sum (a,b) ->
          let! x = eval a
          let! y = eval b
          return x + y
      | Diff (a,b) ->
          let! x = eval a
          let! y = eval b
          return x - y
      | Prod (a,b) ->
          let! x = eval a
          let! y = eval b
          return x * y
      | Div (a,b) ->
          let! x = eval a
          let! y = eval b
          return x / y
      | DivRem (a,b) ->
          let! x = eval a
          let! y = eval b
          return x % y
      | Let (z,a,b) -> 
          let! env = ask
          let! x = eval a
          let env2 = env.Add(z, x)
          let y = eval b
          return (runReader y env2)
  }

// //Example:
// //keeping in mind the expression: let a = 5 in (a + 1) * 6
// let expr = Let ("a",Const 5, Prod(Sum(Ident("a"),Const 1),Const 6))
// eval expr Map.empty<string,int>
// should return 36     
