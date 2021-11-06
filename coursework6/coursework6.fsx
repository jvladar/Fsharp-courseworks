(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name: Ján VLADÁR
  Student ID: javlad
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Sunday, November 7, 2021.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function
  pHoldsForAllSequentialElements : (int -> int -> bool) -> int list -> bool

  that when calling the function

  pHoldsForAllSequentialElements p xs

  will check if predicate p holds for all consequtive elements in the list. If it does,
  the function should return true, else false.

  In the case there is less than two elements in the list it is assumed that
  the predicate does not hold as there are no sequential elements present.

  NB! You are required to implement the function in a tail recursive way using
  explicit recursion.
*)

let rec pHoldsForAllSequentialElements (predicate: int->int->bool) (xs: int list) : bool =
    match xs with
        | x::y::rest when (predicate x y) -> 
        if (rest.IsEmpty) then true 
        else pHoldsForAllSequentialElements predicate (y::rest)
        | _ -> false

//pHoldsForAllSequentialElements (fun x y -> y > x) [2;4;3] 



(*
  Task 2:

  Write a function
  createTwoTuplesOfList : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function.
  In case the list has odd number of elements make the first argument of the
  function be the second element in the tuple. 
  Make sure your implementation uses explicit tail recursion.
*)

let createTwoTuplesOfList (cislo : 'a) (pole: 'a list) =
  let updPole = if (pole.Length % 2 <> 0) then pole @ [cislo] else pole

  let rec createTwoTuplesOfLists2 pole pole2 = 
    match pole with
    | x::y::rest -> createTwoTuplesOfLists2 rest ((x,y)::pole2)
    | _ -> List.rev(pole2) 
  createTwoTuplesOfLists2 updPole []



//createTwoTuplesOfList 1 [2,4,5]

(*
  Task 3:

  Write a function
  createTwoTuplesOfListFold : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of two element tuples of 'a-s that are taken
  sequentially from the list passed as the second argument to the function. In case
  the list has odd number of elements make the first argument of the function be the
  second element in the tuple. 
  Make sure your implementation uses List.fold or List.foldBack appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)

let createTwoTuplesOfListFold (cislo : 'a) (pole: 'a list) =
    let updPole = if (pole.Length % 2 <> 0) then pole @ [cislo] else pole

    fst(List.foldBack (fun e (acc,some) -> 
      match some with
      | None -> (acc,Some e)
      | Some s -> ((e,s):: acc, None)
      )  updPole ([],None))


// createPairsOfListFold 4 [1;2;3;5;6]   
    (*List.fold (fun acc e -> 
      match acc with
      | [] -> [Some e, None]
      | (x1,x2)::tail -> 
        if (x2 <> None) then    // [1] jebni do [[3,4],[5,6]]
          (Some e, None) :: acc
        else                       // [1] jebni do [[2,NULL]]
          (x1 ,Some e) :: tail
      ) [] updPole |> Seq.choose (id)*)

    //|> List.fold (fun acc toupList -> if (List.length toupList = 2) then (toupList.[1], toupList.[0])::acc else acc) [] 
     


// createPairsOfListFold 4 [1;2;3;5;6]


(*
  Task 4:

  Below you find the definition of a type Tr of leaf-labeled trees. Write a
  function
  
  medianAndAverageInTree : int Tree -> int * float
  
  that returns a pair where the first element is the median label in the
  given tree and the second an average across all nodes. The median is the label
  for which the difference in counts of elements to the right and left is
  either 0 or the count of elements to the right is exactly 1 greater than the
  count of elements to the left. The average is the sum of all elements divided with
  the number of elements.
  Use continuation-passing style in your implementation and perform the operations
  in a single pass of the tree.
*)

type 'a Tr =
  | Lf   of 'a
  | Br of 'a Tr * 'a Tr

let rec count b cont = 
  match b with
  | Lf(n) -> cont(1)
  | Br (at, bt) -> count at (fun asum ->count bt (fun bsum -> cont (asum + bsum)))

let rec countt bb contt = 
  match bb with
  | Lf(n) -> contt(n)
  | Br (at, bt) -> countt at (fun asum ->countt bt (fun bsum -> contt (asum + bsum)))

let rec getLastLeaf branch =
  match branch with
  | Lf (a) -> a
  | Br (_, bt) -> getLastLeaf bt

let getLeafNo branch cnt =
  let rec getLeafNoRec branch (cnt, ans) =
    if cnt = 0 then (cnt, ans)
    else
    match branch with
    | Lf (a) -> (cnt-1, a)
    | Br (at, bt) -> getLeafNoRec bt (getLeafNoRec at (cnt, ans)) 
  snd (getLeafNoRec branch (cnt, 0))

let medianAndAverageInTree (tree: int Tr): int * float =
  let rec inner (tr: int Tr) f =
    match tr with
    | Lf l -> f (1, l, [l])
    | Br (tl, tr) -> inner tl (fun (countL, valL, listLeft) -> inner tr (fun (countR, valR, listRight) -> f(countL + countR, valL + valR, listLeft @ listRight)))
  let (count, sum, list) =  inner tree id 
  (list.[(count - 1) / 2], float(sum) / float(count))

(*let medianAndAverageInTree (a : 'a Tr) : int * float =
  let rec inner (a : 'a Tr) (acc) : int list =
    match a with
    | Lf n           -> acc([n])
    | Br(left,right) -> inner left (fun vl -> inner right (fun vr -> acc(vr@vl)))
  let values = inner a (id)
  

  let pele = List.sort values
  let mid = (values.Length - 1) / 2
  let avg = List.sum values
  pele.Item(mid), float(avg)/float(values.Length)*)
  



//medianAndAverageInTree (Br(Br (Lf 8, Lf 5),Lf 9));;


