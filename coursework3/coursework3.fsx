(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher-order functions, records

  ------------------------------------
  Name: Ján VLADÁR
  Tallinn University of Technology Student ID: 214328IV
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework3/coursework3.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Your solution should not contain the keywords 'rec' and 'mutable'.

  Deadline for submission is October 8.

  Please check the Moodle page of the course for updates:

  https://moodle.taltech.ee/course/view.php?id=31760

*)

// The task is to model an object that can move on a plane where
// coordinates are specified as integers.

// A position of an object is given as a pair of x and y coordinates as type XY:

type XY = int * int


// The object also has a direction. The possible directions are given as
// an enumeration (a simple version of discriminated unions).

type Dir = N | E | S | W


// If the object moves, then it moves in its current direction.

// Note that moving in a given direction means:
// N - moving in parallel and in the direction of the y axis
// E - moving in parallel and in the direction of the x axis
// S - moving in parallel and in the opposite direction of the y axis
// W - moving in parallel and in the opposite direction of the x axis


//We also keep track of the history of the movement of the object. 

type History = XY list 



// We represent the object by the following record type. In other words,
// we track the current state (position and direction) and the history of
// the object.

type State = { position  : XY
               direction : Dir
               history   : History }



// The possible ways in which we can command the object to move on the
// plane is given by the following discriminated union:

type Command = Step of int       
             | Turn of int       
             | Loop of int * int 

// The meaning of the commands is the following:
// Step n      - the object must move n steps in the current direction;
// Turn n      - the object must turn right n quarter turns, e.g.,
//               when the current direction is N, then Turn 3 indicates
//               that the new direction must be W;
// Loop (m, n) - the object must make a rectangular "loop" with dimensions m and n, e.g.,
//               Loop (m, n) indicates that the object must:
//               - take m steps (in the current direction)
//               - turn right
//               - take n steps secondSteps
//               - turn right
//               - take m steps thirdSteps
//               - turn right
//               - take n steps fourthSteps
//               - turn right


// You are given the function
//
// iterate : int -> ('a -> 'a) -> 'a -> 'a
//
// that will take a function f as an argument and compose it with
// itself n times.
//
// In other words,
//
// iterate 5 f x
//
// applies the function f to x five times, i.e., f (f (f (f (f x))))

let rec iterate (n : int) (f : 'a -> 'a) : 'a -> 'a =
    if n <= 0
    then id
    else f << iterate (n - 1) f


 
// 1. We start by defining some basic functions.

// Define the function
// 
// turn : Dir -> Dir
// 
// that will compute the new direction after making one quarter turn to
// the right. A quarter turn is our unit of measurement for turning.

let turn (d : Dir) : Dir =
  match d with 
  | N -> E
  | E -> S 
  | S -> W 
  | W -> N

turn N


// Define the function
//
// step : Dir -> XY -> XY
//
// that computes the new position after taking
// one step in the given direction.
//
// The distance between the new position and the old position must be 1.

let step (d : Dir) (xy : XY) : XY =
  let x = fst(xy)
  let y = snd(xy)
  match d with 
  | N -> (x,y+1)
  | E -> (x+1,y)
  | S -> (x,y-1)
  | W -> (x-1,y)

step W (3,4)




// 2. We now define how to move the object. Define the function
//
// performCommand : (c : Command) -> (s : State) -> State
//
// that moves the object from state s according to c.
//
// Use the function iterate to perform the move, i.e.,
// Step n, Turn n and Loop (m, n) must be implemented using the functions
// iterate, step and turn.
//
// Note that it is perfectly valid for the object to step a negative
// amount in the current direction. Taking m steps in the current
// direction and then taking -m steps in the current direction should put
// the object back to where it was.
// 
// Similarly, taking m turns followed by -m turns should result in the
// same direction that the object had before.
// 
// You also need to track the history of the object.
// If performing a move makes the position of the object change,
// then you add the previous position to the path. For a Loop, you do this
// every time you reach a corner.
//
// The history is represented so that the most recent item is at the head.

let turnnum (i : int) (c: Dir) : Dir =
  let kok = (i % 4 + 4) % 4
  let ik = iterate kok turn c 
  ik

let historyCheck (position1 : XY)(position2 : XY) (history : History): History =
  if position1=position2 then 
    history
  else 
    position1 :: history

let performCommand (c : Command) (s : State) : State =
  match c with
  | Step n -> 
      if n<0 then
        let position = (iterate (abs n) (step (turnnum 2 s.direction))) s.position
        {position = position; direction=s.direction; history=historyCheck s.position position s.history}
      else
        let position = (iterate n (step s.direction)) s.position
        {position = position; direction=s.direction; history=historyCheck s.position position s.history}
    
  | Turn n -> 
    let direction = iterate ((n % 4 + 4) % 4) turn s.direction
    {position = s.position; direction=direction; history=s.history}

  | Loop(m,n) -> 
    if m=0 && n=0 then
      {position = s.position; direction=s.direction; history=s.history}
    elif m<0 && n<0 then 
      let firstStep = (iterate (abs m) (step (turnnum 2 s.direction))) s.position
      let history0 = historyCheck s.position firstStep s.history 
      let firstTurn = turn s.direction 
      let secondStep = (iterate (abs n) (step (turnnum 2 firstTurn))) firstStep
      let history = historyCheck firstStep secondStep history0
      let secondTurn =  turn  firstTurn
      let thirdStep = (iterate (abs m) (step (turnnum 2 secondTurn))) secondStep
      let history1 = historyCheck secondStep thirdStep history
      let thirdTurn = turn secondTurn
      let fourthStep = (iterate (abs n) (step (turnnum 2 thirdTurn))) thirdStep
      let history2 = historyCheck thirdStep fourthStep history1
      {position = fourthStep; direction=turn thirdTurn; history=history2}
    elif m<0 then 
      let firstStep = (iterate (abs m) (step (turnnum 2 s.direction))) s.position
      let history0 = historyCheck s.position firstStep s.history 
      let firstTurn = turn s.direction
      let secondStep = (iterate (abs n) (step firstTurn)) firstStep
      let history = historyCheck firstStep secondStep history0
      let secondTurn = turn firstTurn
      let thirdStep = (iterate (abs m) (step (turnnum 2 secondTurn))) secondStep
      let history1 = historyCheck secondStep thirdStep history
      let thirdTurn = turn secondTurn
      let fourthStep = (iterate (abs n) (step thirdTurn)) thirdStep
      let history2 = historyCheck thirdStep fourthStep history1
      {position = fourthStep; direction=turn thirdTurn; history=history2}
    elif n<0 then 
      let firstStep = (iterate (abs m) (step  s.direction)) s.position
      let history0 = historyCheck s.position firstStep s.history 
      let firstTurn = turn s.direction
      let secondStep = (iterate (abs n) (step (turnnum 2 firstTurn))) firstStep
      let history = historyCheck firstStep secondStep history0
      let secondTurn = turn firstTurn
      let thirdStep = (iterate (abs m) (step secondTurn)) secondStep
      let history1 = historyCheck secondStep thirdStep history
      let thirdTurn = turn secondTurn
      let fourthStep = (iterate (abs n) (step (turnnum 2 thirdTurn))) thirdStep
      let history2 = historyCheck thirdStep fourthStep history1
      {position = fourthStep; direction=turn thirdTurn; history=history2}
    else
      let firstStep = (iterate m (step s.direction)) s.position
      let firstTurn = turn s.direction
      let history0 = historyCheck s.position firstStep s.history 
      let secondStep = (iterate n (step firstTurn)) firstStep
      let history = historyCheck firstStep secondStep history0
      let secondTurn = turn firstTurn
      let thirdStep = (iterate m (step secondTurn)) secondStep
      let history1 = historyCheck secondStep thirdStep history
      let thirdTurn = turn secondTurn
      let fourthStep = (iterate n (step thirdTurn)) thirdStep
      let history2 = historyCheck thirdStep fourthStep history1
      {position = fourthStep; direction=turn thirdTurn; history=history2}
      
//let S = {position = (0,0); direction=N; history=[]} 
//let c = Step 0
//printfn "%A" (performCommand c S)


// 3. Define the function
//
// performCommands : Command list -> State -> State
//
// that performes the moves according to the given list of commands
// starting from the given initial state.
//
// This must be implemented using a fold over the list of moves. (You
// can choose whether to use fold or foldBack.)

let performCommands (cs : Command list) (s : State) : State =
  List.fold(fun st el -> performCommand el st) s cs




// 4. Define the function
//
// flipSteps : Command list -> Command list
//
// that transforms the given list of commands so that each command Step n
// is replaced with Step -n. Leave other commands the same.
//
// Implement this using List.map
let stepChange(c: Command) : Command =
  match c with
  | Step n -> Step -n
  | _ -> c

let flipSteps (cs : Command list) : Command list =
  List.map(stepChange) cs



// 5. Define the function
//
// flipTurns : Command list -> Command list
//
// that transforms the given list of commands so that each Turn n
// is replaced with Turn m so that performing Turn m makes the object
// face the opposite direction compared to what it would be facing
// after performing Turn n. Leave other commands the same.
//
// Implement this using List.map
let turnChange(c: Command) : Command =
  match c with
  | Turn n -> Turn ((n+2)%4)
  | _ -> c

let flipTurns (cs : Command list) : Command list =
  List.map(turnChange) cs



// 6. Define the function
//
// singleSteps : Command list -> Command list
//
//
// that removes from the given list of commands all those that are of the
// form Loop (m, n) or that are of the form Step k where the absolute
// value of k is not equal to 1.
let singleStepChange (c: Command) : bool =
  match c with
  | Loop(_) -> false
  | Step n when (abs n) <> 1  -> false
  | _ -> true

let singleSteps (cs : Command list) : Command list = 
  cs |> List.filter(fun com -> singleStepChange(com))



// 7. Define the function
//
// unpackLoops : Command list -> Command list
//
// that replaces a command of the form Loop (m, n) with an equivalent
// list of commands consisting only of Step k and Turn l commands. Leave
// other commands the same.
//
// Implement this using List.collect

let looping (c:Command) : Command list =
  match c with
  | Loop(m,n) -> 
    [Step m; Turn 1;Step n; Turn 1;Step m; Turn 1; Step n; Turn 1;]
  | _ -> [c]

let unpackLoops (c: Command list) : Command list =
  c |> List.collect(fun x -> looping(x))

//printfn "%A" (unpackLoops [Turn 2; Step 3; Loop(2,3)])


// 8. Define the function
//
// simplify : Command list -> Command list
//
// that accumulates adjacent Step moves into a single Step
// and adjacent Turn moves into a single Turn. In other words,
// the resulting list of moves must not contain adjacent Step moves
// or adjacent Turn moves. 
//
// The idea is to replace
// - adjacent Step m and Step n with Step (m + n)
// - adjacent Turn m and Turn n with Turn k so that
//   0 <= k <= 3 and
//   after performing Turn k the object is facing the same direction
//   as it would after performing Turn m followed by Turn n.
//
// Implement this using a fold (choose which one) over the input list.
// Do not traverse the input list several times. 
//
// Here is a hint: the type of the accumulator (partial result) you use
// for the fold should allow you to keep track of at least two things:
// - The part of the list that is already simplified.
// - The part that you have seen but have not yet simplified. This is
//   the current sequence of (adjacent) moves of the same kind, i.e.,
//   a sequence of Step or a sequence of Turn moves. If the next move
//   you see is of a different kind, then you first simplify this part,
//   add it to the simplified part and then continue.

let simplifyFold (newC: Command list) (command: Command) =
    printfn "%A" newC
    match command, newC with
    | Step n, Step k :: secondItem -> Step (n+k) :: secondItem 
    | Turn n, Turn k :: secondItem -> Turn (((n+k)%4 + 4 ) % 4) :: secondItem
    | _, newC -> match command with
                  | Turn n -> Turn (((n)%4 + 4 ) % 4) :: newC
                  | _ -> command :: newC

let simplify (clist: Command list): Command list =
  List.fold simplifyFold [] clist |> List.rev

printfn "%A" (simplify [Turn(2);Step(2); Turn(3);Turn(3); Step(2)])