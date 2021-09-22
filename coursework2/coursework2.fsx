(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: Ján VLADÁR
  Tallinn University of Technology Student ID
  or Uni-ID: 214328IV
  ------------------------------------

  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework2/coursework2.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Deadline for submitting the solution is September 24 AoE, 2021.
*)

// You are given a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 1. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/
// Please note that you need not read the whole papers, just pick 7 papers that look interesting to you from the database.
let bibliographyData: BibliographyItem list =
  [ ([ "Bairi, Ramakrishna"
       "A, Ambha"
       "Ramakrishnan, Ganesh" ],
     "Learning to Generate Diversified Query Interpretations using Biconvex Optimization",
     (733, 739),
     2013)
    ([ "Thomas, Dennis C."
       "K, Prakash"
       "Harigovind, Gautam"
       "Sen, Debashis" ],
     "Lung Consolidation Detection through Analysis of Vocal Resonance Signals",
     (957, 960),
     2018)
    ([ "Ciccozzi, Federico"
       "Hochgeschwender, Nico"
       "Malavolta, Ivano"
       "Wortmann, Andreas" ],
     "Report on the 2nd International Workshop on Robotics Software Engineering (RoSE'19)",
     (38, 40),
     2019)
    ([ "Jentzsch, Sophie F."
       "Höhn, Sviatlana"
       "Hochgeschwender, Nico" ],
     "Conversational Interfaces for Explainable AI: A Human-Centred Approach",
     (77, 92),
     2019)
    ([ "Hochgeschwender, Nico"
       "Cornelius, Gary"
       "Voos, Holger" ],
     "Arguing Security of Autonomous Robots",
     (7791, 7797),
     2019)
    ([ "Hochgeschwender, Nico" ],
     "Adaptive Deployment of Safety Monitors for Autonomous Systems",
     (346, 357),
      2019)
    ([ "Nakano, Yoshiyuki"
       "Azuma, Katsuhiko"
       "Kamimura, Tadatoshi"
       "Nabata, Eiji" ],
     "The Momentum Robot Arm With A Flexible Beam",
     (1705, 1710),
     1992)]


// 2. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defined using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!

let rec compareLists (firstL: string List) (secondL: string List) : int =
    match (firstL, secondL) with
    | [], [] -> 0
    | first::next, second::next2 when first = second -> compareLists next next2
    | _ -> compare firstL secondL

    (*match (compare first second) with
    | t when t < 0 -> -1
    | 0 -> 0
    | _ -> 1*)

let getAuthors (a, _, _, _) = a
let aa = getAuthors (bibliographyData.Item(0))
let bb = getAuthors (bibliographyData.Item(1))

let k = ["Hochgeschwender, Nico";"Cornelius, Gary"]
let l = ["Hochgeschwender, Nico";"Angor, Gary"]
let m = ["Hochgeschwender, Nico";"Angor, Gary";"Voos, Holger"]

//printfn "%d" (compareLists l m)

//compareLists(getAuthors(b1),getAuthors(b2))

// 3. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 3.

let compareAuthors (a:BibliographyItem) (b:BibliographyItem) : int =
    let getAuthors (a, _, _, _) = a
    compareLists (getAuthors a) (getAuthors b)

//compareAuthors (bibliographyData.Item(0)) (bibliographyData.Item(1))



// 4. Make a function
// compareAuthorsNumPages : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are
// the same then according to the number of pages in the publication.
let compareAuthorsNumPages (a:BibliographyItem) (b:BibliographyItem) : int =
  let getNumPages (_, _, c, _) = c 
  let first = getNumPages(a)
  let second = getNumPages(b)
  if (snd(first)-fst(first)) < (snd(second)-fst(second)) then snd(second)-fst(second) else snd(first)-fst(first)

compareAuthorsNumPages (bibliographyData.Item(0)) (bibliographyData.Item(1))


// 5. Make a function
// sortBibliographyByNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the number of pages in the
// publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.

let sortBibliographyByAuthorNumPages (a:BibliographyItem List) : BibliographyItem List =
  bibliographyData
sortBibliographyByAuthorNumPages(bibliographyData)

// 6. Make a function
// sortBibliographyByAuthorNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and number of pages in the publication in ascending order
// If two items are at the same level in the sort order, their order should be preserved.


// 7. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.
