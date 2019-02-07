module Tests

open Xunit
open MiniSat

let private isSatisfiable =
  findSolutions
  >> Seq.isEmpty
  >> not

[<Fact>]
let ``Determines satisfiability correctly`` () =

  let cases =
    [
      (Conj Set.empty, true)

      (Neg (Atom "a"), true)

      (Disj (set [ Atom "a"; Neg (Atom "a") ]), true)

      (Conj (set [ Atom "a"; Neg (Atom "a") ]), false)

      (
        Disj
          (
            set
              [
                Conj (set [ Atom "a"; Atom "b" ])
                Atom "c"
              ]
          ),
        true
      )

      (Disj (set [ Atom "a"; Atom "b"; Atom "c"; Atom "d"; Neg (Atom "a") ]), true)

      (Conj (set [ Atom "a"; Atom "b"; Atom "c"; Atom "d"; Neg (Atom "a") ]), false)

      (Conj (set [ Atom "a"; Conj Set.empty ]), true)

      (
        Conj
          (
            set
              [
                Atom "a";
                Atom "b";
                Disj (set [ Neg (Atom "c"); (Atom "b") ])
                Atom "c";
                Disj (set [ (Atom "a"); Neg (Atom "b") ]);
                Atom "d";
              ]
          ),
        true
      )
    ]

  for (input, expected) in cases do
    printfn "%O" input
    Assert.Equal(expected, isSatisfiable input)

  ()

