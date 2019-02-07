open System
open MiniSat

[<EntryPoint>]
let main argv =

  let problem =
    Disj
      (
        set
          [
            Conj (set [ Atom "a"; Atom "b" ])
            Atom "c"
          ]
      )

  // let problem =
  //   Conj (set [ Atom "a"; Neg (Atom "a") ])

  printfn "%s" ("PROBLEM " + (string problem))

  for solution in findSolutions problem do
    printfn "%s" ("SOLUTION " + (string solution))

  0
