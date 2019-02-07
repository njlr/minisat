module MiniSat

type Expr =
| Atom of string
| Conj of Set<Expr>
| Disj of Set<Expr>
| Neg of Expr
  with
    override this.ToString () =
      match this with
      | Atom a -> a
      | Conj xs -> "and(" + (xs |> Seq.map string |> String.concat " ") + ")"
      | Disj xs -> "or(" + (xs |> Seq.map string |> String.concat " ") + ")"
      | Neg x -> "not " + (string x)

type Model = Map<string, bool>

let model =
  Map.empty
  |> Map.add "a" true
  |> Map.add "b" false

let rec satisfiedBy (model : Model) problem =
  match problem with
  | Atom a ->
    model
    |> Map.tryFind a
    |> Option.defaultValue false
  | Neg e -> not (satisfiedBy model e)
  | Conj xs ->
    xs
    |> Seq.forall (satisfiedBy model)
  | Disj xs ->
    xs
    |> Seq.exists (satisfiedBy model)

let rec extractAtoms problem =
  match problem with
  | Atom a -> set [ a ]
  | Neg e -> extractAtoms e
  | Conj xs ->
    xs
    |> Seq.collect extractAtoms
    |> Set.ofSeq
  | Disj xs ->
    xs
    |> Seq.collect extractAtoms
    |> Set.ofSeq

let findSolutions problem =
  let mutable visited = Set.empty

  let rec loop model = seq {
    printfn "%O" model

    if problem |> satisfiedBy model
    then
      yield model
    else
      let atoms = extractAtoms problem

      let unset =
        atoms
        |> Set.filter (fun x -> model |> Map.containsKey x |> not)

      for x in unset do
        for b in [ true; false] do
          if visited |> Set.contains (x, b)
          then
            printfn "%s" ("SKIPPING " + (string (x, b)))
            ()
          else
            visited <- Set.add (x, b) visited
            yield! loop (model |> Map.add x b)
    }

  loop Map.empty