// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with 
  | Variable varName when subst.ContainsKey(varName) ->
    subst.[varName]
  | Predicate(pn, tl) -> 
    let substitutedTerms = tl |> List.map(fun t -> substitute subst t)
    Predicate(pn, substitutedTerms)
  | _ -> term

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  subst 
  |> List.map (fun (s, t) -> 
    (s, substitute newSubst t))

let substituteTerms subst (terms:list<Term>) = 
  terms |> List.map (fun t -> substitute subst t)

let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] -> 
      Some(List.Empty)
  | h1::t1, h2::t2 -> 
      let headSubst = unify h1 h2
      match headSubst with
      | Some hs ->
        let hsMap = Map.ofList hs
        let substitutedT1 = substituteTerms hsMap t1
        let substitutedT2 = substituteTerms hsMap t2
        let tailSubst = unifyLists substitutedT1 substitutedT2
        match tailSubst with
        | Some ts ->
          let hsSubstituted = substituteSubst (Map.ofList ts) hs
          Some(hsSubstituted @ ts)
        | None -> None
      | None -> None
  | _ -> 
      None

and unify t1 t2 = 
  match t1, t2 with 
  | Atom a1, Atom a2 when a1=a2 -> 
      // * For matching atoms, return empty substitution
      Some([])
  | Predicate (p1, ts1), Predicate (p2, ts2) when p1=p2 ->
      // * For matching predicates, return the result of 'unifyLists'
      unifyLists ts1 ts2
  | Variable varName, t | t, Variable varName ->
      // * For variable and any term, return a new substitution
      Some([ varName, t ])
  | _ ->
      // * For anything else, return None (failed to unify)
      None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with
  | Atom a -> []
  | Variable varName -> [ varName ]
  | Predicate (p, terms) -> 
    terms |> List.collect(fun t -> freeVariables t)

let withFreshVariables (clause:Clause) : Clause =
  let headVariables = freeVariables clause.Head
  let bodyVariables = clause.Body |> List.collect(fun t -> freeVariables t)
  let distinctVars = (headVariables @ bodyVariables) |> List.distinct

  let suffix = nextNumber().ToString()
  let subst = distinctVars 
            |> List.map(fun vn -> (vn, Variable(vn + suffix))) 
            |> Map.ofList

  let headSubstituted = substitute subst clause.Head
  let bodySubstituted = substituteTerms subst clause.Body

  rule headSubstituted bodySubstituted

let query (program:list<Clause>) (query:Term) =
  // Return all clauses from 'program' whose 'Head' can be
  // unified with the specified 'query' and return the resulting
  // substitutions. Before unifying, rename variables in the program
  // rule using 'withFreshVariables'. You can do this using 'List.choose' 
  // or by using list comprehension.
  // 
  // The return type of this is a list of tuples consisting of the matching
  // clause and a substitution (list<string * Term>). Calling 'unify'
  // gives you 'option<list<string * Term>>', so you need to pattern match
  // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
  program
  |> List.map (withFreshVariables)
  |> List.map (fun fresh -> (fresh, unify query fresh.Head)) 
  |> List.choose (fun (fresh, subst) -> 
                match subst with
                | Some s -> Some(fresh, s)
                | None -> None)

let rec solve (program: list<Clause>) (subst: list<string*Term>) (goals: list<Term>) (depth: int) = 
  match goals with 
  | g::goals -> 
      // TODO: We need to solve the goal (term) 'g'. To do so, find all 
      // matching clauses in the 'program' using 'query' and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
      // For each possible solution, we need to add the 'clause.Body' to 
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. Then we need to apply the 
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two and call 'solve' recursively with this new substitution
      // to solve the new goals.
      let matches = query program g

      if depth = 0 then
        printfn "bana: %A\n goal: %A" matches g

      for clause, newSubst in matches do
        //printfn "%d===\nclause: %A \n newSubst: %A \n" depth clause newSubst

        let newGoals = goals @ clause.Body
        let newSubstMap = Map.ofList(newSubst)

        let substWithNewSubstApplied = substituteSubst newSubstMap subst
        let goalsSubstituted = substituteTerms newSubstMap newGoals

        //printfn "---BANAN---\n %A ---JAHODA---\n" goalsSubstituted

        solve program (substWithNewSubstApplied @ newSubst) (goalsSubstituted) (depth + 1)

  | [] -> 
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    printfn "%A" subst

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ] 0

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> George, Y -> William, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ] 0

