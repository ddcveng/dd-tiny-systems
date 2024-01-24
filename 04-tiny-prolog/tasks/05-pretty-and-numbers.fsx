// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  // TODO: Write an active pattern to recognize numbers in the form used below.
  // If the term is 'Atom("zero")' return Some(0). 
  // If the term is 'Predicate("succ", [n])' where 'n' is itself
  // a term representing number, return the number value +1. 
  match term with 
  | Atom(s) ->
    match s with
    | "zero" -> Some(0)
    | _ -> None
  | Predicate("succ", predTerms) ->
    match predTerms with
    | [Number n] -> Some(n + 1)
    | _ -> None
  | _ -> None

let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      // TODO: format all arguments recursively using 'formatTerm'
      // You can then concatenate the arguments using 'String.concat'
      let itemsFormatted = items |> List.map formatTerm
      let itemsStr = String.concat ", " itemsFormatted

      p + "(" + itemsStr + ")"

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

let rec solve program subst goals =
  // TODO: When printing the computed substitution 'subst', print
  // the terms nicely using 'formatTerm'. You can use 'for' loop like:
  // 'for var, term in subst do printfn ...'
  match goals with 
  | g::goals -> 
      // We need to solve the goal (term) 'g'. To do so, find all 
      // matching clauses in the 'program' using 'query' and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
      // For each possible solution, we need to add the 'clause.Body' to 
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. Then we need to apply the 
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two and call 'solve' recursively with this new substitution
      // to solve the new goals.
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = goals @ clause.Body
        let newSubstMap = Map.ofList(newSubst)

        let substWithNewSubstApplied = substituteSubst newSubstMap subst
        let goalsSubstituted = substituteTerms newSubstMap newGoals

        solve program (substWithNewSubstApplied @ newSubst) (goalsSubstituted)

  | [] -> 
    // We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    for var, term in subst do
      printfn "%s = %s" var (formatTerm term)

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num(n: int): Term = 
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  match n with
  | 0 -> Atom("zero")
  | _ -> 
    let prevNum: Term = num(n-1)
    Predicate("succ", [prevNum])


// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
