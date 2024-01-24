// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
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
  // TODO: Return a list of all variables that appear in 'term'
  // (this may contain duplicates, we will eliminate them below)
  // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
  match term with
  | Atom a -> []
  | Variable varName -> [ varName ]
  | Predicate (p, terms) -> 
    terms |> List.collect(fun t -> freeVariables t)

let withFreshVariables (clause:Clause) : Clause =
  // TODO: Get a list of distinct variables in the clause (using 
  // 'freeVariables' and 'List.distinct'), generate a substitution 
  // that append a number 'n' obtained by 'nextNumber()' to the end
  // of all the variable names, and apply the substitutions to the 
  // head and body of the clause.
  //
  // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
  // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
  //
  // This may not be correct if the user-provided names of variables
  // had numbers in them in a certain format, but that's OK for now! 
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


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  // TODO: Return all clauses from 'program' whose 'Head' can be
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
  |> List.map (fun c -> (c, withFreshVariables c))
  |> List.map (fun (orig, fresh) -> (orig, unify query fresh.Head)) 
  |> List.choose (fun (orig, subst) -> 
                match subst with
                | Some s -> Some(orig, s)
                | None -> None)


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

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

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
