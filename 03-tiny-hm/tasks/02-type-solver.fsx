// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  // TODO: Return true of type 'ty' contains variable 'vcheck'
  match ty with 
  | TyBool -> false
  | TyNumber -> false
  | TyVariable vv -> vcheck = vv
  | TyList inner -> occursCheck vcheck inner

let rec substType (subst:Map<string, Type>) ty = 
  // TODO: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with 
  | TyBool
  | TyNumber -> ty
  | TyVariable vv -> if subst.ContainsKey vv then subst.[vv] else ty
  | TyList inner -> TyList( substType subst inner )

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
  List.map(fun (tyFirst, tySecond) -> 
    (substType subst tyFirst, substType subst tySecond)) cs
 

let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList l1, TyList l2)::cs -> solve ((l1, l2)::cs)
  | (TyNumber, TyBool)::cs | (TyBool, TyNumber)::cs ->
    failwith "cannot be solved"
  | (TyList _, TyNumber)::cs | (TyNumber, TyList _)::cs
  | (TyList _, TyBool)::cs | (TyBool, TyList _)::cs ->
    failwith "cannot be solved"
  | (n, TyVariable v)::cs | (TyVariable v, n)::cs ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substConstrs (Map.ofList [v, n]) cs
      let subst = solve constraints
      let n = substType (Map.ofList subst) n
      (v, n)::subst


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
