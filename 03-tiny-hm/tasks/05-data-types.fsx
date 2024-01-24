// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with 
  | TyBool -> false
  | TyNumber -> false
  | TyVariable vv -> vcheck = vv
  | TyList inner -> occursCheck vcheck inner
  | TyFunction (t1, t2) -> (occursCheck vcheck t1) || (occursCheck vcheck t2)
  | TyTuple (t1, t2) -> (occursCheck vcheck t1) || (occursCheck vcheck t2)


let rec substType (subst:Map<_, _>) ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with 
  | TyBool
  | TyNumber -> ty
  | TyVariable vv -> if subst.ContainsKey vv then subst.[vv] else ty
  | TyList inner -> TyList( substType subst inner )
  | TyFunction (t1, t2) -> TyFunction((substType subst t1), (substType subst t2))
  | TyTuple (t1, t2) -> TyTuple((substType subst t1), (substType subst t2))

let substConstrs subst cs = 
  List.map(fun (tyFirst, tySecond) -> 
    (substType subst tyFirst, substType subst tySecond)) cs
 
let rec solve constraints =
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match constraints with 
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
  | (TyFunction(ta1, tb1), TyFunction(ta2, tb2))::cs -> solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (TyFunction(f, g), _)::cs | (_, TyFunction(f, g))::cs -> 
    failwith "cannot be solved"
  | (TyTuple(ta1, tb1), TyTuple(ta2, tb2))::cs -> solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (TyTuple(a, b), _)::cs | (_, TyTuple(a, b))::cs -> 
    failwith "cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  
  | Binary("*", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary("=", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

  | Binary(op, _, _) ->
      failwithf "Binary operator '%s' not supported." op

  | Variable v -> 
      if not (ctx.ContainsKey v) then failwith "unknown variable"
      let vType = ctx.[v]

      vType, []

  | If(econd, etrue, efalse) ->
      let t1, s1 = generate ctx econd
      let t2, s2 = generate ctx etrue
      let t3, s3 = generate ctx efalse

      t2, s1 @ s2 @ s3 @ [t1, TyBool; t2, t3]

  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let e2ctx = ctx.Add(v, t1)
      let t2, s2 = generate e2ctx e2

      t2, s1 @ s2
  
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let ctxWithArg = ctx.Add(v, targ)

      let t1, s1 = generate ctxWithArg e
      let resultType = TyFunction(targ, t1)

      resultType, s1

  | Application(e1, e2) -> 
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      let tret = newTyVariable()

      tret, s1 @ s2 @ [t1, TyFunction(t2, tret)]

  | Tuple(e1, e2) ->
      // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2

      TyTuple(t1, t2), s1 @ s2

  | TupleGet(b, e) ->
      // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
      // We need to generate two new type variables and a constraint.
      let tTuple, innerConstraints = generate ctx e

      let tLeft = newTyVariable()
      let tRight = newTyVariable()

      let constraints = innerConstraints @ [tTuple, TyTuple(tLeft, tRight)]
      let returnType = if b then tLeft else tRight

      returnType, constraints

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
