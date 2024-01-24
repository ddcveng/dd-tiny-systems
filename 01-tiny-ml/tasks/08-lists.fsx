// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value
  // NOTE: A value that represents "empty value" and is
  // useful as the value for representing the empty list.
  | ValUnit 

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  | Recursive of string * Expression * Expression
  // NOTE: An expression that evaluates to a unit value.
  // This exists in F# too and it is written as '()'
  | Unit 

and VariableContext = 
  Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res.Value
      | _ -> failwith ("unbound variable: " + v)

  | Unary(op, e) ->
      let banan = evaluate ctx e
      match banan with
      | ValNum value ->
          match op with
          | "-" -> ValNum(-value)
          | _ -> failwith "unsupported unary operator"
      | _ -> failwith "unary operator must be applied to ValNum"
  | If(condition: Expression, successExpr, failureExpr) -> 
      let conditionValue = evaluate ctx condition
      match conditionValue with
      | ValNum n ->
        match n with 
        | 1 -> evaluate ctx successExpr
        | _ -> evaluate ctx failureExpr
      | _ -> failwith "if operator must be applied to ValNum"
  
  | Lambda(name, e) ->
      // TODO: Evaluate a lambda - create a closure value
      ValClosure(name, e, ctx)

  | Application(e1, e2) ->
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
      let value = evaluate ctx e1
      match value with
      | ValClosure(parameterName, expr, varCtx) ->
        let parameterValue = evaluate ctx e2
        let context = varCtx.Add(parameterName, Lazy(parameterValue))
        evaluate context expr  
      | _ -> failwith "e1 must be a closure"
  
  | Let(v, e1, e2) ->
    // TODO: There are two ways to do this! A nice tricky is to 
    // treat 'let' as a syntactic sugar and transform it to the
    // 'desugared' expression and evaluating that :-)
    let inValue = evaluate ctx e1
    let inContext = ctx.Add(v, Lazy(inValue))

    evaluate inContext e2
  | Tuple(e1, e2) ->
      // TODO: Construct a tuple value here!
      let value1 = evaluate ctx e1
      let value2 = evaluate ctx e2
      ValTuple(value1, value2)

  | TupleGet(b, e) ->
      // TODO: Access #1 or #2 element of a tuple value.
      // (If the argument is not a tuple, this fails.)
      let value = evaluate ctx e
      match value with
      | ValTuple(first, second) ->
        match b with
        | true -> first
        | false -> second
      | _ -> failwith "not a tuple >:("
  | Match(e, v, e1, e2) ->
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!
      let case = evaluate ctx e
      match case with
      | ValCase(which, value) -> 
        let caseCtx = ctx.Add(v, Lazy(value))
        match which with 
        | true -> evaluate caseCtx e1
        | false -> evaluate caseCtx e2
      | _ -> failwith "not a case >:("

  | Case(b, e) ->
      // TODO: Create a union value.
      ValCase(b, evaluate ctx e)

  | Recursive(v, e1, e2) ->
      // TODO: Implement recursion for 'let rec v = e1 in e2'.
      // (In reality, this will only work if 'e1' is a function
      // but the case can be implemented without assuming that).
      let rec recContext = 
        ctx.Add(v, lazy evaluate recContext e1)
      evaluate recContext e2
  // NOTE: This is so uninteresting I did this for you :-)
  | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2), 
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we 
// do not need to write them by hand!
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1 .. 5 -> Constant i ]
// List.filter in TinyML:
// let rec filter = 

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> Case1(f x#1, (map f) x#2) 
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em = 
  Recursive("map",
    Lambda("f", Lambda("l", // f must be a lambda
      Match(
        Variable("l"), "x", // x is eval(l) | l must be case
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")), // x#1 in f
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x")) // map(f(x#2))
        )),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "map", 
      Lambda("y", Binary("*", Variable "y", Constant 10))), el)
  )
evaluate Map.empty em

let filter = 
  Recursive("filter", 
    Lambda("condition", Lambda("l",
      Match(
        Variable("l"), "x", // TODO: Match doesnt care about type of e1 and e2 -> Case() can be inside if and we get rid of unnecessary nesting
        Case(true, If(Application(Variable "condition", TupleGet(true, Variable "x")),
         Tuple( // if true, keep the current element and recure further
          TupleGet(true, Variable "x"),
          Application(Application(Variable "filter", Variable "condition"), 
            TupleGet(false, Variable "x"))),
         Application(Application(Variable "filter", Variable "condition"), 
            TupleGet(false, Variable "x")))), // if false, just recurse further
        Case(false, Unit)
      )
    )), // this lambda filters the list
    Application(Application(Variable "filter",
      Lambda("y", Binary("+", Variable "y", Constant -2))), el) // this lambda will be the condition
  )
  
evaluate Map.empty filter
// TODfO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> 
//          if f x#1 then Case1(x#1, (map f) x#2) 
//          else (map f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y + (-2)) l
//
let ef = failwith "not implemented"
evaluate Map.empty ef
