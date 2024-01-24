// ----------------------------------------------------------------------------
// 05 - Add a simple data type - tuples
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  // NOTE: A tuple value consisting of two other values.
  // (Think about why we have 'Value' here but 'Expression'
  // in the case of 'ValClosure' above!)
  | ValTuple of Value * Value

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: 'Tuple' represents two-element tuple constructor
  // and 'TupleGet' the destructor (accessing a value)
  // Use 'true' for #1 element, 'false' for #2. This is not
  // particularly descriptive, but it works OK enough.
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

and VariableContext = 
  Map<string, Value>

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
      | Some res -> res
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
        | x when x > 0 -> evaluate ctx successExpr
        | _ -> evaluate ctx failureExpr
      | _ -> failwith "if operator must be applied to ValNum"
  
  | Lambda(name, e) ->
      // TODO: Evaluate a lambda - create a closure value
      ValClosure(name, e, VariableContext([]))

  | Application(e1, e2) ->
      // TODO: Evaluate a function application. Recursively
      // evaluate 'e1' and 'e2'; 'e1' must evaluate to a closure.
      // You can then evaluate the closure body.
      let value = evaluate ctx e1
      match value with
      | ValClosure(parameterName, expr, varCtx) ->
        let parameterValue = evaluate ctx e2
        let context = varCtx.Add(parameterName, parameterValue)
        evaluate context expr  
      | _ -> failwith "e1 must be a closure"
  
  | Let(v, e1, e2) ->
    // TODO: There are two ways to do this! A nice tricky is to 
    // treat 'let' as a syntactic sugar and transform it to the
    // 'desugared' expression and evaluating that :-)
    let inValue = evaluate ctx e1
    let inContext = ctx.Add(v, inValue)

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

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1= 
  TupleGet(true, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed1

let ed2 = 
  TupleGet(false, 
    Tuple(Binary("*", Constant(2), Constant(21)), 
      Constant(123)))
evaluate Map.empty ed2

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = 
  TupleGet(true, Constant(42))
evaluate Map.empty ed3
