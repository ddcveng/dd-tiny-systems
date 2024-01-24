// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
  | ValCase of bool * Value

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
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

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
  | Match(e, v, e1, e2) ->
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!
      let case = evaluate ctx e
      match case with
      | ValCase(which, value) -> 
        let caseCtx = ctx.Add(v, value)
        match which with 
        | true -> evaluate caseCtx e1
        | false -> evaluate caseCtx e2
      | _ -> failwith "not a case >:("

  | Case(b, e) ->
      // TODO: Create a union value.
      ValCase(b, evaluate ctx e)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
