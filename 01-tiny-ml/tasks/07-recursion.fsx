// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
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
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: A recursive definition. You can think of 
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'. 
  | Recursive of string * Expression * Expression

and VariableContext = 
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
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
      | Some res ->
          // NOTE: As 'res' is now 'Lazy<Value>' we need to get its value here.
          res.Value
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

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 10000)
  )
evaluate Map.empty er
