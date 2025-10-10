// ----------------------------------------------------------------------------
// 01 - Simple numerical evaluator as the starting point
// ----------------------------------------------------------------------------

// Values (we only have numbers), expressions (we have constants,
// binary operators and variables) and variable contexts)

type Value = 
  | ValNum of int 

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string

type VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator - a recursive function taking the variable context and
// an expression to be evaluated; returns 'Value' or fails.
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
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Basic arithmentic: (1*2) + (20*2)
let eba1 = 
  Binary("+",
    Binary("*", Constant(1), Constant(2)),
    Binary("*", Constant(20), Constant(2)))

evaluate Map.empty eba1

// Basic artihmetic with variables: x + (x*20)
let eba2 = 
  Binary("+", Variable("x"), 
    Binary("*", Variable("x"), Constant(20)))

let ctx1 = Map.ofList ["x", ValNum 4]
evaluate ctx1 eba2
let ctx2 = Map.ofList ["x", ValNum 2]
evaluate ctx2 eba2