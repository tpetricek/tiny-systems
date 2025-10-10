// --------------------------------------------------------
// Numerical expression evaluator
// --------------------------------------------------------

type Value = 
  | ValNum of int

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string

type VariableContext = 
  Map<string, Value>

let rec evaluate (ctx:VariableContext) e = 
  match e with 
  | Constant c -> ValNum(c)
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 ->
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported operator"    
  | Variable(v) -> ctx.[v]

let e1 = 
  Binary("+", Constant(2), 
    Binary("*", Constant(2), Constant(20)))

evaluate Map.empty e1

let e2 = 
  Binary("+", Variable("x"), 
    Binary("*", Variable("x"), Constant(20)))

let ctx = Map.ofList ["x", ValNum 4]
evaluate ctx e2

// --------------------------------------------------------
// Operators using Map
// --------------------------------------------------------

let ops1 = Map.empty
let ops2 = Map.add "+" (fun a b -> a + b) ops1
let ops3 = Map.add "*" (fun a b -> a * b) ops2

let ops = Map.ofList [ "+", (+); "*", (*)]

ops.["+"] 1 2
ops.["*"] 1 2

// --------------------------------------------------------
// Lazy conditional
// --------------------------------------------------------

let iff cond (tbranch:Lazy<_>) (fbranch:Lazy<_>) = 
  if cond then tbranch else fbranch

let input = 6

iff (input % 3 = 0) 
  (lazy printfn "Divisible by 3!")
  (lazy printfn "Not divisible by 3!")

// --------------------------------------------------------
// Lazy list implementation
// --------------------------------------------------------

type LazyList = 
  | Cons of int * Lazy<LazyList>
  | End

let rec printList count l = 
  if count > 0 then
    match l with 
    | End -> ()
    | Cons(l, ll) ->
        printfn "%d" l
        printList (count-1) ll.Value

let l1 = Cons(1, lazy Cons(2, lazy End))
let rec nums n = Cons(n, lazy nums (n+1))
let rec l2 = Cons(42, lazy l2)

printList 10 l1
printList 100 l2
printList 1000 (nums 0)