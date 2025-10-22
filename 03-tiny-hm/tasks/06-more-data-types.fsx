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
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  // NOTE: Added two types of expression for working with unions
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  // NOTE: Added type for tuples
  | TyUnion of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
  failwith "not implemented"

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
  failwith "not implemented"

let substConstrs subst cs = 
  failwith "implemented in step 2"
 
let rec solve constraints =
  // TODO: Add case for 'TyUnion' (same as 'TyFunction')
  failwith "not implemented"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> failwith "implemented in step 3"
  | Binary("+", e1, e2) -> failwith "implemented in step 3"
  | Binary("=", e1, e2) -> failwith "implemented in step 3"
  | Binary(op, _, _) -> failwith "implemented in step 3"
  | Variable v -> failwith "implemented in step 3"
  | If(econd, etrue, efalse) -> failwith "implemented in step 3"

  | Let(v, e1, e2) -> failwith "implemented in step 4"
  | Lambda(v, e) -> failwith "implemented in step 4"
  | Application(e1, e2) -> failwith "implemented in step 4"

  | Tuple(e1, e2) -> failwith "implemented in step 5"
  | TupleGet(b, e) -> failwith "implemented in step 5"

  | Match(e, v, e1, e2) ->
      // TODO: As with tuples, we know the type of 'e' is some union,
      // but we do not know what. We need new type variables. When 
      // checking 'e1' and 'e2', add variable 'v' to the context!
      // Also note that the return types of 'e1' and 'e2' have to match.
      failwith "not implemented"

  | Case(b, e) ->
      // TODO: Here, we know the type of 'e' is the type of one of 
      // the cases, but we still need a new type variable for the other.
      failwith "not implemented"
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Both cases are constrained because 'if' returns either one or the other
// * fun x -> if x = 0 then Case1(fun x -> x) else Case2(42)
Lambda("x", 
  If(Binary("=", Variable("x"), Constant(0)),
    Case(true, Lambda("x", Variable("x"))),
    Case(false, Constant(42))
  ))
|> infer

// No constraints to fix the second case type (case<number, 'a> -> number)
// * fun x -> match x with Case1 v -> v + 1 | Case2 _ -> 0 
Lambda("x", Match(Variable("x"), "v", 
  Binary("+", Variable("v"), Constant(1)),
  Constant(0)))
|> infer