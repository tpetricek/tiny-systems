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
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  // NOTE: Added the unit value and recursive definition
  | Recursive of string * Expression * Expression
  | Unit 

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  | TyUnion of Type * Type
  // NOTE: We need another primitive type for units
  | TyUnit

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
  failwith "not implemented"

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
  failwith "not implemented"

let substConstrs subst cs = 
  failwith "implemented in step 2"
 
let rec solve constraints =
  // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
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

  | Match(e, v, e1, e2) -> failwith "implemented in step 6"
  | Case(b, e) -> failwith "implemented in step 6"

  | Unit -> 
      // NOTE: This is so easy I wrote it for you :-)
      TyUnit, []

  | Recursive(v, e1, e2) ->
      // TODO: This is easier than evaluation. We need a new type variable
      // for the type of the thing we are defining (variable 'v') and add
      // it to the context when checking both 'e1' and 'e2'.
      failwith "not implemented"
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Helper to generate list 1 .. 5 from TinyML tasks
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

// We can type check this, but the type is horrible!
makeListExpr [ for i in 1 .. 5 -> Constant i ]
|> infer 

// Code for the List.map function from TinyML task. This fails to check.
Recursive("map",
  Lambda("f", Lambda("l", 
    Match(
      Variable("l"), "x",
      Case(true, Tuple(
        Application(Variable "f", TupleGet(true, Variable "x")),
        Application(Application(Variable "map", Variable "f"), 
          TupleGet(false, Variable "x"))
      )),
      Case(false, Unit)
    )
  )),
  Variable("map"))
|> infer 
