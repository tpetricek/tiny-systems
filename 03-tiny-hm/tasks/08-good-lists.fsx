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
  | Recursive of string * Expression * Expression
  | Unit 
  // NOTE: To keep things simpler, we add special expressions 
  // for list construction and pattern matching on lists.
  | ListCase of bool * Expression
  | ListMatch of Expression * string * Expression * Expression
  
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  | TyTuple of Type * Type
  | TyUnion of Type * Type
  | TyUnit

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  failwith "implemented in steps 2 to 7"

let rec substType (subst:Map<_, _>) t1 = 
  failwith "implemented in steps 2 to 7"

let substConstrs subst cs = 
  failwith "implemented in step 2"
 
let rec solve constraints =
  failwith "implemented in steps 2 to 7"


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

  | Unit -> failwith "implemented in step 7"
  | Recursive(v, e1, e2) -> failwith "implemented in step 7"
  
  | ListMatch(e, v, e1, e2) -> 
      // TODO: Type of 'e' ('tylist') needs to be a list of elements ('tyel').
      // In 'e1', the type of the variable 'v' is then a tuple 'tyel * tylist'.
      // In 'e2', the type of the variable 'v' is just 'unit'.
      // To express this, you will need a new type variable for 'tyel'.
      failwith "not implemented"

  | ListCase(true, Tuple(ehd, etl)) -> 
      // TODO: If type of 'ehd' is 'tyel' and type of 'etl' is 'tylist'
      // then we need a constraint 'tylist = list<tyel>'.
      failwith "not implemented"

  | ListCase(false, Unit) -> 
      // TODO: The type of '[]' is a list of some type (needs a type variable)
      failwith "not implemented"

  | ListCase _ ->
      // TODO: For simplicity, we here restrict the syntax of list constructs.
      // In general, this is not needed, but it makes the task easier...
      failwith "unsupported list syntax"

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// NOTE: The following is modified from task 7 to use
// ListCase and ListMatch instead of normal Case and Match.
// It should all type check as expected now!

let rec makeListExpr l = 
  match l with
  | x::xs -> ListCase(true, Tuple(x, makeListExpr xs))
  | [] -> ListCase(false, Unit)

makeListExpr [ for i in 1 .. 5 -> Constant i ]
|> infer 

Recursive("map",
  Lambda("f", Lambda("l", 
    ListMatch(
      Variable("l"), "x",
      ListCase(true, Tuple(
        Application(Variable "f", TupleGet(true, Variable "x")),
        Application(Application(Variable "map", Variable "f"), 
          TupleGet(false, Variable "x"))
      )),
      ListCase(false, Unit)
    )
  )),
  Variable("map"))
|> infer 
