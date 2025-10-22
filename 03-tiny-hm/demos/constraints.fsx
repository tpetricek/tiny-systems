// Peano representation of numbers (Zero, Succ)
// with variables to be used for constraint solving
type Number =
  | Zero
  | Succ of Number
  | Variable of string

// Check if variable 'v' appears anywhere inside 'n'
// (return true if so - this means we cannot unify)
let rec occursCheck (v:string) (n:Number) = 
  failwith "not implement"

// Replace all occurrences of variable 'v' in the
// number 'n' with the replacement number 'subst'
let rec substite (v:string) (subst:Number) (n:Number) =
  failwith "not implement"

// Substitute 'v' for 'subst' (use 'substitute') in all
// numbers in all the constraints in 'constraints'
let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) = 
  failwith "not implemented"

// Perform all substitutions specified 
// in 'subst' on the number 'n'
let substituteAll (subst:list<string * Number>) (n:Number) =
  failwith "not implemeted"

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
solve 
  [ Succ(Variable "x"), Succ(Zero) ]

// Should faild: S(Z) <> Z
solve 
  [ Succ(Succ(Zero)), Succ(Zero) ]

// Not done: Need to substitute x/Z in S(x)
solve 
  [ Succ(Variable "x"), Succ(Zero)
    Variable "y", Succ(Variable "x") ]

// Not done: Need to substitute z/Z in S(S(z))
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ]

// Not done: Need occurs check
solve
  [ Variable "x", Succ(Variable "x") ]
