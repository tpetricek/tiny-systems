open System

// Basic inference with numerical code
let x = 10
let add a b = a + b
let addf a b = a + b + 1.0

// Basic let polymorphism
let twice f x = f (f x)

// Let polymorphism with more useful example
let rec map f list =
  match list with 
  | [] -> []
  | x::xs -> (f x)::(map f xs)

map (fun n -> (n, n*n)) [1 .. 10]
map (fun n -> string n) [1 .. 10]


// Value-restriction - this is a polymorphic function...
let len1 l = List.fold (fun st _ -> st + 1) 0 l
len1 [1;2;3]
len1 ["hi"; "hello"]

// .. but if we try to use partial application, it 
// becomes just a value and those cannot be generalized.
let len2 = List.fold (fun st _ -> st + 1) 0
len2 [1;2;3]
len2 ["hi"; "hello"]

// Value-restriction - if we were able to generalize
// values, the following invalid code would type-check!
let store = ref None
store.Value <- Some "hello"

match store.Value with 
| Some n -> n + 1
| _ -> 0

// Occurs check - this creates constraints where we 
// try to unfiy the type 'a with 'a -> 'b (impossible)
let foo f = f f


// Left-to right type inference in F# with .NET objects
let dates = 
  [ DateTime(2020,1,1); DateTime(2021,1,1);
    DateTime(2022,1,1); DateTime(2023,1,1) ]

// ...F# does not know what class 'dt' is
List.map (fun dt -> dt.Ticks) dates
// ...we can either add a type annotation
List.map (fun (dt:DateTime) -> dt.Ticks) dates
// or we can use |> and get the information to the left
dates |> List.map (fun dt -> dt.Ticks) 

