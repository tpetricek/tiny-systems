open Browser
open Demo.Html

type State = 
  { Count : int }

type Event = 
  | Increment
  | Decrement
  | Reset 

let initial = 
  { Count = 0 }

let update state evt = 
  match evt with 
  | Increment -> { Count = state.Count + 1 }
  | Decrement -> { Count = state.Count - 1 }
  | Reset -> { Count = 0 }

let render trigger state =
  h?div [] [ 
    h?h1 [] [ text ("Count: " + string state.Count) ]
    h?button [ "click" =!> fun _ _ -> trigger(Increment) ] [ text "Increment" ]
    h?button [ "click" =!> fun _ _ -> trigger(Decrement) ] [ text "Decrement" ]
    h?button [ "click" =!> fun _ _ -> trigger(Reset) ] [ text "Reset" ]
  ]

createVirtualDomApp "root" initial render update