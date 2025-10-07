open Browser
open Demo.Html

type State = 
  { Items : string list 
    Input : string }

type Event = 
  | Add
  | Input of string

let initial = 
  { Items = [ "Introduce F#"; "Learn about Tiny Systems"] 
    Input = "" }

let update state event = 
  match event with 
  | Input inp -> { Items = state.Items; Input = inp }
  | Add -> { Items = state.Items @ [state.Input]; Input = "" }

let render trigger state =
  h?div [] [ 
    h?h1 [] [ text "Tiny TODO list" ] 
    h?ul [] [
      for it in state.Items ->
        h?li [] [ text it ]
    ]
    h?input ["type" => "text"; "change" =!> fun el _ ->
      trigger(Input((el :?> Types.HTMLInputElement).value)) ] []
    h?button ["click" =!> fun _ _ -> trigger Add] [text "Add"]
  ]

createVirtualDomApp "root" initial render update