// --------------------------------------------------------
// Parsing strings, numbers and matching
// --------------------------------------------------------

let euraw = 
 [ "Germany,84358845"; "France,68070697"; "Italy,58850717"; 
   "Spain,48059777"; "Poland,36753736"; "Romania,19051562"; 
   "Netherlands,17811291"; "Belgium,11754004"; "Czechia,10827529"; 
   "Sweden,10521556"; "Portugal,10467366"; "Greece,10394055"; 
   "Hungary,9597085"; "Austria,9104772"; "Bulgaria,6447710"; 
   "Denmark,5932654"; "Finland,5563970"; "Slovakia,5428792"; 
   "Ireland,5194336"; "Croatia,3850894"; "Lithuania,2857279"; 
   "Slovenia,2116792"; "Latvia,1883008"; "Estonia,1365884"; 
   "Cyprus,920701"; "Luxembourg,660809"; "Malta,542051" ]

let parseCountry (s:string) = 
  match s.Split(',') with 
  | [| sname; spop |] -> 
      match System.Int32.TryParse(spop) with
      | true, pop -> sname, pop
      | _ -> failwith "parsing failed"
  | _ -> failwith "parsing failed"

let eu = List.map parseCountry euraw

// --------------------------------------------------------
// Functional data processing - without the pipe
// --------------------------------------------------------

let count1 = List.length eu
let pop1 = List.sum (List.map snd eu)
let avg1 = pop1 / count1

List.length (List.filter (fun (_, pop) -> pop > avg1) eu)
List.length (List.filter (fun (_, pop) -> pop < avg1) eu)

List.sortBy snd eu

// --------------------------------------------------------
// Functional data processing - with the pipe
// --------------------------------------------------------

let count2 = eu |> List.length 
let pop2 = eu |> List.map snd |> List.sum
let avg2 = pop2 / count2

eu 
|> List.filter (fun (_, pop) -> pop > avg2) 
|> List.length

eu 
|> List.filter (fun (_, pop) -> pop > avg2) 
|> List.length

let (|>) x f = f x

// --------------------------------------------------------
// Looking for values with TryFind and options
// --------------------------------------------------------

let lookup = "Grea"
let euOpt = 
  eu |> List.tryFind (fun (c, _) -> 
    c.StartsWith(lookup)) 

match euOpt with 
| Some (country, pop) -> 
    printfn "Found %s, population %d" country pop
| None -> 
    printfn "Country not found"
