open System

let rnd = System.Random()

let rec star () = 
  Console.CursorLeft <- rnd.Next(Console.WindowWidth)
  Console.CursorTop <- rnd.Next(Console.WindowHeight)
  Console.Write("*")
  white 10

and white n = 
  Console.CursorLeft <- rnd.Next(Console.WindowWidth)
  Console.CursorTop <- rnd.Next(Console.WindowHeight)
  Console.Write(" ")
  if n = 0 then star () else white (n-1)

Console.CursorVisible <- false
star()  