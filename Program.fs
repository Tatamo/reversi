// Learn more about F# at http://fsharp.org

open System
open Reversi

[<EntryPoint>]
let main argv =
  printfn "%A" (seq {for x in 0..7 do for y in 1..7 do yield (x,y)} |> Seq.filter (fun pos -> checkPlacable initBoard Black pos))
  printfn "%b" (checkPlacable initBoard Black (1, 1))
  printfn "%b" (checkPlacable initBoard Black (2, 3))
  printfn "%b" (checkPlacable initBoard Black (3, 2))
  printfn "%b" (checkPlacable initBoard Black (3, 3))
  0 // return an integer exit code
