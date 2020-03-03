// Learn more about F# at http://fsharp.org

open System
open Reversi

[<EntryPoint>]
let main argv =
  printfn "%A" (seq {for x in 0..7 do for y in 1..7 do yield (x,y)} |> Seq.filter (fun pos -> checkPlacable Black pos initBoard))
  printfn "%b" (checkPlacable Black (1, 1) initBoard)
  printfn "%b" (checkPlacable Black (2, 3) initBoard)
  printfn "%b" (checkPlacable Black (3, 2) initBoard)
  printfn "%b" (checkPlacable Black (3, 3) initBoard)
  printfn "%s" (initBoard |> place Black (2, 3) |> formatBoard)
  printfn "%s" (initBoard |> place Black (2, 3) |> place White (2, 2) |> formatBoard)
  0 // return an integer exit code
