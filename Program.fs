open Reversi

let inline input f = System.Console.ReadLine() |> f
let inline inputs f = System.Console.ReadLine().Split() |> Array.map f
let getIntPairFromInput (): int * int = let nums = inputs int in (nums.[0], nums.[1])

[<EntryPoint>]
let main argv =
  initGame
  |> Seq.unfold (fun game ->
    match snd game with
    | InGame(_) -> 
      printfn "%s" (formatGame game)
      let pos = getIntPairFromInput()
      if fst pos = -1 && snd pos = -1 then None else
      Some(play pos game)
    | GameEnd(_) -> None
  ) |> Seq.toList |> printfn "%A"
  0 // return an integer exit code
