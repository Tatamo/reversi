module Reversi

type Color =
  | Black
  | White

let swap color =
  match color with
  | Black -> White
  | White -> Black

let color2Char color =
  match color with
  | Black -> 'x'
  | White -> 'o'

type Disk = Color option

type Board = Disk [,]

type Pos = int * int

let initPlacer x y =
  match (x, y) with
  | (3, 3)
  | (4, 4) -> Some(White)
  | (3, 4)
  | (4, 3) -> Some(Black)
  | _ -> None

let initBoard: Board = Array2D.init 8 8 initPlacer

let directions =
  seq {
    for x in -1 .. 1 do
      for y in -1 .. 1 do
        if not (x = 0 && y = 0) then yield (x, y)
  }

let isInBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

/// pick up disks, starting from (x,y) and go to the specified direction toward the end of board
let rec extractLine (x, y) dir (board: Board) =
  if isInBoard (x, y) then
    match dir with
    | (dx, dy) -> board.[x, y] :: extractLine (x + dx, y + dy) dir board
  else
    []

let rec _checkFlippableLine color disks =
  match disks with
  | Some(c) :: remains ->
      if c = color then true else _checkFlippableLine color remains
  | None :: _ -> false
  | [] -> false

let checkFlippableLine color disks =
  match disks with
  | Some(c) :: remains ->
      if c = color then false else _checkFlippableLine color remains
  | None :: _ -> false
  | [] -> false

let checkPlacable color (x, y) (board: Board) =
  if not (isInBoard (x, y)) then
    false
  else
    match board.[x, y] with
    | Some(_) -> false
    | None ->
        directions
        |> Seq.exists (fun (dx, dy) -> checkFlippableLine color (extractLine (x + dx, y + dy) (dx, dy) board))

let findPlacable color board =
  seq {
    for x in 0 .. 7 do
      for y in 0 .. 7 do
        yield (x, y)
  }
  |> Seq.filter (fun pos -> checkPlacable color pos board)

let count color (board: Board) =
  seq {
    for x in 0 .. 7 do
      for y in 0 .. 7 do
        yield board.[x, y]
  }
  |> Seq.filter (fun disk -> disk = Some(color))
  |> Seq.length

let getWinner board =
  let countBlack = count Black board
  let countWhite = count White board
  if countBlack = countWhite then None
  else if countBlack > countWhite then Some(Black)
  else Some(White)

let place color (x, y) (board: Board) =
  let newBoard = Array2D.copy board
  directions
  |> Seq.map (fun (dx, dy) -> (dx, dy), extractLine (x + dx, y + dy) (dx, dy) board)
  |> Seq.filter (fun (dir, line) -> checkFlippableLine color line)
  |> Seq.iter (fun ((dx, dy), line) ->
       let flipLength = (Seq.findIndex (fun disk -> disk = Some(color))) line
       for distance in 1 .. flipLength do
         Array2D.set newBoard (x + dx * distance) (y + dy * distance) (Some color))
  |> ignore
  Array2D.set newBoard x y (Some color)
  newBoard

let formatBoard (board: Board) =
  let charArray2D =
    board
    |> Array2D.map (fun disk ->
         match disk with
         | Some(color) -> color2Char color
         | None -> '.')
  seq {
    for y in 0 .. 7 do
      yield y.ToString() + (seq {
                              for x in 0 .. 7 -> charArray2D.[x, y]
                            }
                            |> Seq.toArray
                            |> System.String)
  }
  |> Seq.fold (fun acc elm -> acc + "\n" + elm) " 01234567"

type GameStatus =
  | InGame of Color
  | GameEnd of Color option

let formatGameStatus gameStatus =
  match gameStatus with
  | InGame(color) -> sprintf "%s's turn (%s)" (color.ToString()) ((color2Char color).ToString())
  | GameEnd(Some(color)) -> sprintf "Winner: %s" (color.ToString())
  | GameEnd(None) -> "Draw"

type Game = Board * GameStatus

let initGame: Game = initBoard, InGame Black
let formatGame (board, status) = formatBoard board + "\n" + formatGameStatus status

type FinalTurnResult =
  { hand: Pos * Color
    win: Color option }

type SuccessTurnResult =
  { hand: Pos * Color
    nextColor: Color }

type FailedTurnResult =
  | InvalidPosition of Pos
  | GameIsOver

type TurnResult =
  | End of FinalTurnResult
  | Success of SuccessTurnResult
  | Failed of FailedTurnResult

let getMessage turnResult =
  match turnResult with
  | End({hand = hand; win = Some(win)}) -> sprintf "%A: %A  Winner: %A" (snd hand) (fst hand) win
  | End({hand = hand; win = None}) -> sprintf "%A: %A  Draw" (snd hand) (fst hand)
  | Success(succ) -> sprintf "%A: %A  next: %A's turn" (snd succ.hand) (fst succ.hand) succ.nextColor
  | Failed(InvalidPosition(x, y)) -> sprintf "(%d, %d) is not valid place" x y
  | Failed(GameIsOver) -> "Game is Over!"

let play (x, y) game =
  match game with
  | _, GameEnd(_) -> Failed(GameIsOver), game
  | board, InGame(color) ->
      if checkPlacable color (x, y) board then
        let nextBoard = place color (x, y) board

        if (findPlacable (swap color) nextBoard
            |> Seq.isEmpty
            && findPlacable color nextBoard |> Seq.isEmpty) then
          // both player has no place to play
          let winner = getWinner nextBoard
          End
            ({ hand = (x, y), color
               win = winner }), (nextBoard, GameEnd(winner))
        else
          let nextColor =
            if findPlacable (swap color) nextBoard |> Seq.isEmpty
            then color
            else swap color
          Success
            ({ hand = (x, y), color
               nextColor = nextColor }), (nextBoard, InGame(nextColor))
      else
        Failed(InvalidPosition(x, y)), game
