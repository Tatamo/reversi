module Reversi

type Color =
    | Black
    | White

let swap color =
    match color with
    | Black -> White
    | White -> Black

type Disk = Color option

type Board = Disk [,]

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
let rec extractLine (board: Board) (x, y) dir =
    if isInBoard (x, y) then
        match dir with
        | (dx, dy) -> board.[x, y] :: extractLine board (x + dx, y + dy) dir
    else
        []

let rec _checkFlippableLine disks color =
    match disks with
    | Some(c) :: remains ->
        if c = color then true else _checkFlippableLine remains color
    | None :: remains -> false
    | [] -> false

let checkFlippableLine disks color =
    match disks with
    | Some(c) :: remains ->
        if c = color then false else _checkFlippableLine remains color
    | None :: remains -> false
    | [] -> false

let checkPlacable (board: Board) color (x, y) =
    if not (isInBoard (x, y)) then
        false
    else
        match board.[x, y] with
        | Some(_) -> false
        | None ->
            directions
            |> Seq.exists (fun (dx, dy) -> checkFlippableLine (extractLine board (x + dx, y + dy) (dx, dy)) color)

type GameStatus =
    | InGame of Color
    | GameEnd of Color

type Game = Board * GameStatus

let initGame: Game = initBoard, InGame Black
