module Day6

type Region = (int * int) * (int * int)

type Action = 
 | TurnOff of Region
 | TurnOn of Region
 | Toggle of Region

let parsec (line:string) = 
    let cv x1 y1 x2 y2 = ((int x1, int y1),(int x2, int y2))
    match line.Split([|' ';','|]) with
    | [|"turn"; "off";a;b;"through";c;d|] -> TurnOff <| cv a b c d
    | [|"turn"; "on";a;b;"through";c;d|] -> TurnOn <| cv a b c d
    | [|"toggle";a;b;"through";c;d|] -> Toggle <| cv a b c d
    | _ -> failwith "fuck you!"

// it's possible to replace turnOn/turnOff by applyAction, but it works a bit slower..
let turnOn (array : bool[,]) ((x1,y1),(x2,y2)) =
    array.[x1..x2,y1..y2] <- Array2D.create (x2-x1+1) (y2-y1+1) true
    array

let turnOff (array : bool[,]) act =
    let ((x1,y1),(x2,y2)) = act
    array.[x1..x2,y1..y2] <- Array2D.create (x2-x1+1) (y2-y1+1) false
    array

let applyAction (array : 'A[,]) reg f =
    let (x1,y1),(x2,y2) = reg
    let mapped = array.[x1..x2,y1..y2] |> Array2D.map f
    array.[x1..x2,y1..y2] <- mapped
    array

let fold array  =  function
    | TurnOff reg -> turnOff array reg
    | TurnOn reg -> turnOn array reg
    | Toggle reg -> applyAction array reg (not)

let secondFold array = function
    | TurnOff reg -> applyAction array reg <| fun x -> max 0 (x-1)
    | TurnOn reg -> applyAction array reg ((+) 1)
    | Toggle reg -> applyAction array reg ((+) 2)

let first file = 
    Ridos.readLines file
    |> Seq.map parsec
    |> Seq.fold fold (Array2D.create 1000 1000 false)
    |> Seq.cast<bool>
    |> Seq.sumBy (function | true -> 1 | false -> 0)
    |> printfn "> Солнышки посчитанны: %A"

let secand file =
    Ridos.readLines file
    |> Seq.map parsec
    |> Seq.fold secondFold (Array2D.create 1000 1000 0)
    |> Seq.cast<int>
    |> Seq.sum
    |> printfn "> Солнышки пересчитанны: %A"
