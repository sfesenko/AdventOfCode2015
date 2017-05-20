module Day6

type Action =  | TurnOff  | TurnOn | Toggle

let parsec (line:string) = 
 match line.Split([|' ';','|]) with
 | [|"turn"; "off";a;b;"through";c;d|] -> TurnOff,(a,b),(c,d)
 | [|"turn"; "on";a;b;"through";c;d|] -> TurnOn,(a,b),(c,d)
 | [|"toggle";a;b;"through";c;d|] -> Toggle,(a,b),(c,d)
 | _ -> failwith "fuck you!"

let test11 file = 
    Ridos.readLines file
    |> Seq.map parsec
    |> printfn ">%A"
