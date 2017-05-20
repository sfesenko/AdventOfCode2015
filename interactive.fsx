
type tt = | Qw

let parsec (line:string) = 
 match line.Split([|' ';','|]) with
 | [|"turn"; "off";a;b;"through";c;d|] -> TurnOff,(a,b),(c,d)
 | [|"turn"; "on";a;b;"through";c;d|] -> TurnOn,(a,b),(c,d)
 | [|"toggle";a;b;"through";c;d|] -> Toggle,(a,b),(c,d)
 | _ -> failwith "fuck you!"


"day6_input.txt"
|> Ridos.readLines
|> Seq.map parsec
|> printfn ">%A"

