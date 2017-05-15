module Day2

open Ridos

let splitLine (line:string) =
    line.Split 'x' 
      |> Array.map (fun a -> int a)

// summ by line
let sm1 l w h =
    let a = l * w
    let b = w * h
    let c = h * l
    let d = [a;b;c]
    List.sum d * 2 + List.min d

let calcLine1 line = 
   let a = splitLine line
   match a with
   | [|l;w;h|] -> sm1 l w h
   | _ -> failwithf "ops X-("
 
let calcLine2 line =
    let a = splitLine line
    let v1 = Array.reduce (*) a
    let v2 = (Array.sort a).[0.. 1] |> Array.reduce (+) |> (*) 2
    v1 + v2

let first argv = 
    argv
    |> readLines
    |> Seq.sumBy calcLine1
    |> printfn "Risalt1 = %d"

let second src =
    src
    |> readLines
    |> Seq.sumBy calcLine2
    |> printfn "Risult2 = %d"

