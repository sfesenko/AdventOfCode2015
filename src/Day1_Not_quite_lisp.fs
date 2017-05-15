
// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Day1

open Ridos

let sm c =
    match c with
    | '(' -> +1
    | _ -> -1

let simpleCounter path =
    readLine path
    |> Seq.sumBy sm

let subCounter path =
    readLine path
    |> Seq.scan (fun n x -> 
        match x with
        | '(' -> n+1
        | _ -> n-1) 0
    |> Seq.findIndex (fun n -> n = -1)

let first x =
    simpleCounter x
    |> printfn "Result = %A"

let second x =
    subCounter x
    |> printfn "1st subground on %A position"