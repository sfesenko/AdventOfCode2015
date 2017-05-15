// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Day3

open Ridos

let nav list c =
    let x,y = match list with
              | [] -> (0,0)
              | _ -> List.head list
    let p1 = match c with
              | '>' -> (x+1, y)
              | 'v' -> (x, y-1)
              | '<' -> (x-1,y)
              | '^' -> (x, y+1)
              | _ -> (x,y)
    p1 :: list
 
let nav2 (list1, list2) c =
    let list = nav list1 c
    list2, list

let count2 (l1,l2) =
    Seq.append l1 l2
    |> set
    |> Set.count

let first file =
    file
    |> readLine
    |> Seq.fold nav [(0,0)]
    |> set
    |> Set.count |> printfn "%d"

let second vv =
    vv
    |> readLine
    |> Seq.fold nav2 ([0,0],[])
    |> count2
    |> printfn "%d" 

