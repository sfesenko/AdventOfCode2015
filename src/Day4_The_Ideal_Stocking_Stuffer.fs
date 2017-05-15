// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Day4

open System
open System.Text
open System.Security.Cryptography

let MD5 = new MD5CryptoServiceProvider()

let md5 (s : string) = 
  Encoding.UTF8.GetBytes s
  |> MD5.ComputeHash
  |> BitConverter.ToString
  |> (fun s -> s.Replace("-", ""))

let check5 (s:string) = s.StartsWith("00000")
 
let input = "iwrupvqb"

let first = 
    Seq.initInfinite (id)
    |> Seq.find (string >> (+) input >> md5 >> check5)
    |> printfn "%A"

let second =
    Seq.initInfinite (id)
        |> Seq.find (string >> (+) input >> md5 >> (fun s -> s.StartsWith("000000")))
        |> printfn "%A"