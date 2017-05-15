module Day5

open Ridos

type Naughty = 
    {vowels:int; doubles:bool; bads:bool}
    member this.ToString = sprintf "%d %A %A" this.vowels this.doubles this.bads

let isGood {vowels=vv;doubles=dd;bads=bb} = 
    vv >= 3 && dd && not bb

let rec check1 n s =
    let {vowels=vv;doubles=dd;bads=bb} = n
    match s with
    | a::b::tail when (List.contains a ['a';'e';'i';'o';'u']) && a = b-> check1 {n with vowels=vv+1;doubles=true} (b::tail )
    | a::b::tail when a = b -> check1 {n with doubles=true} (b::tail)
    | 'a'::'b'::tail -> false
    | 'c'::'d'::tail -> false
    | 'p'::'q'::tail -> false
    | 'x'::'y'::tail -> false
    | a::tail when List.contains a ['a';'e';'i';'o';'u'] -> check1 {n with vowels=vv+1} tail 
    | a::tail -> check1 n tail
    | [] -> isGood n

let check s =
    s  |> Seq.toList |> check1 {vowels=0; doubles=false;bads=false}

let isSuperNice s =
    let hasDoubles =
        let rec hasDoubles2 pairs = function
            | a::b::c::tail -> hasDoubles2 ((a,b)::pairs) (c::tail) || hasDoubles2 ((b,c)::pairs) tail
            | a::b::tail -> hasDoubles2 ((a,b)::pairs) tail
            | a::tail -> hasDoubles2 pairs tail
            | [] -> pairs 
                    |> Seq.countBy id 
                    |> Seq.map snd 
                    |> Seq.exists (fun i -> i > 1)
        Seq.toList >> hasDoubles2 [] 

    let isACDC =
        let rec isACDC2 = function
            | a::b::c::tail when a = c -> true
            | a::tail -> isACDC2 tail
            | [] -> false
        Seq.toList >> isACDC2 

    hasDoubles s && isACDC s

let first file = 
    file
    |> readLines
    |> Seq.countBy check
    |> printfn "%A"

let second file =
    file
    |> readLines
    |> Seq.countBy isSuperNice
    |> printfn "%A"

