
module Ridos

let readLine path =
   use sr = new System.IO.StreamReader(path:string)
   sr.ReadLine()

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
