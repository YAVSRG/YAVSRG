module Collatz

type Digit =
    | False
    | True
    | Var of char
    | Xor of Digit * Digit
    | And of Digit * Digit
    override this.ToString() =
        match this with
        | False -> "FALSE"
        | True -> "TRUE"
        | Var c -> c.ToString()
        | Xor (a, b) -> a.ToString()+"."+b.ToString()
        | And (a, b) -> a.ToString()+"^"+b.ToString()

let rec foldDigit f t v x a d : 'a =
    match d with
    | False -> f
    | True -> t
    | Var c -> v c
    | Xor (n, m) -> x (foldDigit f t v x a n) (foldDigit f t v x a m)
    | And (n, m) -> a (foldDigit f t v x a n) (foldDigit f t v x a m)

let simp =
    foldDigit False True Var
        (fun a b ->
            match (a, b) with
            | (False, x) -> x
            | (x, False) -> x
            | (_, _) -> Xor (a, b))
        (fun a b ->
            match (a, b) with
            | (False, x) -> False
            | (x, False) -> False
            | (True, x) -> x
            | (x, True) -> x
            | (_, _) -> And (a, b))

type Number = Digit list //least significant bit first, for many reasons

let numToString num = String.concat ";\n" (List.map (simp >> fun x -> x.ToString()) num)

//1;0;1;1 means 1101

let rec addOne pos d (b : Number) : Number =
    if pos > 0 then 
        match b with
        | [] -> False :: addOne (pos - 1) d b
        | x :: xs -> x :: addOne (pos - 1) d xs
    else
        match b with
        | [] -> [d]
        | x :: xs -> Xor (x,d) :: addOne 0 (And (x,d)) xs

let add (a : Number) (b : Number) : Number =
    let rec f depth x y =
        match x with
        | [] -> y
        | (u::us) -> addOne depth u (f (depth + 1) us y)
    f 0 a b

let main = 
    let n = [Var 'a'; Var 'b'; Var 'c']
    printfn "%A" (numToString (add n (True :: n)))