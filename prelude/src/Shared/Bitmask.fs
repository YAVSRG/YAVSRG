namespace Prelude

type Bitmask = uint16

module Bitmask =

    let empty = 0us

    let rec count (x: Bitmask) : int =
        match x with
        | 0us -> 0
        | n -> (if (n &&& 1us) = 1us then 1 else 0) + (count (n >>> 1))

    let has_key (k: int) (x: Bitmask) = (1us <<< k) &&& x > 0us
    let set_key (k: int) (x: Bitmask) = (1us <<< k) ||| x
    let unset_key (k: int) (x: Bitmask) = ~~~(1us <<< k) &&& x
    let toggle_key (k: int) (x: Bitmask) = (1us <<< k) ^^^ x

    let rec toSeq (x: Bitmask) : int seq =
        seq {
            for i = 0 to 15 do
                if (has_key i x) then
                    yield i
        }

    let ofSeq (l: int seq) : Bitmask =
        let mutable bm: Bitmask = 0us
        Seq.iter (fun k -> bm <- set_key k bm) l
        bm