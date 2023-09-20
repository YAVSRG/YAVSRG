namespace Prelude

[<AutoOpen>]
module Types = 

    type Bitmask = uint16

    module Bitmask = 

        let empty = 0us

        let rec count (x : Bitmask) =
            match x with
            | 0us -> 0
            | n -> (if (n &&& 1us) = 1us then 1 else 0) + (count (n >>> 1))

        let hasBit (k: int) (x: Bitmask) = (1us <<< k) &&& x > 0us
        let setBit k (x: Bitmask) = (1us <<< k) ||| x
        let unsetBit k (x: Bitmask) = ~~~(1us <<< k) &&& x
        let toggleBit k (x: Bitmask) = (1us <<< k) ^^^ x

        let rec toSeq (x: Bitmask) =
            seq {
                for i = 0 to 15 do
                    if (hasBit i x) then yield i
            }

        let ofSeq (l: seq<int>) =
            let mutable bm: Bitmask = 0us
            Seq.iter (fun k -> bm <- setBit k bm) l
            bm

    [<Measure>] type ms = Percyqaz.Common.ms
    [<Measure>] type beat
    [<Measure>] type minute
    type Time = Percyqaz.Common.Time
    
    module Time =

        let inline ofFloat (f: ^T) = float32 f * 1.0f<ms>
        let infinity = infinityf * 1.0f<ms>

        let abs (t: Time) = if t < 0.0f<ms> then -t else t

    [<Struct>]
    type TimeItem<'T> = { Time: Time; Data: 'T }
    type TimeArray<'T> = TimeItem<'T> array // with the invariant that it is sorted ascending by time
    module TimeArray =
        
        let isEmpty (arr: TimeArray<'T>) = arr.Length = 0

        let map (c: 'T -> 'U) (arr: TimeItem<'T> array) = Array.map (fun { Time = time; Data = d } -> { Time = time; Data = c d }) arr

        let last (arr: TimeArray<'T>) = if arr.Length > 0 then Some arr.[arr.Length - 1] else None
        let first (arr: TimeArray<'T>) = if arr.Length > 0 then Some arr.[0] else None

        let scale (scale: float32) (arr: TimeArray<'T>) = arr |> Array.map (fun { Time = time; Data = d } -> { Time = time * scale; Data = d })

        // greatest index i where arr.[i] <= time
        // if no such index because you are before all events, -1
        let find_left time (arr: TimeArray<'T>) =
            let mutable low = 0
            let mutable high = arr.Length
            let mutable mid = -1
            while low < high do
                mid <- (high + low) / 2
                if arr.[mid].Time <= time then low <- mid + 1
                else high <- mid
            high - 1
            
        // least index i where arr.[i] >= time
        let find_right time (arr: TimeArray<'T>) =
            let mutable low = 0
            let mutable high = arr.Length
            let mutable mid = -1
            while low < high do
                mid <- (high + low) / 2
                if arr.[mid].Time < time then low <- mid + 1
                else high <- mid
            low

        let between time1 time2 (arr: TimeArray<'T>) =
            seq {
                let mutable i = find_right time1 arr
                while (i < arr.Length && arr.[i].Time < time2) do
                    yield arr.[i]
                    i <- i + 1
            }

        open System.IO

        let read<'T> (br: BinaryReader) (f: BinaryReader -> 'T) =
            let count = br.ReadInt32()
            let array = Array.zeroCreate<TimeItem<'T>> count
            for i = 0 to count - 1 do
                array.[i] <- { Time = br.ReadSingle() * 1.0f<ms>; Data = f br }
            array

        let write<'T> (arr: TimeArray<'T>) (bw: BinaryWriter) (f: BinaryWriter -> 'T -> unit) =
            bw.Write arr.Length
            for { Time = time; Data = data } in arr do
                bw.Write(time |> float32)
                f bw data