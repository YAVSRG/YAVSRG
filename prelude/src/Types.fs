namespace Prelude

open System

[<AutoOpen>]
module Types =

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

    [<Measure>]
    type ms = Percyqaz.Common.ms

    [<Measure>]
    type rate = Percyqaz.Common.rate

    [<Measure>]
    type beat

    [<Measure>]
    type minute

    [<Measure>]
    type second

    type Time = Percyqaz.Common.Time

    module Time =

        let inline of_number (f: ^T) : Time = float32 f * 1.0f<ms>
        let infinity : Time = infinityf * 1.0f<ms>

    type GameplayTime = float32<ms / rate>

    type Rate = float32<rate>

    module Setting =
        open Percyqaz.Common.Setting
        let rate x = x |> bounded (0.5f<rate>, 3.0f<rate>) |> roundf_uom 2

    [<Struct>]
    type TimeItem<'T> = { Time: Time; Data: 'T }

    type TimeArray<'T> = TimeItem<'T> array // with the invariant that it is sorted ascending by time

    module TimeArray =

        let filter (f: 'T -> bool) (arr: TimeArray<'T>) : TimeArray<'T> =
            Array.filter (fun { Time = _; Data = d } -> f d) arr

        let map (c: 'T -> 'U) (arr: TimeArray<'T>) : TimeArray<'U> =
            Array.map (fun { Time = time; Data = d } -> { Time = time; Data = c d }) arr

        let last (arr: TimeArray<'T>) : TimeItem<'T> option =
            if arr.Length > 0 then Some arr.[arr.Length - 1] else None

        let first (arr: TimeArray<'T>) : TimeItem<'T> option =
            if arr.Length > 0 then Some arr.[0] else None

        let scale (scale: float32<'u>) (arr: TimeArray<'T>) : TimeArray<'T> =
            arr
            |> Array.map (fun { Time = time; Data = d } -> { Time = time * float32 scale; Data = d })

        // todo: find_left, find_right, between are never used. remove? or add tests for them for future use?

        // greatest index i where arr.[i] <= time
        // if no such index because you are before all events, -1
        let find_left (time: Time) (arr: TimeArray<'T>) =
            let mutable low = 0
            let mutable high = arr.Length
            let mutable mid = -1

            while low < high do
                mid <- (high + low) / 2

                if arr.[mid].Time <= time then
                    low <- mid + 1
                else
                    high <- mid

            high - 1

        // least index i where arr.[i] >= time
        let find_right (time: Time) (arr: TimeArray<'T>) =
            let mutable low = 0
            let mutable high = arr.Length
            let mutable mid = -1

            while low < high do
                mid <- (high + low) / 2

                if arr.[mid].Time < time then
                    low <- mid + 1
                else
                    high <- mid

            low

        let between (time1: Time) (time2: Time) (arr: TimeArray<'T>) : TimeItem<'T> seq =
            seq {
                let mutable i = find_right time1 arr

                while (i < arr.Length && arr.[i].Time < time2) do
                    yield arr.[i]
                    i <- i + 1
            }

        open System.IO

        let read<'T> (br: BinaryReader) (f: BinaryReader -> 'T) : TimeArray<'T> =
            let count = br.ReadInt32()
            let array = Array.zeroCreate<TimeItem<'T>> count

            for i = 0 to count - 1 do
                array.[i] <-
                    {
                        Time = br.ReadSingle() * 1.0f<ms>
                        Data = f br
                    }

            array

        let write<'T> (arr: TimeArray<'T>) (bw: BinaryWriter) (f: BinaryWriter -> 'T -> unit) =
            bw.Write arr.Length

            for { Time = time; Data = data } in arr do
                bw.Write(time |> float32)
                f bw data