namespace Prelude

open System.IO

type [<Struct>] TimeItem<'T> = { Time: Time; Data: 'T }

/// Invariant: Sorted nondecreasing by time
type TimeArray<'T> = TimeItem<'T> array

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