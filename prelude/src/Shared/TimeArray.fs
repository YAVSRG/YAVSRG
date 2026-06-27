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