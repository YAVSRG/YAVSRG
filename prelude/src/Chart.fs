namespace Prelude.Charts

open System
open System.IO
open System.Linq
open System.Collections.Generic
open System.Security.Cryptography
open Percyqaz.Common
open Percyqaz.Data
open Prelude

type NoteType =
    | NOTHING = 0uy
    | NORMAL = 1uy
    | HOLDHEAD = 2uy
    | HOLDBODY = 3uy
    | HOLDTAIL = 4uy

type NoteRow = NoteType array

module NoteRow =

    let clone = Array.copy

    let create_empty (keycount: int) : NoteRow = Array.create keycount NoteType.NOTHING

    let create_notes (keycount: int) (notes: Bitmask) : NoteRow =
        let nr = create_empty keycount

        for k in Bitmask.toSeq notes do
            nr.[k] <- NoteType.NORMAL

        nr

    let create_ln_bodies (keycount: int) (notes: Bitmask) : NoteRow =
        let nr = create_empty keycount

        for k in Bitmask.toSeq notes do
            nr.[k] <- NoteType.HOLDBODY

        nr

    let is_empty: NoteRow -> bool =
        Array.forall (
            function
            | NoteType.NOTHING
            | NoteType.HOLDBODY -> true
            | _ -> false
        )

    let read (keycount: int) (br: BinaryReader) : NoteRow =
        let row = create_empty keycount
        let columns = br.ReadUInt16()

        for k in Bitmask.toSeq columns do
            row.[k] <-
                match br.ReadByte() with
                | 1uy -> NoteType.NORMAL
                | 2uy -> NoteType.HOLDHEAD
                | 3uy -> NoteType.HOLDBODY
                | 4uy -> NoteType.HOLDTAIL
                | b -> failwithf "unexpected note type in chart data: %i" b

        row

    let write (bw: BinaryWriter) (row: NoteRow) : unit =
        let columns =
            seq {
                for i in 0 .. row.Length - 1 do
                    if row.[i] <> NoteType.NOTHING then
                        yield i
            }

        bw.Write(Bitmask.ofSeq columns)

        for k in columns do
            bw.Write(byte row.[k])

    let pretty_print (row: NoteRow) : string =
        let p =
            function
            | NoteType.NORMAL -> '#'
            | NoteType.HOLDHEAD -> '^'
            | NoteType.HOLDBODY -> '|'
            | NoteType.HOLDTAIL -> 'v'
            | NoteType.NOTHING
            | _ -> ' '

        new string (row |> Array.map p)

type BPM =
    {
        Meter: int<beat>
        MsPerBeat: float32<ms / beat>
    }

type SV = float32

type Chart =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<SV>
    }

    member this.FirstNote : Time = (TimeArray.first this.Notes).Value.Time
    member this.LastNote : Time = (TimeArray.last this.Notes).Value.Time

module Chart =

    let hash (chart: Chart) : string =
        let h = SHA256.Create()
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)

        let offset = chart.FirstNote

        bw.Write chart.Keys

        for r in chart.Notes do
            for nt in r.Data do
                bw.Write(byte nt)

        let mutable speed = 1.0

        for { Time = o; Data = f } in chart.SV do
            let f = float f

            if speed <> f then
                bw.Write((o - offset) * 0.2f |> Convert.ToInt32)
                bw.Write f
                speed <- f

        bw.Write((chart.LastNote - offset) * 0.01f |> float32 |> round |> int)

        BitConverter.ToString(h.ComputeHash(ms.ToArray())).Replace("-", "")

    let check (chart: Chart) : Result<Chart, string> =
        try
            if chart.Notes.Length = 0 then
                failwith "Chart must have notes"

            if chart.BPM.Length = 0 then
                failwith "Chart must have at least one BPM marker"

            if chart.Keys < 3 || chart.Keys > 10 then
                failwithf "Chart must have 3-10 keys (Has %i)" chart.Keys

            let mutable last_time = -Time.infinity

            for { Time = time; Data = { Meter = meter; MsPerBeat = mspb } } in chart.BPM do
                if time < last_time then
                    failwithf "BPM appears before the previous time (%f, %f)" time last_time
                elif not (Single.IsFinite (float32 time)) then
                    failwithf "BPM timestamp is invalid value: %f" time
                elif meter <= 0<beat> then
                    failwithf "BPM meter is non-positive: %i at %f" meter time
                elif mspb < 0.0f<ms / beat> then
                    failwithf "BPM is negative: %f mspb at %f" mspb time
                elif Single.IsNaN (float32 mspb) then
                    failwithf "BPM is invalid value: %f mspb at %f" mspb time

                last_time <- time

            let mutable last_time = -Time.infinity

            for { Time = time; Data = sv } in chart.SV do
                if time < last_time then
                    failwithf "SV appears before the previous time (%f, %f)" time last_time
                elif not (Single.IsFinite (float32 time)) then
                    failwithf "SV timestamp is invalid value: %f" time
                elif Single.IsNaN sv then
                    failwithf "SV is NaN (should use +Infinity instead): %f" time

                last_time <- time

            let mutable last_time = -Time.infinity
            let mutable ln = 0us

            for { Time = time; Data = nr } in chart.Notes do
                if time <= last_time then
                    failwithf "Note row appears on or before the previous time (%f, %f)" time last_time
                elif not (Single.IsFinite (float32 time)) then
                    failwithf "Note row timestamp is invalid value: %f" time

                last_time <- time

                for k = 0 to (chart.Keys - 1) do
                    if nr.[k] = NoteType.HOLDHEAD then
                        if Bitmask.has_key k ln then
                            failwithf "Hold head appears inside hold at %f" time

                        ln <- Bitmask.set_key k ln
                    elif nr.[k] = NoteType.HOLDBODY then
                        if Bitmask.has_key k ln |> not then
                            failwithf "Hold middle appears with no head at %f" time
                    elif nr.[k] = NoteType.NOTHING then
                        if Bitmask.has_key k ln then
                            failwithf "Hold middle should have been present at %f" time
                    elif nr.[k] = NoteType.HOLDTAIL then
                        if Bitmask.has_key k ln |> not then
                            failwithf "Hold tail appears with no head at %f" time

                        ln <- Bitmask.unset_key k ln

                if NoteRow.is_empty nr then
                    failwithf "Note row is redundant at %f" time

            if ln <> 0us then
                failwithf "Unterminated hold notes at end of chart at %f [%i]" last_time ln

            Ok chart
        with err ->
            Error err.Message

    let write_headless (chart: Chart) (bw: BinaryWriter) =
        TimeArray.write chart.Notes bw (fun bw nr -> NoteRow.write bw nr)

        TimeArray.write
            chart.BPM
            bw
            (fun bw bpm ->
                bw.Write(bpm.Meter / 1<beat>)
                bw.Write(float32 bpm.MsPerBeat)
            )

        TimeArray.write chart.SV bw (fun bw f -> bw.Write f)

    let read_headless (keys: int) (br: BinaryReader) : Result<Chart, string> =
        try
            let notes = TimeArray.read br (NoteRow.read keys)

            let bpms =
                TimeArray.read
                    br
                    (fun r ->
                        {
                            Meter = r.ReadInt32() * 1<beat>
                            MsPerBeat = r.ReadSingle() * 1.0f<ms / beat>
                        }
                    )

            let sv = TimeArray.read br (fun r -> r.ReadSingle())

            check {
                Keys = keys
                Notes = notes
                BPM = bpms
                SV = sv
            }
        with err -> Error err.Message

    let diff (left: Chart) (right: Chart) =
        let f (o: Time) : int = o * 0.01f |> float32 |> round |> int
        let left_offset = left.FirstNote

        let xs =
            left.Notes
            |> Array.map (fun { Time = o; Data = nr } -> f (o - left_offset), NoteRow.pretty_print nr)

        let right_offset = right.FirstNote

        let ys =
            right.Notes
            |> Array.map (fun { Time = o; Data = nr } -> f (o - right_offset), NoteRow.pretty_print nr)

        for i = 0 to (min xs.Length ys.Length) - 1 do
            if xs.[i] <> ys.[i] then
                printfn
                    "! %A %A %f %f"
                    xs.[i]
                    ys.[i]
                    (left.Notes.[i].Time - left_offset)
                    (right.Notes.[i].Time - right_offset)

        printfn "%f : %f" (left.LastNote - left.FirstNote) (right.LastNote - right.FirstNote)

    let scale (scale: Rate) (chart: Chart) : Chart =
        { chart with
            Notes = TimeArray.scale scale chart.Notes
            BPM = TimeArray.scale scale chart.BPM
            SV = TimeArray.scale scale chart.SV
        }

    let private find_bpm_durations (points: TimeArray<BPM>) (end_time: Time) : Dictionary<float32<ms / beat>, Time> =

        let data = new Dictionary<float32<ms / beat>, Time>()

        let points = points |> List.ofSeq

        match points with
        | [] -> failwith "Impossible for a valid chart"
        | x :: xs ->
            let mutable current: float32<ms / beat> = x.Data.MsPerBeat
            let mutable time = Time.of_number x.Time

            for b in xs do
                if (not (data.ContainsKey current)) then
                    data.Add(current, 0.0f<ms>)

                data.[current] <- data.[current] + Time.of_number b.Time - time
                time <- Time.of_number b.Time
                current <- b.Data.MsPerBeat

            if (not (data.ContainsKey current)) then
                data.Add(current, 0.0f<ms>)

            data.[current] <- data.[current] + max (end_time - time) 0.0f<ms>

        data

    let find_most_common_bpm (chart: Chart) : float32<ms / beat> =
        (find_bpm_durations chart.BPM chart.LastNote)
            .OrderByDescending(fun p -> p.Value)
            .First()
            .Key

    let find_min_max_bpm (chart: Chart) : float32<ms / beat> * float32<ms / beat> =
        let d = (find_bpm_durations chart.BPM chart.LastNote).OrderBy(fun p -> p.Key)
        (d.First().Key, d.Last().Key)

    let pretty_print (notes: TimeArray<NoteRow>) =
        notes
        |> Array.iter (fun nr ->
            printfn "%06.1f | %s" nr.Time (NoteRow.pretty_print nr.Data)
        )