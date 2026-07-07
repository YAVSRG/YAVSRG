namespace Prelude.Charts

open System
open System.IO
open System.Linq
open System.Collections.Generic
open System.Security.Cryptography
open Prelude

type BPM =
    {
        Meter: int<beat>
        MsPerBeat: float32<ms / beat>
    }

type SV = float32

type [<Struct>] NoteData =
    { Keys: int; Notes: TimeArray<NoteRow> }
    member this.ToNoteData() : NoteData = this

[<StructuredFormatDisplay("<A {Keys}K chart>")>]
type Chart =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<SV>
    }

    member this.FirstNote : Time = (TimeArray.first this.Notes).Value.Time
    member this.LastNote : Time = (TimeArray.last this.Notes).Value.Time
    member this.ToNoteData() : NoteData = { Keys = this.Keys; Notes = this.Notes }

    member this.Hash() : string =
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)

        let offset = this.FirstNote

        bw.Write this.Keys

        for r in this.Notes do
            for nt in r.Data do
                bw.Write(byte nt)

        let mutable speed = 1.0

        for { Time = o; Data = f } in this.SV do
            let f = float f

            if speed <> f then
                bw.Write((o - offset) * 0.2f |> Convert.ToInt32)
                bw.Write f
                speed <- f

        bw.Write((this.LastNote - offset) * 0.01f |> float32 |> round |> int)

        BitConverter.ToString(SHA256.HashData(ms.ToArray())).Replace("-", "")

    member this.CheckForErrors() : Result<Chart, string> =
        try
            if this.Notes.Length = 0 then
                failwith "Chart must have notes"

            if this.BPM.Length = 0 then
                failwith "Chart must have at least one BPM marker"

            if this.Keys < 3 || this.Keys > 10 then
                failwithf "Chart must have 3-10 keys (Has %i)" this.Keys

            let inline check_valid_bpms() : unit =
                let mutable last_time = -Time.infinity

                for { Time = time; Data = { Meter = meter; MsPerBeat = mspb } } in this.BPM do
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
                
            let inline check_valid_svs() : unit =
                let mutable last_time = -Time.infinity
                
                for { Time = time; Data = sv } in this.SV do
                    if time < last_time then
                        failwithf "SV appears before the previous time (%f, %f)" time last_time
                    elif not (Single.IsFinite (float32 time)) then
                        failwithf "SV timestamp is invalid value: %f" time
                    elif Single.IsNaN sv then
                        failwithf "SV is NaN (should use +Infinity instead): %f" time

                    last_time <- time

            let inline check_valid_notes() : unit =
                let mutable last_time = -Time.infinity
                let mutable ln = Bitmask.Empty

                for { Time = time; Data = nr } in this.Notes do
                    if time <= last_time then
                        failwithf "Note row appears on or before the previous time (%f, %f)" time last_time
                    elif not (Single.IsFinite (float32 time)) then
                        failwithf "Note row timestamp is invalid value: %f" time

                    last_time <- time

                    for k = 0 to (this.Keys - 1) do
                        if nr.[k] = NoteType.HOLDHEAD then
                            if ln.Contains(k) then
                                failwithf "Hold head appears inside hold at %f" time

                            ln <- ln.Add(k)
                        elif nr.[k] = NoteType.HOLDBODY then
                            if not (ln.Contains(k)) then
                                failwithf "Hold middle appears with no head at %f" time
                        elif nr.[k] = NoteType.NOTHING then
                            if ln.Contains(k) then
                                failwithf "Hold middle should have been present at %f" time
                        elif nr.[k] = NoteType.HOLDTAIL then
                            if not (ln.Contains(k)) then
                                failwithf "Hold tail appears with no head at %f" time

                            ln <- ln.Remove(k)

                    if NoteRow.is_empty nr then
                        failwithf "Note row is redundant at %f" time

                if not ln.IsEmpty then
                    failwithf "Unterminated hold notes at end of chart at %f [%i]" last_time (ln.ToInt16())
                    
            check_valid_bpms()
            check_valid_svs()
            check_valid_notes()
            Ok this
        with err ->
            Error err.Message

    member this.WriteToStreamHeadless(bw: BinaryWriter) : unit =
        TimeArray.write this.Notes bw (fun bw nr -> NoteRow.write bw nr)

        TimeArray.write
            this.BPM
            bw
            (fun bw bpm ->
                bw.Write(bpm.Meter / 1<beat>)
                bw.Write(float32 bpm.MsPerBeat)
            )

        TimeArray.write this.SV bw (fun bw f -> bw.Write f)

    static member ReadFromStreamHeadless (keys: int, br: BinaryReader) : Result<Chart, string> =
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

            let sv = TimeArray.read br _.ReadSingle()

            let result =
                {
                    Keys = keys
                    Notes = notes
                    BPM = bpms
                    SV = sv
                }
            result.CheckForErrors()
        with err -> Error err.Message
        
module Chart =

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

    let scale (scale: float32</rate>) (chart: Chart) : Chart =
        { chart with
            Notes = TimeArray.scale scale chart.Notes
            BPM = TimeArray.scale scale chart.BPM
            SV = TimeArray.scale scale chart.SV
        }

    let inline notecount<^T when ^T: (member Notes : TimeArray<NoteRow>)> (chart: ^T) : int * int =
        let mutable notes = 0
        let mutable lnotes = 0

        for { Data = nr } in chart.Notes do
            for n in nr do
                if n = NoteType.NORMAL then
                    notes <- notes + 1
                elif n = NoteType.HOLDHEAD then
                    notes <- notes + 1
                    lnotes <- lnotes + 1
        notes, lnotes

    let private find_bpm_durations (points: TimeArray<BPM>) (end_time: Time) : Dictionary<float32<ms / beat>, Time> =

        let data = Dictionary<float32<ms / beat>, Time>()

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

    let pretty_print (notes: TimeArray<NoteRow>) : unit =
        notes
        |> Array.iter (fun nr ->
            printfn "%06.1f | %s" nr.Time (NoteRow.pretty_print nr.Data)
        )