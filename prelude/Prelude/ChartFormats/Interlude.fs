namespace Prelude.ChartFormats

open System
open System.IO
open System.Collections.Generic
open System.Security.Cryptography
open System.ComponentModel
open Percyqaz.Json
open Prelude.Common


module Interlude =

    (*
        Bit mappings for use in storing rows of notes. Stores up to 16 flags so this format can theoretically support 16 columns.
        Interlude only uses 10 columns.
    *)

    type Bitmap = uint16
    module Bitmap = 
        let rec count (x : Bitmap) =
            match x with
            | 0us -> 0
            | n -> (if (n &&& 1us) = 1us then 1 else 0) + (count (n >>> 1))

        let (|?) (x: Bitmap) k = (1us <<< k) &&& x > 0us
        let (|+) (x: Bitmap) k = (1us <<< k) ||| x
        let (|-) (x: Bitmap) k = ~~~(1us <<< k) &&& x
        let (|^) (x: Bitmap) k = (1us <<< k) ^^^ x

        let hasBit (k: int) (x: Bitmap) = x |? k
        let setBit k (x: Bitmap) = x |+ k
        let unsetBit k (x: Bitmap) = x |- k
        let toggleBit k (x: Bitmap) = x |^ k

        let rec toSeq (x: Bitmap) =
            seq {
                for i = 0 to 15 do
                    if (hasBit i x) then yield i
            }

        let create (l: seq<int>) =
            let mutable bm: Bitmap = 0us
            Seq.iter (fun k -> bm <- setBit k bm) l
            bm

    (*
        Array of bitmaps used to represent rows of notes. The enum dictates which position in the array corresponds to what data.
    *)

    type NoteType =
        | NOTHING = 0uy
        | NORMAL = 1uy
        | HOLDHEAD = 2uy
        | HOLDBODY = 3uy
        | HOLDTAIL = 4uy

    type NoteRow = NoteType array

    module NoteRow =
        let setNoteData (nt: NoteType) (row: NoteRow) (data: Bitmap) =
            for k = 0 to row.Length - 1 do
                if Bitmap.hasBit k data then row.[k] <- nt
                elif row.[k] = nt then row.[k] <- NoteType.NOTHING

        let clone = Array.copy

        let createEmpty keycount : NoteRow = Array.create keycount NoteType.NOTHING

        let createNotes keycount (notes: Bitmap) =
            let nr = createEmpty keycount 
            setNoteData NoteType.NORMAL nr notes
            nr

        let createLnBodies keycount (notes: Bitmap) =
            let nr = createEmpty keycount 
            setNoteData NoteType.HOLDBODY nr notes
            nr

        let isEmpty : NoteRow -> bool = Array.forall (function NoteType.NOTHING | NoteType.HOLDBODY -> true | _ -> false)

        let noteData (nt: NoteType) (row: NoteRow) = Array.indexed row |> Array.filter (fun (i, x) -> x = nt) |> Array.map fst |> Seq.ofArray |> Bitmap.create

        let apply (nt: NoteType) f (row: NoteRow) = (noteData nt row |> f |> setNoteData nt row)

        let read (keycount: int) (br: BinaryReader) : NoteRow =
            let row = createEmpty keycount
            let columns = br.ReadUInt16()
            for k in Bitmap.toSeq columns do
                row.[k] <-
                    match br.ReadByte() with
                    | 1uy -> NoteType.NORMAL
                    | 2uy -> NoteType.HOLDHEAD
                    | 3uy -> NoteType.HOLDBODY
                    | 4uy -> NoteType.HOLDTAIL
                    | b -> failwithf "unexpected note type in chart data: %i" b
            row

        let write (bw: BinaryWriter) (row: NoteRow) =
            let columns = 
                seq { 
                    for i in 0 .. row.Length - 1 do
                        if row.[i] <> NoteType.NOTHING then yield i
                }
            bw.Write(Bitmap.create columns)
            for k in columns do
                bw.Write(byte row.[k])

        let prettyPrint (row: NoteRow) =
            let p =
                function
                | NoteType.NORMAL -> '#'
                | NoteType.HOLDHEAD -> '^'
                | NoteType.HOLDBODY -> '|'
                | NoteType.HOLDTAIL -> 'v'
                | NoteType.NOTHING
                | _ -> ' '
            new string(row |> Array.map p)

    (*
        Handling of time-sequenced data for notes, timing points, slider velocities, etc.
        Anything that is a sequence of events over the course of a chart
    *)

    type BPM = int<beat> * float32<ms/beat>

    // todo: struct this as optimisation?
    type TimeDataItem<'t> = Time * 't
    let offsetOf ((offset, _): TimeDataItem<'t>) = offset

    type TimeData<'t>(list) =
        let data: List<TimeDataItem<'t>> = list

        member this.Data = data.AsReadOnly()

        new() = TimeData<'t>(new List<TimeDataItem<'t>>())

        member this.Count = data.Count

        member this.SetData(list: IReadOnlyList<TimeDataItem<'t>>) =
            data.Clear()
            data.AddRange(list)
        member this.SetData(data: TimeData<'t>) = this.SetData(data.Data)
        member this.Clone() = let t = TimeData<'t>() in t.SetData(this.Data); t

        member this.IndexAt time: int * bool =
            match this.Count with
            | 0 -> (-1, false)
            | 1 ->
                let (offset, _) = data.[0]
                if time < offset then (-1, false)
                else if time = offset then (0, true)
                else (0, false)
            | n ->
                //Binary search with the added touch that you get
                //(I, TRUE/FALSE) with I = the highest index with offset <= the time requested
                //and the TRUE/FALSE flag is true if the offset = the time requested
                let mutable low = 0
                let mutable high = this.Count
                let mutable mid = -1
                while (low < high) do
                    mid <- (high + low) / 2
                    let o = offsetOf data.[mid]
                    if (o < time) then
                        low <- mid + 1
                    else
                        high <- mid
                if low = 0 then (-1, false) else
                    if (offsetOf data.[low - 1] = time) then (low - 1, true) else (low - 1, false)

        member this.GetPointAt time: TimeDataItem<'t> =
            let (index, _) = this.IndexAt time in data.[index]

        member this.GetNextPointAt time: TimeDataItem<'t> =
                let (index, _) = this.IndexAt time
                if (index + 1 < this.Count) then data.[index + 1] else data.[index]

        member this.InterpolatePointAt time interp_func : TimeDataItem<'t> =
            match this.IndexAt time with
            | (index, true) -> data.[index]
            | (index, false) ->
                let (time0, data) = data.[index] in (time, interp_func time0 time data)

        member this.InsertAt time guts =
            match this.IndexAt time with
            | (_, true) -> failwith "Cannot insert two points in same place, use ReplaceAt instead."
            | (index, false) -> data.Insert(index + 1, (time, guts))

        member this.Insert(time, guts) = this.InsertAt time guts

        member this.ReplaceAt time guts =
            match this.IndexAt time with
            | (index, true) ->
                data.RemoveAt(index)
                data.Insert(index, (time, guts))
            | (_, false) -> failwith "No point to replace, use InsertAt instead."

        member this.InsertOrReplaceAt time guts =
            match this.IndexAt time with
            | (index, true) ->
                data.RemoveAt(index)
                data.Insert(index, (time, guts))
            | (index, false) -> data.Insert(index + 1, (time, guts))

        member this.InsertOrReplace(time, guts) = this.InsertOrReplaceAt time guts

        member this.RemoveAt time =
            match this.IndexAt time with
            | (index, true) -> data.RemoveAt(index)
            | (_, false) -> failwith "No point to remove here."

        member this.Empty = this.Count = 0
        member this.First = if this.Empty then None else Some data.[0]
        member this.Last = if this.Empty then None else Some data.[data.Count - 1]
        member this.Clear() = data.Clear()
        
        member this.EnumerateBetween time1 time2 = 
            seq {
                let mutable i =
                    match this.IndexAt(time1) with
                    | (j, false) -> j + 1
                    | (j, true) -> j
                while (i < this.Count && offsetOf data.[i] < time2) do
                    yield data.[i]
                    i <- i + 1
            }

        member this.MapBetween time1 time2 (mapper: TimeDataItem<'t> -> 't) =
            let mutable i =
                match this.IndexAt(time1) with
                | (j, false) -> j + 1
                | (j, true) -> j
            while (i < this.Count && offsetOf data.[i] < time2) do
                data.[i] <- (offsetOf data.[i], mapper data.[i])
                i <- i + 1

    type MultiTimeData<'t>(keys) =
        let data = [| for i in 0 .. keys -> TimeData() |]

        member this.SetChannelData(k, (newData: List<TimeDataItem<'t>>)) = data.[k + 1].SetData(newData)
        member this.SetChannelData(k, (newData: TimeData<'t>)) = data.[k + 1].SetData(newData)
        member this.GetChannelData k = data.[k + 1]
        member this.IsEmpty() = Array.fold (fun b (t: TimeData<'t>) -> b && t.Empty) true data
        member this.Clear() = Array.iter (fun (t: TimeData<'t>) -> t.Clear()) data
        member this.Clone() =
            let mt = MultiTimeData keys
            for i in 0 .. keys do mt.SetChannelData(i - 1, this.GetChannelData(i - 1))
            mt

    (*
        Overall Interlude chart storage format
    *)

    type MediaPath =
        | Relative of string
        | Absolute of string

    type ChartSource =
        | Osu of beatmapsetid: int * beatmapid: int
        | Stepmania of packid: int
        | Unknown

    type ChartHeader =
        { 
            Title: string
            Artist: string
            Creator: string
            DiffName: string
            PreviewTime: Time
            SourcePack: string
            BackgroundFile: MediaPath
            AudioFile: MediaPath
            ChartSource: ChartSource
        }
        static member Default =
            {
                Title = "Untitled Chart"
                Artist = ""
                Creator = ""
                DiffName = ""
                PreviewTime = 0.0f<ms>
                SourcePack = "Unknown"
                BackgroundFile = Relative ""
                AudioFile = Relative "audio.mp3"
                ChartSource = Unknown
            }

    type Chart(keys: int, header: ChartHeader, notes: TimeData<NoteRow>, bpms: TimeData<BPM>, sv: MultiTimeData<float32>, path: string) =

        do if notes.Count = 0 then invalidArg (nameof notes) "A chart cannot have no notes"

        member this.Keys = keys
        member this.Notes: TimeData<NoteRow> = notes
        member this.BPM: TimeData<BPM> = bpms
        member this.Header: ChartHeader = header
        member this.SV: MultiTimeData<float32> = sv
        member this.FileIdentifier: string = path

        member this.BackgroundPath = match header.BackgroundFile with Relative s -> Path.Combine(Path.GetDirectoryName path, s) | Absolute s -> s
        member this.AudioPath = match header.AudioFile with Relative s -> Path.Combine(Path.GetDirectoryName path, s) | Absolute s -> s

        member this.FirstNote = offsetOf this.Notes.First.Value
        member this.LastNote = offsetOf this.Notes.Last.Value

        new() = Chart(4, ChartHeader.Default, TimeData(ResizeArray([(0.0f<ms>, NoteRow.createEmpty 4)])), TimeData(), MultiTimeData(4), "unknown.yav")

    module Chart = 
        let private readSection<'t> (br: BinaryReader) f =
            let objectList = new List<TimeDataItem<'t>>()
            let count = br.ReadInt32()
            for i = 1 to count do
                objectList.Add((br.ReadSingle() * 1.0f<ms>, f br))
            TimeData<'t>(objectList)

        let private writeSection<'t> (data: TimeData<'t>) (bw: BinaryWriter) f =
            bw.Write(data.Count)
            for (time, guts) in data.Data do
                bw.Write(time |> float32)
                f guts

        let fromFile filepath =
            try
                use fs = new FileStream(filepath, FileMode.Open)
                use br = new BinaryReader(fs)
                let keys = br.ReadByte() |> int

                let header = JSON.FromString (br.ReadString()) |> JsonResult.expect
                let notes = readSection br (NoteRow.read keys)
                let bpms = readSection br (fun r -> BPM(r.ReadInt32() * 1<beat>, r.ReadSingle() * 1.0f<ms/beat>))
                let sv = MultiTimeData(keys)
                for i in 0..keys do
                    sv.SetChannelData(i - 1, readSection br (fun r -> r.ReadSingle()))

                Some (Chart (keys, header, notes, bpms, sv, filepath))
            with err -> Logging.Error ("Could not load chart from " + filepath, err); None

        let toFile (chart: Chart) filepath =
            use fs = new FileStream(filepath, FileMode.Create)
            use bw = new BinaryWriter(fs)
            bw.Write(chart.Keys |> byte)
            bw.Write(JSON.ToString chart.Header)
            writeSection chart.Notes bw (fun nr -> NoteRow.write bw nr)
            writeSection chart.BPM bw (fun (meter, msPerBeat) -> bw.Write (meter / 1<beat>); bw.Write (float32 msPerBeat))
            for i = 0 to chart.Keys do
                writeSection (chart.SV.GetChannelData (i - 1)) bw (fun f -> bw.Write f)

        let save (chart: Chart) =
            toFile chart chart.FileIdentifier

        let hash (chart: Chart) : string =
            let h = SHA256.Create()
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)

            let offset = chart.FirstNote

            for (o, nr) in chart.Notes.Data do
                if NoteRow.isEmpty nr |> not then
                    bw.Write ((o - offset) * 0.2f |> Convert.ToInt32)
                    for nt in nr do bw.Write (byte nt)

            for i = 0 to chart.Keys do
                let mutable speed = 1.0
                for (o, f) in (chart.SV.GetChannelData (i - 1)).Data do
                    let f = float f
                    if (speed <> f) then
                        bw.Write((o - offset) * 0.2f |> Convert.ToInt32)
                        bw.Write(f)
                        speed <- f

            BitConverter.ToString(h.ComputeHash (ms.ToArray())).Replace("-", "")

        let check (chart: Chart) =
            try
                let mutable lastTime = -Time.infinity
                let mutable ln = 0us
                for (time, row) in chart.Notes.Data do
                    if time <= lastTime then failwithf "Sanity check failed: Note row appears on or before the previous time (%f, %f)" time lastTime
                    lastTime <- time

                    for k = 0 to (chart.Keys - 1) do
                        if row.[k] = NoteType.HOLDHEAD then
                            if Bitmap.hasBit k ln then failwithf "Sanity check failed: Hold head appears inside hold at %f" time
                            ln <- Bitmap.setBit k ln
                        elif row.[k] = NoteType.HOLDBODY then
                            if Bitmap.hasBit k ln |> not then failwithf "Sanity check failed: Hold middle appears with no head at %f" time
                        elif row.[k] = NoteType.NOTHING then
                            if Bitmap.hasBit k ln then failwithf "Sanity check failed: Hold middle should have been present at %f" time
                        elif row.[k] = NoteType.HOLDTAIL then
                            if Bitmap.hasBit k ln |> not then failwithf "Sanity check failed: Hold tail appears with no head at %f" time
                            ln <- Bitmap.unsetBit k ln

                    if NoteRow.isEmpty row then failwithf "Sanity check failed: Note row is useless/empty at %f" time
            with err ->
                Logging.Error (sprintf "Sanity check for chart %s failed; %s" chart.FileIdentifier err.Message)
                Console.ReadLine() |> ignore