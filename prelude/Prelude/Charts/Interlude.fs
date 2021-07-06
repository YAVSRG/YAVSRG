namespace Prelude.Charts

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
        | NORMAL = 0
        | HOLDHEAD = 1
        | HOLDBODY = 2
        | HOLDTAIL = 3
        | MINE = 4
        | SPECIAL = 5
        | HITSOUND = 6

    type NoteRow = Bitmap array

    module NoteRow =
        let create taps holds middles ends mines special hitsound: NoteRow =
            [| taps; holds; middles; ends; mines; special; hitsound |]
        let noteData (nt: NoteType) (data: NoteRow) = data.[int nt]
        let hasNote k (nt: NoteType) = noteData nt >> Bitmap.hasBit k
        let apply (nt: NoteType) f (data: NoteRow) = data.[int nt] <- f data.[int nt]
        let isEmpty = function | ([| taps; holds; middles; ends; mines; special; hitsound|]: NoteRow) -> not (taps <> 0us || holds <> 0us || ends <> 0us || mines <> 0us || special <> 0us) | _ -> false

        let read (br: BinaryReader) =
            let (storage: uint16) = br.ReadByte() |> uint16
            let data = [| 0us; 0us; 0us; 0us; 0us; 0us; 0us |]
            for i = 0 to 6 do
                if (Bitmap.hasBit i storage) then data.[i] <- br.ReadUInt16()
            data

        let write (bw: BinaryWriter) (data: NoteRow) =
            let mutable storage = 0us
            for i = 0 to 6 do
                if data.[i] > 0us then storage <- Bitmap.setBit i storage
            bw.Write(storage |> byte)
            for i = 0 to 6 do
                if data.[i] > 0us then bw.Write(data.[i])

    (*
        Handling of time-sequenced data for notes, timing points, slider velocities, etc.
        Anything that is a sequence of events over the course of a chart
    *)

    type BPM = int<beat> * float32<ms/beat>

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

    type ChartHeader =
        { 
            Title: string
            Artist: string
            Creator: string
            DiffName: string
            PreviewTime: Time
            SourcePack: string
            BGFile: string
            AudioFile: string
        }
        static member Default =
            {
                Title = "Untitled Chart"
                Artist = ""
                Creator = ""
                DiffName = ""
                PreviewTime = 0.0f<ms>
                SourcePack = "Unknown"
                BGFile = ""
                AudioFile = "audio.mp3"
            }

    type Chart(keys, header, notes, bpms, sv, path) =
        member this.Keys = keys
        member this.Notes: TimeData<NoteRow> = notes
        member this.BPM: TimeData<BPM> = bpms
        member this.Header: ChartHeader = header
        member this.SV: MultiTimeData<float32> = sv
        member this.FileIdentifier: string = path
        member this.BGPath = Path.Combine(Path.GetDirectoryName(path), header.BGFile)
        member this.AudioPath = Path.Combine(Path.GetDirectoryName(path), header.AudioFile)

        new() = Chart(4, ChartHeader.Default, TimeData(), TimeData(), MultiTimeData(4), "unknown.yav")

    module Chart = 
        let private readSection<'t> (br: BinaryReader) f =
            let objectList = new List<TimeDataItem<'t>>()
            let count = br.ReadInt32()
            for i = 1 to count do
                objectList.Add((br.ReadSingle() * 1.0f<ms>, f br))
            TimeData<'t>(objectList)

        let private writeSection<'t> (data: TimeData<'t>) (bw : BinaryWriter) f =
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
                let notes = readSection br NoteRow.read
                let bpms = readSection br (fun r -> BPM(r.ReadInt32() * 1<beat>, r.ReadSingle() * 1.0f<ms/beat>))
                let sv = MultiTimeData(keys)
                for i in 0..keys do
                    sv.SetChannelData(i - 1, readSection br (fun r -> r.ReadSingle()))

                Some (Chart (keys, header, notes, bpms, sv, filepath))
            with
            | err -> Logging.Error ("Could not load chart from " + filepath, err); None

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
            if (chart.Notes.Count = 0) then
                "_"
            else
                let offset = offsetOf <| chart.Notes.First.Value
                for (o, nr) in chart.Notes.Data do
                    bw.Write ((o - offset) * 0.2f |> Convert.ToInt32)
                    for i = 0 to 5 do
                        bw.Write(nr.[i])
                for i = 0 to chart.Keys do
                    let mutable speed = 1.0
                    for (o, f) in (chart.SV.GetChannelData (i - 1)).Data do
                        let f = float f
                        if (speed <> f) then
                            bw.Write((o - offset) * 0.2f |> Convert.ToInt32)
                            bw.Write(f)
                            speed <- f
                BitConverter.ToString(h.ComputeHash (ms.ToArray())).Replace("-", "")