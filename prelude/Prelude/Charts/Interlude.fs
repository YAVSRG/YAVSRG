module Prelude.Charts.Interlude

open System
open System.IO
open System.Collections.Generic
open System.Security.Cryptography
open Prelude.Common
open Newtonsoft.Json

(*
    Bit mappings for use in storing rows of notes. Stores up to 16 flags so this format can theoretically support 16 columns.
    Interlude only uses 10 columns.
*)

type Bitmap = uint16

let rec countBits (x : Bitmap) =
    match x with
    | 0us -> 0
    | n -> (if (n &&& 1us) = 1us then 1 else 0) + (countBits (n >>> 1))

let hasBit k (x: Bitmap) = (1us <<< k) &&& x > 0us
let setBit k (x: Bitmap) = (1us <<< k) ||| x
let unsetBit k (x: Bitmap) = ~~~(1us <<< k) &&& x
let toggleBit k (x: Bitmap) = (1us <<< k) ^^^ x

let rec getBits x =
    seq {
        for i = 0 to 15 do
            if (hasBit i x) then yield i
    }

let makeBitmap (l: seq<int>) =
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

let makeNoteRow taps holds middles ends mines special hitsound: NoteRow =
    [| taps; holds; middles; ends; mines; special; hitsound |]
let noteData (nt: NoteType) (data: NoteRow) = data.[nt |> int]
let testForNote k (nt: NoteType) = noteData nt >> hasBit k
let applyToNoteData (nt: NoteType) f (data: NoteRow) = data.[nt |> int] <- data |> noteData nt |> f

let readRowFromFile (br: BinaryReader) =
    let (storage: uint16) = br.ReadByte() |> uint16
    let data = [| 0us; 0us; 0us; 0us; 0us; 0us; 0us |]
    for i = 0 to 6 do
        if (hasBit i storage) then data.[i] <- br.ReadUInt16()
    data

let writeRowToFile (bw: BinaryWriter) (data: NoteRow) =
    let mutable storage = 0uy
    for i = 0 to 6 do
        if data.[i] > 0us then storage <- storage &&& (1uy <<< i)
    bw.Write(storage)
    for i = 0 to 6 do
        if data.[i] > 0us then bw.Write(data.[i])

(*
    Handling of time-sequenced data for notes, timing points, slider velocities, etc.
    Anything that is a sequence of events over the course of a chart
*)

type BPM = int * float

type TimeDataItem<'t> = float * 't

let offsetOf ((offset, _): TimeDataItem<'t>) = offset

type TimeData<'t>(list) =
    let data: List<TimeDataItem<'t>> = list
    let mutable count: int = list.Count

    member private this.GetData = data
    new() = TimeData<'t>(new List<TimeDataItem<'t>>())
    new(td: TimeData<'t>) = TimeData<'t>(td.GetData)

    member this.Count = count

    member this.SetData(list: List<TimeDataItem<'t>>) =
        data.Clear()
        data.AddRange(list)
        count <- list.Count

    member this.SetData(data: TimeData<'t>) = this.SetData(data.GetData)

    member this.IndexAt time: int * bool =
        match count with
        | 0 -> (0, false)
        | 1 ->
            let (offset, _) = data.[0]
            if time < offset then (-1, false)
            else if time = offset then (0, true)
            else (1, false)
        | n ->
            let (offset, _) = data.[0]
            if time < offset then
                (-1, false)
            else
                //Binary search with the added touch that you get
                //(I, TRUE/FALSE) with I = the highest index with offset <= the time requested
                //and the TRUE/FALSE flag is true if the offset = the time requested
                let mutable low = 0
                let mutable high = count
                let mutable mid = -1
                while (low + 1 < high) do
                    mid <- (high + low) / 2
                    let o = offsetOf data.[mid]
                    if (o = time) then
                        low <- mid
                        high <- mid
                    else if (o < time) then
                        low <- mid
                    else
                        high <- mid
                match (high - low) with
                | 0 -> (low, true)
                | 1 -> (low, false)
                | _ -> failwith "impossible"

    member this.GetPointAt time: TimeDataItem<'t> =
        let (index, _) = this.IndexAt time
        Logging.Debug (string index) ""
        data.[index]

    member this.GetNextPointAt time: TimeDataItem<'t> =
            let (index, _) = this.IndexAt time
            Logging.Debug (string index) ""
            if (index + 1 < count) then data.[index + 1] else data.[index]

    member this.InterpolatePointAt time interp_func : TimeDataItem<'t> =
        match this.IndexAt time with
        | (index, true) -> data.[index]
        | (index, false) ->
            let (time0, data) = data.[index] in (time, interp_func time0 time data)

    member this.InsertAt time guts =
        match this.IndexAt time with
        | (_, true) -> failwith "Cannot insert two points in same place, use ReplaceAt instead."
        | (index, false) ->
            data.Insert(index, (time, guts))
            count <- count + 1

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
        | (index, false) ->
            data.Insert(index, (time, guts))
            count <- count + 1

    member this.InsertOrReplace(time, guts) = this.InsertOrReplaceAt time guts

    member this.RemoveAt time =
        match this.IndexAt time with
        | (index, true) ->
            data.RemoveAt(index)
            count <- count - 1
        | (_, false) -> failwith "No point to remove here."

    member this.IsEmpty = (count = 0)

    member this.Clear = data.Clear(); count <- 0

    member this.First = data.[0] //Use IsEmpty to check this exists first before use
    member this.Enumerate = data.AsReadOnly()

    member this.EnumerateBetween time1 time2 = 
        seq {
            let mutable i =
                match this.IndexAt(time1) with
                | (j, false) -> j + 1
                | (j, true) -> j
            while (i < count && offsetOf data.[i] < time2) do
                yield data.[i]
                i <- i + 1
        }

type MultiTimeData<'t>(keys) =

    let data = [| for i in 0 .. keys -> TimeData() |]

    member this.SetChannelData(k, (newData: List<TimeDataItem<'t>>)) = data.[k + 1].SetData(newData)
    member this.SetChannelData(k, (newData: TimeData<'t>)) = data.[k + 1].SetData(newData)
    member this.GetChannelData k = data.[k + 1]
    member this.IsEmpty = Array.fold (fun b (t: TimeData<'t>) -> b && t.IsEmpty) true data
    member this.Clear = Array.forall (fun (t: TimeData<'t>) -> t.Clear; true) data

(*
    Overall Interlude chart storage format
*)

type ChartHeader =
    { Title: string
      Artist: string
      Creator: string
      DiffName: string
      PreviewTime: float
      SourcePack: string
      BGFile: string
      AudioFile: string
      [<JsonIgnore>]
      File: string
      [<JsonIgnore>]
      SourcePath: string }

let DefaultChartHeader =
    { Title = "Untitled Chart"
      Artist = ""
      Creator = ""
      DiffName = ""
      PreviewTime = 0.0
      SourcePath = ""
      SourcePack = "Unknown"
      BGFile = ""
      AudioFile = "audio.mp3"
      File = "unknown.yav" }

type Chart(keys, header, notes, bpms, sv) =
    member this.Keys = keys
    member this.Notes: TimeData<NoteRow> = notes
    member this.BPM: TimeData<int * float> = bpms
    member this.Header: ChartHeader = header
    member this.SV: MultiTimeData<float> = sv

    new() = Chart(4, DefaultChartHeader, TimeData(), TimeData(), MultiTimeData(4))

    member this.WithHeader(h) = Chart(this.Keys, h, this.Notes, this.BPM, this.SV)

    member this.FileIdentifier = Path.Combine(this.Header.SourcePath, this.Header.File)

    member this.WriteToFile(path: string): unit = () //NYI

let private readSection<'t> (br: BinaryReader) f =
    let objectList = new List<TimeDataItem<'t>>()
    let count = br.ReadInt32()
    for i = 1 to count do
        objectList.Add((br.ReadSingle() |> float, f br))
    TimeData<'t>(objectList)

let loadChartFile filepath =
    use fs = new FileStream(filepath, FileMode.Open)
    use br = new BinaryReader(fs)
    let keys = br.ReadByte()

    let header =
        { loadJson (br.ReadString()) with
              File = Path.GetFileName(filepath)
              SourcePath = Path.GetDirectoryName(filepath) }

    let notes = readSection br (readRowFromFile)
    let bpms = readSection br (fun r -> BPM(r.ReadInt32(), r.ReadSingle() |> float))
    Chart
        (keys |> int, header, notes, bpms,
         let sv = MultiTimeData(keys |> int)
         for i in 0 .. (keys |> int) do
             sv.SetChannelData(i - 1, readSection br (fun r -> (r.ReadSingle() |> float)))
         sv)

let calculateHash (chart: Chart): string =
    let h = SHA256.Create()
    let data = Array.zeroCreate<byte> (16 * chart.Notes.Count)
    if (chart.Notes.Count = 0) then
        "_"
    else
        let offset = offsetOf chart.Notes.First
        let mutable p = 0
        for (o, nr) in chart.Notes.Enumerate do
            BitConverter.GetBytes((o - offset) |> int).CopyTo(data, p)
            p <- p + 4
            for i = 0 to 5 do
                BitConverter.GetBytes(nr.[i]).CopyTo(data, p + 2 * i)
            p <- p + 12
        for i = 0 to chart.Keys |> int do
            let mutable speed = 1.0
            for (o, f) in (chart.SV.GetChannelData(i - 1)).Enumerate do
                if (speed <> f) then
                    Array.Resize(ref data, data.Length + 8)
                    BitConverter.GetBytes((o - offset) |> int).CopyTo(data, p)
                    p <- p + 4
                    BitConverter.GetBytes(f |> float32).CopyTo(data, p)
                    p <- p + 4
                    speed <- f
                else
                    ()
        BitConverter.ToString(h.ComputeHash(data)).Replace("-", "")
