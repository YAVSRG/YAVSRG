namespace Prelude.Charts.Formats

open System
open System.IO
open System.Security.Cryptography
open Percyqaz.Json
open Percyqaz.Common
open Prelude

module Interlude =

    type NoteType =
        | NOTHING = 0uy
        | NORMAL = 1uy
        | HOLDHEAD = 2uy
        | HOLDBODY = 3uy
        | HOLDTAIL = 4uy

    type NoteRow = NoteType array

    module NoteRow =

        let clone = Array.copy

        let createEmpty keycount : NoteRow = Array.create keycount NoteType.NOTHING

        let createNotes keycount (notes: Bitmask) =
            let nr = createEmpty keycount
            for k in Bitmask.toSeq notes do
                nr.[k] <- NoteType.NORMAL
            nr

        let createLnBodies keycount (notes: Bitmask) =
            let nr = createEmpty keycount 
            for k in Bitmask.toSeq notes do
                nr.[k] <- NoteType.HOLDBODY
            nr

        let isEmpty : NoteRow -> bool = Array.forall (function NoteType.NOTHING | NoteType.HOLDBODY -> true | _ -> false)

        let read (keycount: int) (br: BinaryReader) : NoteRow =
            let row = createEmpty keycount
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

        let write (bw: BinaryWriter) (row: NoteRow) =
            let columns = 
                seq { 
                    for i in 0 .. row.Length - 1 do
                        if row.[i] <> NoteType.NOTHING then yield i
                }
            bw.Write(Bitmask.ofSeq columns)
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

    type BPM = { Meter: int<beat>; MsPerBeat: float32<ms/beat> }
    type SV = float32

    (*
        Overall Interlude chart storage format
    *)
    
    [<Json.AutoCodec>]
    type MediaPath =
        | Relative of string
        | Absolute of string
        | Asset of string
        | Missing

    [<Json.AutoCodec>]
    type Origin =
        | Osu of beatmapsetid: int * beatmapid: int
        | Stepmania of packid: int
        | Unknown

    [<Json.AutoCodec(false)>]
    type ChartHeader =
        { 
            Title: string
            TitleNative: string option
            Artist: string
            ArtistNative: string option
            Creator: string
            DiffName: string
            Subtitle: string option
            Source: string option
            Tags: string list

            PreviewTime: Time
            BackgroundFile: MediaPath
            AudioFile: MediaPath

            ChartSource: Origin
        }
        static member Default =
            {
                Title = "Untitled Chart"
                TitleNative = None
                Artist = ""
                ArtistNative = None
                Creator = ""
                DiffName = ""
                Subtitle = None
                Source = None
                Tags = []

                PreviewTime = 0.0f<ms>
                BackgroundFile = Missing
                AudioFile = Missing

                ChartSource = Unknown
            }

    type Chart =
        {
            Keys: int
            Header: ChartHeader
            Notes: TimeArray<NoteRow>
            BPM: TimeArray<BPM>
            SV: TimeArray<float32>

            LoadedFromPath: string
        }
        
        member this.FirstNote = (TimeArray.first this.Notes).Value.Time
        member this.LastNote = (TimeArray.last this.Notes).Value.Time

    module Chart =

        let readHeadless (keys: int) (header: ChartHeader) (source_path: string) (br: BinaryReader) =

            let notes = TimeArray.read br (NoteRow.read keys)
            let bpms = TimeArray.read br (fun r -> { Meter = r.ReadInt32() * 1<beat>; MsPerBeat =  r.ReadSingle() * 1.0f<ms/beat> })
            let sv = TimeArray.read br (fun r -> r.ReadSingle())

            Some {
                Keys = keys
                Header = header
                Notes = notes
                BPM = bpms
                SV = sv

                LoadedFromPath = source_path
            }

        let fromFile filepath =
            try
                use fs = new FileStream(filepath, FileMode.Open)
                use br = new BinaryReader(fs)
                let keys = br.ReadByte() |> int

                let header = match JSON.FromString (br.ReadString()) with Ok v -> v | Error err -> Logging.Error(sprintf "%O" err); raise err
                readHeadless keys header filepath br

            with err -> Logging.Error (sprintf "Couldn't load chart from %s: %O" filepath err, err); None

        let writeHeadless (chart: Chart) (bw: BinaryWriter) =
            TimeArray.write chart.Notes bw (fun bw nr -> NoteRow.write bw nr)
            TimeArray.write chart.BPM bw (fun bw bpm -> bw.Write (bpm.Meter / 1<beat>); bw.Write (float32 bpm.MsPerBeat))
            TimeArray.write chart.SV bw (fun bw f -> bw.Write f)

        let toFile (chart: Chart) filepath =
            use fs = new FileStream(filepath, FileMode.Create)
            use bw = new BinaryWriter(fs)
            bw.Write(chart.Keys |> byte)
            bw.Write(JSON.ToString chart.Header)
            writeHeadless chart bw

        module LegacyHash =

            let fix (chart: Chart) : Chart =
                let fixed_notes =
                    seq {
                        let mutable i = 0
                        let mutable current = chart.Notes.[i]
                        while i < chart.Notes.Length do
                            if chart.Notes.[i].Time = current.Time then
                                for k = 0 to chart.Keys - 1 do 
                                    if chart.Notes.[i].Data.[k] <> NoteType.NOTHING then current.Data.[k] <- chart.Notes.[i].Data.[k]
                            else
                                yield current
                                current <- chart.Notes.[i]
                            i <- i + 1
                        yield current
                    } |> Array.ofSeq
                { chart with Notes = fixed_notes }

            let hash (chart: Chart) : string =
                let h = SHA256.Create()
                use ms = new MemoryStream()
                use bw = new BinaryWriter(ms)

                let offset = chart.FirstNote

                for { Time = o; Data = nr } in chart.Notes do
                    if NoteRow.isEmpty nr |> not then
                        bw.Write ((o - offset) * 0.2f |> Convert.ToInt32)
                        for nt in nr do bw.Write (byte nt)

                let mutable speed = 1.0
                for { Time = o; Data = f } in chart.SV do
                    let f = float f
                    if (speed <> f) then
                        bw.Write((o - offset) * 0.2f |> Convert.ToInt32)
                        bw.Write(f)
                        speed <- f

                BitConverter.ToString(h.ComputeHash (ms.ToArray())).Replace("-", "")
        
        let hash (chart: Chart) : string =
            let h = SHA256.Create()
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
        
            let offset = chart.FirstNote

            bw.Write chart.Keys
        
            for r in chart.Notes do
                for nt in r.Data do bw.Write (byte nt)
        
            let mutable speed = 1.0
            for { Time = o; Data = f } in chart.SV do
                let f = float f
                if speed <> f then
                    bw.Write((o - offset) * 0.2f |> Convert.ToInt32)
                    bw.Write f
                    speed <- f

            bw.Write((chart.LastNote - offset) * 0.01f |> float32 |> round |> int)
        
            BitConverter.ToString(h.ComputeHash (ms.ToArray())).Replace("-", "")

        let check (chart: Chart) : Result<unit, string> =
            try
                let mutable lastTime = -Time.infinity
                let mutable ln = 0us
                for { Time = time; Data = nr } in chart.Notes do
                    if time <= lastTime then failwithf "Note row appears on or before the previous time (%f, %f)" time lastTime
                    lastTime <- time

                    for k = 0 to (chart.Keys - 1) do
                        if nr.[k] = NoteType.HOLDHEAD then
                            if Bitmask.hasBit k ln then failwithf "Hold head appears inside hold at %f" time
                            ln <- Bitmask.setBit k ln
                        elif nr.[k] = NoteType.HOLDBODY then
                            if Bitmask.hasBit k ln |> not then failwithf "Hold middle appears with no head at %f" time
                        elif nr.[k] = NoteType.NOTHING then
                            if Bitmask.hasBit k ln then failwithf "Hold middle should have been present at %f" time
                        elif nr.[k] = NoteType.HOLDTAIL then
                            if Bitmask.hasBit k ln |> not then failwithf "Hold tail appears with no head at %f" time
                            ln <- Bitmask.unsetBit k ln

                    if NoteRow.isEmpty nr then failwithf "Note row is useless/empty at %f" time
                if ln <> 0us then failwithf "Unterminated hold notes at end of chart at %f [%i]" lastTime ln
                Ok()
            with err ->
                Error err.Message

        let diff (left: Chart) (right: Chart) =
            let f (o: Time) : int = o * 0.01f |> float32 |> round |> int
            let left_offset = left.FirstNote
            let xs =
                left.Notes
                |> Array.map (fun { Time = o; Data = nr } -> f (o - left_offset), NoteRow.prettyPrint nr)
            let right_offset = right.FirstNote
            let ys =
                right.Notes
                |> Array.map (fun { Time = o; Data = nr } -> f (o - right_offset), NoteRow.prettyPrint nr)
            for i = 0 to (min xs.Length ys.Length) - 1 do
                if xs.[i] <> ys.[i] then printfn "! %A %A %f %f" xs.[i] ys.[i] (left.Notes.[i].Time - left_offset) (right.Notes.[i].Time - right_offset)
            printfn "%f : %f" (left.LastNote - left.FirstNote) (right.LastNote - right.FirstNote)