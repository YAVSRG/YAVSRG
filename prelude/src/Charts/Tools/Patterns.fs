namespace Prelude.Charts.Tools.Patterns

open System.Collections.Generic
open Prelude.Common
open Prelude.Charts.Formats.Interlude

(*
    This file is a WIP, will change significantly before it is used
*)


// Leaving this here as a reminder of the original idea but it would be foolish to build all of this in one

type Primitives =
    | Stream of
        // if spacing of each note is equal back to back it's a stair e.g. 1234 or 1357, valued 1 - 10. note that 1 is only possible for an isolated note
        stair_length: int
    | Jack of 
        // marks length of jack, from 2 - 10
        length: int *
        // number of columns shared from this row to next, 1 - 10
        count: int *
        // number of notes in row, 1 - 10
        chord_size: int
    | Chord of
        // number of notes in chord
        size: int *
        // number of consecutive single notes appearing after chord, 1 - 9 
        spacing: int
    | Chord_Stream of
        // number of notes in chord, 2 - 10
        size: int *
        // chords in a row where notes are only ascending/descending, 1 - 5
        roll_length: int *
        // chords in a row, from 2 - 10
        length: int
    | Trill of
        // number of columns shared from this row to 2 rows ago, 1 - 9
        count: int *
        // number of notes in row, 1 - 9
        chord_size: int *
        // length of trill in rows, 3 - 10
        length: int
    | Release

[<RequireQualifiedAccess>]
type Direction =
    | None
    | Left
    | Right
    | Outwards
    | Inwards

type RowInfo =
    {
        Notes: int
        Jacks: int
        Direction: Direction
        Roll: bool
        Time: Time
        BPM: float32<beat/minute>
    }

module Analysis =

    let run (chart: Chart) : RowInfo list =
        
        let t, row = chart.Notes.First.Value
        let mutable previous_row =
            seq {0 .. chart.Keys - 1}
            |> Seq.filter (fun x -> row.[x] = NoteType.NORMAL || row.[x] = NoteType.HOLDHEAD)
            |> Array.ofSeq
        let mutable previous_time = t

        seq {
            for t, row in (chart.Notes.Data |> Seq.skip 1) do
                
                let current_row = 
                    seq {0 .. chart.Keys - 1}
                    |> Seq.filter (fun x -> row.[x] = NoteType.NORMAL || row.[x] = NoteType.HOLDHEAD)
                    |> Array.ofSeq

                if current_row.Length > 0 then

                    let pmin = Array.min previous_row
                    let pmax = Array.max previous_row
                    let cmin = Array.min current_row
                    let cmax = Array.max current_row

                    yield {
                        Notes = current_row.Length
                        Jacks = current_row.Length - (Array.except previous_row current_row).Length
                        Direction =
                            let lo = cmin - pmin
                            let hi = cmax - pmax

                            if lo > 0 then
                                if hi > 0 then Direction.Right
                                else Direction.Inwards
                            elif lo < 0 then
                                if hi < 0 then Direction.Left
                                else Direction.Outwards
                            else
                                if hi < 0 then Direction.Inwards
                                elif hi > 0 then Direction.Outwards
                                else Direction.None
                        Roll = pmin > cmax || pmax < cmin
                        Time = t
                        BPM = (0.25f<beat> * (60000.0f<ms/minute> / (t - previous_time)))
                    }

                    previous_row <- current_row
                    previous_time <- t

        } |> List.ofSeq

    let density (samples: int) (chart: Chart) : int array * int array =
        let mutable i = 0
        let mutable notes = 0
        let mutable rows = 0
        let mutable last_sample = 0
        let mutable last_sample_rows = 0
        let start = chart.FirstNote
        let length = chart.LastNote - start
        let interval = length / float32 samples

        let notecounts = Array.zeroCreate samples
        let rowcounts = Array.zeroCreate samples

        for t, row in chart.Notes.Data do
            let mutable is_empty = true
            for nt in row do
                if nt = NoteType.NORMAL || nt = NoteType.HOLDHEAD then 
                    is_empty <- false
                    notes <- notes + 1
            if not is_empty then rows <- rows + 1
            while t - start >= interval * float32 (i + 1) do
                notecounts.[i] <- notes - last_sample
                rowcounts.[i] <- rows - last_sample_rows
                last_sample <- notes
                last_sample_rows <- rows
                i <- i + 1
        if i <> samples then 
            notecounts.[samples - 1] <- notes - last_sample
            rowcounts.[samples - 1] <- rows - last_sample_rows

        notecounts, rowcounts

type Pattern = RowInfo list -> bool

module Patterns =

    module Common =
        
        let STREAMS : Pattern = 
            function
            |      { Jacks = 0 }
                :: { Jacks = 0 }
                :: { Jacks = 0 }
                :: { Jacks = 0 }
                :: _ -> true
            | _ -> false

        let ALTERNATION : Pattern =
            function 
            |      { Jacks = 0; Direction = Direction.Right }
                :: { Jacks = 0; Direction = Direction.Left }
                :: _ -> true
            |      { Jacks = 0; Direction = Direction.Left }
                :: { Jacks = 0; Direction = Direction.Right }
                :: _ -> true
            | _ -> false

        let JACKS : Pattern = function { Jacks = x } :: _ when x > 0 -> true | _ -> false

        let CHORDJACKS : Pattern = 
            function
            |   { Notes = a }
                :: { Notes = b; Jacks = j } 
                :: _ when a > 1 && b > 1 && j >= 1 && (b < a || j < b) -> true
            | _ -> false

        let GLUTS : Pattern =
            function
            |   { Notes = a }
                :: { Notes = b; Jacks = 1 } 
                :: _ when a > 1 && b > 1 -> true
            | _ -> false

    module FourKey =

        let HANDSTREAM : Pattern =
            function
            |      { Notes = 3; Jacks = 0 }
                :: { Jacks = 0 }
                :: _ -> true
            | _ -> false

        let JUMPSTREAM : Pattern =
            function
            |      { Notes = 2; Jacks = 0 }
                :: { Jacks = 0 }
                :: _ -> true
            | _ -> false
            
        let DENSE_JUMPSTREAM : Pattern =
            function
            |      { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: _ -> true
            | _ -> false

        let DOUBLE_JUMPSTREAM : Pattern =
            function
            |      { Notes = 1; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: _ -> true
            | _ -> false

        let TRIPLE_JUMPSTREAM : Pattern =
            function
            |      { Notes = 1; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: _ -> true
            | _ -> false
        
        let JUMPTRILL : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Roll = true }
                :: { Notes = 2; Roll = true }
                :: { Notes = 2; Roll = true }
                :: _ -> true
            | _ -> false

        let SPLITTRILL : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Roll = false }
                :: { Notes = 2; Jacks = 0; Roll = false }
                :: _ -> true
            | _ -> false

        let ROLL : Pattern =
            function
            |      { Notes = 1; Direction = Direction.Left }
                :: { Notes = 1; Direction = Direction.Left }
                :: { Notes = 1; Direction = Direction.Left }
                :: _ -> true
            |      { Notes = 1; Direction = Direction.Right }
                :: { Notes = 1; Direction = Direction.Right }
                :: { Notes = 1; Direction = Direction.Right }
                :: _ -> true
            | _ -> false

        let JUMPJACKS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 2 }
                :: _ -> true
            | _ -> false

        let JUMPGLUTS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 1 }
                :: _ -> true
            | _ -> false

    module SevenKey = 

        let DOUBLE_STAIRS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Direction = Direction.Left; Roll = false }
                :: _ -> true
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Direction = Direction.Right; Roll = false }
                :: _ -> true
            | _ -> false
            
        let DOUBLE_STREAMS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Roll = false }
                :: _ -> true
            | _ -> false

        let CHORDSTREAM : Pattern =
            function
            |      { Notes = x }
                :: { Notes = y; Jacks = 0 }
                :: _ when x > 1 && y > 1 -> true
            | _ -> false

        let CHORD_ROLL : Pattern =
            function
            |      { Notes = x }
                :: { Notes = y; Direction = Direction.Left; Roll = true }
                :: { Notes = z; Direction = Direction.Left; Roll = true }
                :: _ when x > 1 && y > 1 && z > 1 -> true
            |      { Notes = x }
                :: { Notes = y; Direction = Direction.Right; Roll = true }
                :: { Notes = z; Direction = Direction.Right; Roll = true }
                :: _ when x > 1 && y > 1 && z > 1 -> true
            | _ -> false

    let matches (patterns: IDictionary<string, Pattern>) (data: RowInfo list) : (string * (Time * float32<beat/minute>)) seq =
        let mutable data = data
        seq {
            while not data.IsEmpty do
                for pattern_name in patterns.Keys do
                    if patterns.[pattern_name] data then yield (pattern_name, (data.Head.Time, data.Head.BPM))
                data <- List.tail data
        }

    let analysis_4k = dict [
            "Streams", Common.STREAMS
            "Jumpstream", FourKey.JUMPSTREAM
            "Dense Jumpstream", FourKey.DENSE_JUMPSTREAM
            "Double Jumpstream", FourKey.DOUBLE_JUMPSTREAM
            "Triple Jumpstream", FourKey.TRIPLE_JUMPSTREAM
            "Jumptrill", FourKey.JUMPTRILL
            "Split trill", FourKey.SPLITTRILL
            "Roll", FourKey.ROLL
            "Handstream", FourKey.HANDSTREAM
            "Alternation", Common.ALTERNATION
            "Jacks", Common.JACKS
            "Jumpjacks", FourKey.JUMPJACKS
            "Chordjacks", Common.CHORDJACKS
            "Gluts", Common.GLUTS
            "Jumpgluts", FourKey.JUMPGLUTS
        ]

    let analysis_generic = dict [
            "Streams", Common.STREAMS
            "Alternation", Common.ALTERNATION
            "Jacks", Common.JACKS
            "Chordjacks", Common.CHORDJACKS
            "Gluts", Common.GLUTS
        ]

    let analysis_7k = dict [
            "Streams", Common.STREAMS
            "Chordstream", SevenKey.CHORDSTREAM
            "Double streams", SevenKey.DOUBLE_STREAMS
            "Double stairs", SevenKey.DOUBLE_STAIRS
            "Chord rolls", SevenKey.CHORD_ROLL
            "Alternation", Common.ALTERNATION
            "Jacks", Common.JACKS
            "Chordjacks", Common.CHORDJACKS
            "Gluts", Common.GLUTS
        ]

    let display = dict [
            "Streams", (Color.Green, 120, 400)
            "Alternation", (Color.Cyan, 100, 300)
            "Jumpstream", (Color.SkyBlue, 100, 350)
            "Dense Jumpstream", (Color.DeepSkyBlue, 100, 350)
            "Double Jumpstream", (Color.CadetBlue, 100, 300)
            "Triple Jumpstream", (Color.Aqua, 100, 300)
            "Handstream", (Color.Orange, 100, 300)
            "Chordstream", (Color.Orange, 100, 250)
            "Double streams", (Color.Yellow, 100, 250)
            "Double stairs", (Color.OrangeRed, 100, 250)
            "Chord rolls", (Color.Yellow, 100, 250)
            "Split trill", (Color.Purple, 100, 400)
            "Jumptrill", (Color.Blue, 100, 400)
            "Roll", (Color.Lime, 100, 400)
            "Jacks", (Color.PaleVioletRed, 100, 200)
            "Jumpjacks", (Color.Lavender, 100, 200)
            "Chordjacks", (Color.Red, 100, 200)
            "Gluts", (Color.Crimson, 100, 200)
            "Jumpgluts", (Color.Magenta, 100, 200)
        ]

    let analyse (chart: Chart) =
        let data = Analysis.run chart
        if chart.Keys = 4 then matches analysis_4k data
        elif chart.Keys = 7 then matches analysis_7k data
        else matches analysis_generic data