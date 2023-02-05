namespace Prelude.Charts.Tools.Patterns

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
                        Time = t - previous_time
                    }

                    previous_row <- current_row
                    previous_time <- t

        } |> List.ofSeq


type Pattern = RowInfo list -> bool

module Patterns =

    let matches (pattern: RowInfo list -> bool) (data: RowInfo list) : int =
        let mutable data = data
        let mutable matches = 0
        while not data.IsEmpty do
            if pattern data then matches <- matches + 1
            data <- List.tail data
        matches

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

        let QUAD : Pattern = function { Notes = 4 } :: _ -> true | _ -> false

        let HAND_CHORDJACK : Pattern = function { Notes = 3 } :: { Notes = 3; Jacks = x } :: _ when x < 3 -> true | _ -> false
        let HANDJACK : Pattern = function { Notes = 3 } :: { Notes = 3; Jacks = 3 } :: _ -> true | _ -> false

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

        let STREAM : Pattern = function { Jacks = 0 } :: _ -> true | _ -> false

    let analyse (chart: Chart) =
        let data = Analysis.run chart
        printfn "== %s ==" chart.Header.Title
        if chart.Keys = 4 then
            printfn "HANDSTREAM: %i" (matches FourKey.HANDSTREAM data)
            printfn "JUMPSTREAM: %i" (matches FourKey.JUMPSTREAM data)
            printfn "DENSE_JUMPSTREAM: %i" (matches FourKey.DENSE_JUMPSTREAM data)
            printfn "DOUBLE_JUMPSTREAM: %i" (matches FourKey.DOUBLE_JUMPSTREAM data)
            printfn "TRIPLE_JUMPSTREAM: %i" (matches FourKey.TRIPLE_JUMPSTREAM data)
            printfn "JUMPTRILL: %i" (matches FourKey.JUMPTRILL data)
            printfn "SPLITTRILL: %i" (matches FourKey.SPLITTRILL data)
            printfn "ROLL: %i" (matches FourKey.ROLL data)
            printfn "QUAD: %i" (matches FourKey.QUAD data)
            printfn "HANDJACK: %i" (matches FourKey.HANDJACK data)
            printfn "HAND_CHORDJACK: %i" (matches FourKey.HAND_CHORDJACK data)

        if chart.Keys = 7 then
            printfn "DOUBLE_STAIRS: %i" (matches SevenKey.DOUBLE_STAIRS data)
            printfn "DOUBLE_STREAMS: %i" (matches SevenKey.DOUBLE_STREAMS data)
            printfn "CHORDSTREAM: %i" (matches SevenKey.CHORDSTREAM data)
            printfn "STREAM: %i" (matches SevenKey.STREAM data)