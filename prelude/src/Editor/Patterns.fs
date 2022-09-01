namespace Prelude.Editor.Patterns

open Prelude.Common
open Prelude.ChartFormats.Interlude

(*
    This file is a WIP, will change significantly before it is used
*)

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

type P1 =
    {
        Primitive: Primitives
        BPM: int
        Holding: int
        Releasing: int
        Timestamp: Time
    }
    
[<RequireQualifiedAccess>]
type RowTag =
    | None = 0
    | Stream = 1
    | Jack = 2
    | Chord = 3
    | Chords = 4
    | Only_Releases = 5

type RowInfo =
    {
        mutable Notes: NoteType array
        mutable Tag: RowTag
        // other vars go here
        mutable Count: int
        mutable MCount: int
        mutable RCount: int
        mutable JCount: int
        mutable TCount: int
        mutable TimeSince: Time
    }
    static member Blank =
        { 
            Notes = Array.empty
            Tag = RowTag.None
            Count = 0
            MCount = 0
            RCount = 0
            JCount = 0
            TCount = 0
            TimeSince = 0.0f<ms>
        }

type Buffer =
    {
        Size: int
        Contents: RowInfo array
        mutable Tail: int // Index of oldest item
    }

module Buffer =

    let create (length: int) = { Size = length; Contents = Array.init length (fun i -> RowInfo.Blank); Tail = 0 }

    let tail (buf: Buffer) = buf.Contents.[buf.Tail]

    let push (nr: NoteRow) (buf: Buffer) : RowInfo =
        let on = buf.Tail
        let n = (buf.Tail + 1) % buf.Size
        let head = buf.Contents.[buf.Tail]
        head.Notes <- nr

        head.Count <- 0
        head.MCount <- 0
        head.RCount <- 0
        for nt in nr do
            match nt with
            | NoteType.NORMAL
            | NoteType.HOLDHEAD -> head.Count <- head.Count + 1
            | NoteType.HOLDBODY -> head.MCount <- head.MCount + 1
            | NoteType.HOLDTAIL -> head.RCount <- head.RCount + 1
            | NoteType.NOTHING
            | _ -> ()
        head.JCount <- 0
        head.TCount <- 0
        buf.Tail <- n
        buf.Contents.[on]

module Analysis =

    let run (notes: TimeData<NoteRow>) (emit: P1 -> unit) =
        
        let emitter (inf: RowInfo) =
            printfn "%s" (NoteRow.prettyPrint inf.Notes)
            let e prim = 
                emit { 
                    Primitive = prim
                    BPM = 15000.0f<ms/minute> / inf.TimeSince |> int
                    Holding = inf.MCount
                    Releasing = inf.RCount
                    Timestamp = 0.0f<ms> //nyi
                }
            match inf.Tag with
            | RowTag.Stream -> e (Stream 1) // stair length nyi
            | RowTag.Chord -> e (Chord (inf.Count, 1)) // spacing nyi
            | RowTag.Only_Releases -> e Release
            | _ -> ()

        let buf = Buffer.create 10

        let mutable lastTime = System.Single.MinValue * 1.0f<ms>
        
        for (time, nr) in notes.Data do
            
            let newRow = Buffer.push nr buf
            newRow.TimeSince <- time - lastTime
            lastTime <- time
            
            match newRow.Count with
            | 0 -> newRow.Tag <- RowTag.Only_Releases
            | 1 -> newRow.Tag <- RowTag.Stream
            | _ -> newRow.Tag <- RowTag.Chord

            // jacks not yet

            let tail = Buffer.tail buf
            emitter tail
        ()

    let test (chart: Chart) =
        run chart.Notes (printfn "%A")