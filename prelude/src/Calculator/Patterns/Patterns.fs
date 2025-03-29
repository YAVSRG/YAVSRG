namespace Prelude.Calculator.Patterns

open Percyqaz.Data
open Prelude

// This file stores the definition of what patterns can be recognised
// The logic to actually "recognise" them and spit them out as data is in FindPatterns.fs

[<Json.AutoCodec>]
type CorePattern =
    | Stream
    | Chordstream
    | Jacks

    member this.DensityToBPM =
        match this with
        | Stream -> 52.5f<beat / minute>
        | Chordstream -> 35f<beat / minute>
        | Jacks -> 17.5f<beat / minute>

    member this.RatingMultiplier =
        match this with
        | Stream -> 1f / 3f
        | Chordstream -> 0.5f
        | Jacks -> 1.0f

    member this.AccuracyBreakpoints =
        match this with
        | Stream -> ( 0.97, 0.935, 0.90 )
        | Chordstream -> ( 0.98, 0.95, 0.91 )
        | Jacks -> ( 0.99, 0.96, 0.93 )

type PatternRecogniser = RowInfo list -> int

module Core =

    let STREAM : PatternRecogniser =
        function
        |      { Notes = 1; Jacks = 0; RawNotes = x }
            :: { Notes = 1; Jacks = 0 }
            :: { Notes = 1; Jacks = 0 }
            :: { Notes = 1; Jacks = 0 }
            :: { Notes = 1; Jacks = 0; RawNotes = y }
            :: _ when x.[0] <> y.[0] -> 5
        | _ -> 0

    let JACKS : PatternRecogniser =
        function
        |   { Jacks = x; MsPerBeat = mspb }
            :: _ when x > 1 && mspb < 2000.0f<_> -> 1
        | _ -> 0

    let CHORDSTREAM : PatternRecogniser =
        function
        |      { Notes = a; Jacks = 0 }
            :: { Notes = b; Jacks = 0 }
            :: { Notes = c; Jacks = 0 }
            :: { Notes = d; Jacks = 0 }
            :: _ when a > 1 && (b > 1 || c > 1 || d > 1) -> 4
        | _ -> 0

module Jacks =

    let CHORDJACKS : PatternRecogniser =
        function
        |   { Notes = a }
            :: { Notes = b; Jacks = j }
            :: _ when a > 2 && b > 1 && j >= 1 && (b < a || j < b) -> 2
        | _ -> 0

    let MINIJACKS : PatternRecogniser =
        function
        |   { Jacks = x }
            :: { Jacks = 0 }
            :: _ when x > 0 -> 2
        | _ -> 0

    let LONGJACKS : PatternRecogniser =
        function
        |   { Jacks = a; RawNotes = ra }
            :: { Jacks = b; RawNotes = rb }
            :: { Jacks = c; RawNotes = rc }
            :: { Jacks = d; RawNotes = rd }
            :: { Jacks = e; RawNotes = re }
            :: _ when a > 0 && b > 0 && c > 0 && d > 0 && e > 0 ->
                if Array.exists (fun x -> Array.contains x rb && Array.contains x rc && Array.contains x rd && Array.contains x re) ra then
                    5
                else
                    0
        | _ -> 0

module Jacks_4K =

    let QUADSTREAM : PatternRecogniser =
        function
        |   { Notes = 4 }
            :: _
            :: { Jacks = 0 }
            :: { Jacks = 0 } :: _ -> 4
        | _ -> 0

    let GLUTS : PatternRecogniser =
        function
        |   { RawNotes = ra }
            :: { Jacks = 1; RawNotes = rb }
            :: { Jacks = 1; RawNotes = rc }
            :: _ ->
                if Array.exists (fun x -> Array.contains x rb && Array.contains x rc) ra then
                    0
                else
                    3
        | _ -> 0

module Chordstream_4K =

    let HANDSTREAM : PatternRecogniser =
        function
        |      { Notes = 3; Jacks = 0 }
            :: { Jacks = 0 }
            :: { Jacks = 0 }
            :: { Jacks = 0 }
            :: _ -> 4
        | _ -> 0

    let JUMPSTREAM : PatternRecogniser =
        function
        |      { Notes = 2; Jacks = 0 }
            :: { Notes = 1; Jacks = 0 }
            :: { Notes = a; Jacks = 0 }
            :: { Notes = b; Jacks = 0 }
            :: _ when a < 3 && b < 3 -> 4
        | _ -> 0

    let DOUBLE_JUMPSTREAM : PatternRecogniser =
        function
        |      { Notes = 1; Jacks = 0 }
            :: { Notes = 2; Jacks = 0 }
            :: { Notes = 2; Jacks = 0 }
            :: { Notes = 1; Jacks = 0 }
            :: _ -> 4
        | _ -> 0

    let TRIPLE_JUMPSTREAM : PatternRecogniser =
        function
        |      { Notes = 1; Jacks = 0 }
            :: { Notes = 2; Jacks = 0 }
            :: { Notes = 2; Jacks = 0 }
            :: { Notes = 2; Jacks = 0 }
            :: { Notes = 1; Jacks = 0 }
            :: _ -> 4
        | _ -> 0

    let JUMPTRILL : PatternRecogniser =
        function
        |      { Notes = 2 }
            :: { Notes = 2; Roll = true }
            :: { Notes = 2; Roll = true }
            :: { Notes = 2; Roll = true }
            :: _ -> 4
        | _ -> 0

    let SPLITTRILL : PatternRecogniser =
        function
        |      { Notes = 2 }
            :: { Notes = 2; Jacks = 0; Roll = false }
            :: { Notes = 2; Jacks = 0; Roll = false }
            :: _ -> 3
        | _ -> 0

module Stream_4K =

    let ROLL : PatternRecogniser =
        function
        |      { Notes = 1; Direction = Direction.Left }
            :: { Notes = 1; Direction = Direction.Left }
            :: { Notes = 1; Direction = Direction.Left }
            :: _ -> 3
        |      { Notes = 1; Direction = Direction.Right }
            :: { Notes = 1; Direction = Direction.Right }
            :: { Notes = 1; Direction = Direction.Right }
            :: _ -> 3
        | _ -> 0

    let TRILL : PatternRecogniser =
        function
        |      { RawNotes = a }
            :: { RawNotes = b; Jacks = 0 }
            :: { RawNotes = c; Jacks = 0 }
            :: { RawNotes = d; Jacks = 0 }
            :: _ when a = c && b = d -> 4
        | _ -> 0

    let MINITRILL : PatternRecogniser =
        function
        |      { RawNotes = a }
            :: { RawNotes = b; Jacks = 0 }
            :: { RawNotes = c; Jacks = 0 }
            :: { RawNotes = d }
            :: _ when a = c && b <> d -> 4
        | _ -> 0

module Chordstream_7K =

    let DOUBLE_STREAMS : PatternRecogniser =
        function
        |      { Notes = 2 }
            :: { Notes = 2; Jacks = 0; Roll = false }
            :: _ -> 2
        | _ -> 0

    let DENSE_CHORDSTREAM : PatternRecogniser =
        function
        |      { Notes = x }
            :: { Notes = y; Jacks = 0 }
            :: _ when x > 1 && y > 1 -> 2
        | _ -> 0

    let LIGHT_CHORDSTREAM : PatternRecogniser =
        function
        |      { Notes = x }
            :: { Notes = y; Jacks = 0 }
            :: _ when x > 1 && y = 1 -> 2
        | _ -> 0

    let CHORD_ROLL : PatternRecogniser =
        function
        |      { Notes = x }
            :: { Notes = y; Direction = Direction.Left; Roll = true }
            :: { Notes = z; Direction = Direction.Left; Roll = true }
            :: _ when x > 1 && y > 1 && z > 1 -> 3
        |      { Notes = x }
            :: { Notes = y; Direction = Direction.Right; Roll = true }
            :: { Notes = z; Direction = Direction.Right; Roll = true }
            :: _ when x > 1 && y > 1 && z > 1 -> 3
        | _ -> 0

    let BRACKETS : PatternRecogniser =
        function
        |      { Notes = x }
            :: { Notes = y; Roll = false; Jacks = 0 }
            :: { Notes = z; Roll = false; Jacks = 0 }
            :: _ when x > 2 && y > 2 && z > 2 && x + y + z > 9 -> 3
        | _ -> 0

module Chordstream_Other =

    let DOUBLE_STREAMS : PatternRecogniser =
        function
        |      { Notes = 2 }
            :: { Notes = 2; Jacks = 0; Roll = false }
            :: _ -> 2
        | _ -> 0

    let DENSE_CHORDSTREAM : PatternRecogniser =
        function
        |      { Notes = x }
            :: { Notes = y; Jacks = 0 }
            :: _ when x > 1 && y > 1 -> 2
        | _ -> 0

    let LIGHT_CHORDSTREAM : PatternRecogniser =
        function
        |      { Notes = x }
            :: { Notes = y; Jacks = 0 }
            :: _ when x > 1 && y = 1 -> 2
        | _ -> 0

    let CHORD_ROLL : PatternRecogniser =
        function
        |      { Notes = x }
            :: { Notes = y; Direction = Direction.Left; Roll = true }
            :: { Notes = z; Direction = Direction.Left; Roll = true }
            :: _ when x > 1 && y > 1 && z > 1 -> 3
        |      { Notes = x }
            :: { Notes = y; Direction = Direction.Right; Roll = true }
            :: { Notes = z; Direction = Direction.Right; Roll = true }
            :: _ when x > 1 && y > 1 && z > 1 -> 3
        | _ -> 0

type SpecificPatterns =
    {
        Stream: (string * PatternRecogniser) list
        Chordstream: (string * PatternRecogniser) list
        Jack: (string * PatternRecogniser) list
    }
    static member SPECIFIC_4K =
        {
            Stream =
                [
                    "Rolls", Stream_4K.ROLL
                    "Trills", Stream_4K.TRILL
                    "Minitrills", Stream_4K.MINITRILL
                ]
            Chordstream =
                [
                    "Handstream", Chordstream_4K.HANDSTREAM
                    "Split Trill", Chordstream_4K.SPLITTRILL
                    "Jumptrill", Chordstream_4K.JUMPTRILL
                    //"Triple Jumpstream", Chordstream_4K.TRIPLE_JUMPSTREAM
                    //"Double Jumpstream", Chordstream_4K.DOUBLE_JUMPSTREAM
                    "Jumpstream", Chordstream_4K.JUMPSTREAM
                ]
            Jack =
                [
                    "Longjacks", Jacks.LONGJACKS
                    "Quadstream", Jacks_4K.QUADSTREAM
                    "Gluts", Jacks_4K.GLUTS
                    "Chordjacks", Jacks.CHORDJACKS
                    "Minijacks", Jacks.MINIJACKS
                ]
        }

    static member SPECIFIC_7K =
        {
            // todo: roll, trill
            Stream = []
            Chordstream =
                [
                    "Brackets", Chordstream_7K.BRACKETS
                    //"Chord Rolls", Chordstream_7K.CHORD_ROLL
                    "Double Stream", Chordstream_7K.DOUBLE_STREAMS
                    "Dense Chordstream", Chordstream_7K.DENSE_CHORDSTREAM
                    "Light Chordstream", Chordstream_7K.LIGHT_CHORDSTREAM
                ]
            Jack =
                [
                    "Longjacks", Jacks.LONGJACKS
                    "Chordjacks", Jacks.CHORDJACKS
                    "Minijacks", Jacks.MINIJACKS
                ]
        }

    static member SPECIFIC_OTHER =
        {
            // todo: roll, trill
            Stream = []
            Chordstream =
                [
                    "Chord Rolls", Chordstream_Other.CHORD_ROLL
                    "Double Stream", Chordstream_Other.DOUBLE_STREAMS
                    "Dense Chordstream", Chordstream_Other.DENSE_CHORDSTREAM
                    "Light Chordstream", Chordstream_Other.LIGHT_CHORDSTREAM
                ]
            Jack =
                [
                    "Longjacks", Jacks.LONGJACKS
                    "Chordjacks", Jacks.CHORDJACKS
                    "Minijacks", Jacks.MINIJACKS
                ]
        }