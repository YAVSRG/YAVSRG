namespace Prelude.Charts.Tools.Patterns

open System.Collections.Generic
open Percyqaz.Data
open Prelude
open Prelude.Charts

[<Json.AutoCodec>]
type PatternId = 
    | Stream of string
    | Jack of string
    override this.ToString() = match this with Stream s | Jack s -> s
type Pattern = RowInfo list -> int

module Patterns =

    module Core =
        
        let STREAM : Pattern = 
            function
            |      { Notes = 1; Jacks = 0; RawNotes = x }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 1; Jacks = 0; RawNotes = y }
                :: _ when x.[0] <> y.[0] -> 5
            | _ -> 0

        let JACKS : Pattern = 
            function
            |   { Jacks = x }
                :: _ when x > 1 -> 1
            | _ -> 0

        let CHORDSTREAM : Pattern =
            function
            |      { Notes = a; Jacks = 0 }
                :: { Notes = b; Jacks = 0 }
                :: { Notes = c; Jacks = 0 }
                :: { Notes = d; Jacks = 0 }
                :: _ when a > 1 && (b > 1 || c > 1 || d > 1) -> 4
            | _ -> 0

    module Jacks =

        let CHORDJACKS : Pattern = 
            function
            |   { Notes = a }
                :: { Notes = b; Jacks = j }
                :: _ when a > 2 && b > 1 && j >= 1 && (b < a || j < b) -> 2
            | _ -> 0

        let GLUTS : Pattern =
            function
            |   { Notes = a }
                :: { Notes = b; Jacks = 1 } 
                :: _ when a > 1 && b > 1 -> 2
            | _ -> 0
            
        let MINIJACKS : Pattern =
            function
            |   { Jacks = x }
                :: { Jacks = 0 } 
                :: _ when x > 0 -> 2
            | _ -> 0

        // todo: identify anchors

    module Chordstream_4K =

        let HANDSTREAM : Pattern =
            function
            |      { Notes = 3; Jacks = 0 }
                :: { Jacks = 0 }
                :: { Jacks = 0 }
                :: { Jacks = 0 }
                :: _ -> 4
            | _ -> 0

        let JUMPSTREAM : Pattern =
            function
            |      { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = a; Jacks = 0 }
                :: { Notes = b; Jacks = 0 }
                :: _ when a < 3 && b < 3 -> 4
            | _ -> 0

        let DOUBLE_JUMPSTREAM : Pattern =
            function
            |      { Notes = 1; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: _ -> 4
            | _ -> 0

        let TRIPLE_JUMPSTREAM : Pattern =
            function
            |      { Notes = 1; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: _ -> 4
            | _ -> 0
        
        let JUMPTRILL : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Roll = true }
                :: { Notes = 2; Roll = true }
                :: { Notes = 2; Roll = true }
                :: _ -> 4
            | _ -> 0

        let SPLITTRILL : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Roll = false }
                :: { Notes = 2; Jacks = 0; Roll = false }
                :: _ -> 3
            | _ -> 0

    module Stream_4K =

        let ROLL : Pattern =
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

        let TRILL : Pattern =
            function
            |      { RawNotes = a }
                :: { RawNotes = b; Jacks = 0 }
                :: { RawNotes = c; Jacks = 0 }
                :: { RawNotes = d; Jacks = 0 }
                :: _ when a = c && b = d -> 4
            | _ -> 0
        
        let MINITRILL : Pattern =
            function
            |      { RawNotes = a }
                :: { RawNotes = b; Jacks = 0 }
                :: { RawNotes = c; Jacks = 0 }
                :: { RawNotes = d }
                :: _ when a = c && b <> d -> 4
            | _ -> 0

    module Chordstream_7K = 
            
        let DOUBLE_STREAMS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Roll = false }
                :: _ -> 2
            | _ -> 0

        let DENSE_CHORDSTREAM : Pattern =
            function
            |      { Notes = x }
                :: { Notes = y; Jacks = 0 }
                :: _ when x > 1 && y > 1 -> 2
            | _ -> 0
            
        let LIGHT_CHORDSTREAM : Pattern =
            function
            |      { Notes = x }
                :: { Notes = y; Jacks = 0 }
                :: _ when x > 1 && y = 1 -> 2
            | _ -> 0

        let CHORD_ROLL : Pattern =
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
        
        let BRACKETS : Pattern =
            function
            |      { Notes = x }
                :: { Notes = y; Roll = false; Jacks = 0 }
                :: { Notes = z; Roll = false; Jacks = 0 }
                :: _ when x > 2 && y > 2 && z > 2 && x + y + z > 9 -> 3
            | _ -> 0
    
    module Chordstream_Other = 
                
        let DOUBLE_STREAMS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Roll = false }
                :: _ -> 2
            | _ -> 0
    
        let DENSE_CHORDSTREAM : Pattern =
            function
            |      { Notes = x }
                :: { Notes = y; Jacks = 0 }
                :: _ when x > 1 && y > 1 -> 2
            | _ -> 0
                
        let LIGHT_CHORDSTREAM : Pattern =
            function
            |      { Notes = x }
                :: { Notes = y; Jacks = 0 }
                :: _ when x > 1 && y = 1 -> 2
            | _ -> 0
    
        let CHORD_ROLL : Pattern =
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

    type MatchedCorePattern = { Pattern: PatternId; Time: ScaledTime; MsPerBeat: float32<ms/beat>; Density: float32; Mixed: bool }
    type MatchedSpecificPattern = { Pattern: PatternId; Time: ScaledTime; MsPerBeat: float32<ms/beat> }

    let CORE_PATTERNS = [|
        Stream "Stream", Core.STREAM
        Stream "Chordstream", Core.CHORDSTREAM
        Jack "Jacks", Core.JACKS
    |]

    let SPECIFIC_PATTERNS_4K = [|
        Stream "Rolls", Stream_4K.ROLL
        Stream "Minitrills", Stream_4K.MINITRILL
        Stream "Trills", Stream_4K.TRILL

        Stream "Jumpstream", Chordstream_4K.JUMPSTREAM
        Stream "Double jumpstream", Chordstream_4K.DOUBLE_JUMPSTREAM
        Stream "Triple jumpstream", Chordstream_4K.TRIPLE_JUMPSTREAM
        Stream "Jumptrill", Chordstream_4K.JUMPTRILL
        Stream "Split trill", Chordstream_4K.SPLITTRILL
        Stream "Handstream", Chordstream_4K.HANDSTREAM

        Jack "Chordjacks", Jacks.CHORDJACKS
        Jack "Gluts", Jacks.GLUTS
        Jack "Minijacks", Jacks.MINIJACKS
    |]
    
    let SPECIFIC_PATTERNS_7K = [|
        Stream "Brackets", Chordstream_7K.BRACKETS
        Stream "Light chordstream", Chordstream_7K.LIGHT_CHORDSTREAM
        Stream "Dense chordstream", Chordstream_7K.DENSE_CHORDSTREAM
        Stream "Chord rolls", Chordstream_7K.CHORD_ROLL
        Stream "Double streams", Chordstream_7K.DOUBLE_STREAMS
    
        Jack "Chordjacks", Jacks.CHORDJACKS
        Jack "Minijacks", Jacks.MINIJACKS
    |]
    
    let SPECIFIC_PATTERNS_OTHER = [|
        Stream "Light chordstream", Chordstream_Other.LIGHT_CHORDSTREAM
        Stream "Dense chordstream", Chordstream_Other.DENSE_CHORDSTREAM
        Stream "Chord rolls", Chordstream_Other.CHORD_ROLL
        Stream "Double streams", Chordstream_Other.DOUBLE_STREAMS
    
        Jack "Chordjacks", Jacks.CHORDJACKS
        Jack "Minijacks", Jacks.MINIJACKS
    |]
    
    
    let private PATTERN_STABILITY_THRESHOLD = 5.0f<ms/beat>

    let private matches (specific_patterns: (PatternId * Pattern) array) (data: RowInfo list) : MatchedCorePattern array * MatchedSpecificPattern array =
        let mutable data = data

        let core_matches = ResizeArray()
        let specific_matches = ResizeArray()

        while not data.IsEmpty do
            for pattern_id, pattern in CORE_PATTERNS do
                match pattern data with
                | 0 -> ()
                | 1 -> 
                    core_matches.Add {
                        Pattern = pattern_id
                        Time = data.Head.Time
                        MsPerBeat = data.Head.MsPerBeat
                        Density = data.Head.Density
                        Mixed = false
                    }
                | n ->
                    let d = List.take n data
                    let mean_mspb = List.take n data |> List.averageBy (fun d -> d.MsPerBeat)

                    core_matches.Add {
                        Pattern = pattern_id
                        Time = data.Head.Time
                        MsPerBeat = mean_mspb
                        Density = d |> List.averageBy (fun d -> d.Density)
                        Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    }
            for pattern_id, pattern in specific_patterns do
                match pattern data with
                | 0 -> ()
                | 1 -> 
                    specific_matches.Add {
                        Pattern = pattern_id
                        Time = data.Head.Time
                        MsPerBeat = data.Head.MsPerBeat
                    }
                | n ->
                    let d = List.take n data
                    let mean_mspb = List.take n data |> List.averageBy (fun d -> d.MsPerBeat)

                    specific_matches.Add { 
                        Pattern = pattern_id
                        Time = data.Head.Time
                        MsPerBeat = mean_mspb
                    }
            data <- List.tail data

        core_matches.ToArray(), specific_matches.ToArray()

    let analyse (rate: float32) (chart: Chart) : MatchedCorePattern array =
        let data = Analysis.run rate chart
        if chart.Keys = 4 then 
            matches SPECIFIC_PATTERNS_4K data
        elif chart.Keys = 7 then 
            matches SPECIFIC_PATTERNS_7K data
        else
            matches SPECIFIC_PATTERNS_OTHER data
        |> fst