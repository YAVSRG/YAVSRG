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

    module Common =
        
        let STREAM : Pattern = 
            function
            |      { Notes = 1; Jacks = 0; RawNotes = x }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 1; Jacks = 0; RawNotes = y }
                :: _ when x.[0] <> y.[0] -> 5
            | _ -> 0

        let ALTERNATION : Pattern =
            function 
            |      { Jacks = 0; Direction = Direction.Right }
                :: { Jacks = 0; Direction = Direction.Left }
                :: _ -> 2
            |      { Jacks = 0; Direction = Direction.Left }
                :: { Jacks = 0; Direction = Direction.Right }
                :: _ -> 2
            | _ -> 0

        let JACKS : Pattern = 
            function
            |   { Jacks = x }
                :: _ when x > 1 -> 1
            | _ -> 0

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

    module ``4K`` =

        let CHORDSTREAM : Pattern =
            function
            |      { Notes = x; Jacks = 0 }
                :: { Jacks = 0 }
                :: { Jacks = 0 }
                :: { Jacks = 0 }
                :: _ when x > 1 -> 4
            | _ -> 0

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
            
        let DENSE_JUMPSTREAM : Pattern =
            function
            |      { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: { Notes = 2; Jacks = 0 }
                :: { Notes = 1; Jacks = 0 }
                :: _ -> 4
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

        let JUMPJACKS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 2 }
                :: _ -> 2
            | _ -> 0

        let JUMPGLUTS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 1 }
                :: _ -> 2
            | _ -> 0

    module ``7K`` = 

        let DOUBLE_STAIRS : Pattern =
            function
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Direction = Direction.Left; Roll = false }
                :: _ -> 2
            |      { Notes = 2 }
                :: { Notes = 2; Jacks = 0; Direction = Direction.Right; Roll = false }
                :: _ -> 2
            | _ -> 0
            
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

    type MatchedPattern = { Time: ScaledTime; MsPerBeat: float32<ms/beat>; Density: float32; Mixed: bool }

    let private PATTERN_STABILITY_THRESHOLD = 5.0f<ms/beat>
    let matches (patterns: IDictionary<PatternId, Pattern>) (data: RowInfo list) : (PatternId * MatchedPattern) seq =
        let mutable data = data
        seq {
            while not data.IsEmpty do
                for pattern_name in patterns.Keys do
                    match patterns.[pattern_name] data with
                    | 0 -> ()
                    | 1 -> 
                        yield (pattern_name, {
                            Time = data.Head.Time
                            MsPerBeat = data.Head.MsPerBeat
                            Density = data.Head.Density
                            Mixed = false
                        })
                    | n ->
                        let d = List.take n data
                        let mean_mspb = List.take n data |> List.averageBy (fun d -> d.MsPerBeat)

                        yield (pattern_name, { 
                            Time = data.Head.Time
                            MsPerBeat = mean_mspb
                            Density = d |> List.averageBy (fun d -> d.Density)
                            Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                        })
                data <- List.tail data
        }

    let analysis_4k = dict [
            Stream "Stream", Common.STREAM
            Stream "Chordstream", ``4K``.CHORDSTREAM
            Jack "Jacks", Common.JACKS
            // todo: mixed streams
        ]

    let analysis_generic = dict [
            Stream "Streams", Common.STREAM
            Stream "Light chordstream", ``7K``.LIGHT_CHORDSTREAM
            Stream "Dense chordstream", ``7K``.DENSE_CHORDSTREAM
            Jack "Jacks", Common.JACKS
            Jack "Chordjacks", Common.CHORDJACKS
            Jack "Gluts", Common.GLUTS
        ]

    let analysis_7k = dict [
            Stream "Streams", Common.STREAM
            Stream "Light chordstream", ``7K``.LIGHT_CHORDSTREAM
            Stream "Dense chordstream", ``7K``.DENSE_CHORDSTREAM
            Stream "Double streams", ``7K``.DOUBLE_STREAMS
            Stream "Double stairs", ``7K``.DOUBLE_STAIRS
            Stream "Chord rolls", ``7K``.CHORD_ROLL
            Jack "Jacks", Common.JACKS
            Jack "Chordjacks", Common.CHORDJACKS
            Jack "Gluts", Common.GLUTS
        ]

    let analyse (rate: float32) (chart: Chart) : (PatternId * MatchedPattern) array =
        let data = Analysis.run rate chart
        if chart.Keys = 4 then matches analysis_4k data
        elif chart.Keys = 7 then matches analysis_7k data
        else matches analysis_generic data
        |> Array.ofSeq