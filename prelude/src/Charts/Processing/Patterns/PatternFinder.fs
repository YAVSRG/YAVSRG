namespace Prelude.Charts.Processing.Patterns

open Percyqaz.Data
open Prelude
open Prelude.Charts

[<Json.AutoCodec>]
type CorePattern =
    | Stream
    | Chordstream
    | Jack
    member this.DensityToBPM =
        match this with
        | Stream -> 52.5f<beat / minute>
        | Chordstream -> 35f<beat / minute>
        | Jack -> 17.5f<beat / minute>
    member this.RatingMultiplier =
        match this with
        | Stream -> 1f / 3f
        | Chordstream -> 0.5f
        | Jack -> 1.0f
    member this.AccuracyBreakpoints =
        match this with
        | Stream -> ( 0.98, 0.94, 0.90 )
        | Chordstream -> ( 0.985, 0.95, 0.915 )
        | Jack -> ( 0.99, 0.96, 0.93 )

type MatchedPattern = 
    { 
        Pattern: CorePattern
        SpecificType: string option
        Time: Time
        MsPerBeat: float32<ms/beat>
        Density: float32</rate>
        Mixed: bool
    }

type PatternRecogniser = RowInfo list -> int

module PatternFinder =

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
            |   { Jacks = x }
                :: _ when x > 1 -> 1
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

        let GLUTS : PatternRecogniser =
            function
            |   { Notes = a }
                :: { Jacks = 1 } 
                :: _ when a > 1 -> 2
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
                :: { Jacks = 1 } 
                :: { Jacks = 0 }
                :: { Jacks = 0 } :: _ -> 4
            | _ -> 0

        // todo: identify anchors

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

    let CORE_PATTERNS = [|
        Stream, Core.STREAM
        Chordstream, Core.CHORDSTREAM
        Jack, Core.JACKS
    |]

    let SPECIFIC_PATTERNS_4K = [|
        Stream, "Rolls", Stream_4K.ROLL
        Stream, "Minitrills", Stream_4K.MINITRILL
        Stream, "Trills", Stream_4K.TRILL

        Chordstream, "Jumpstream", Chordstream_4K.JUMPSTREAM
        Chordstream, "Double jumpstream", Chordstream_4K.DOUBLE_JUMPSTREAM
        Chordstream, "Triple jumpstream", Chordstream_4K.TRIPLE_JUMPSTREAM
        Chordstream, "Jumptrill", Chordstream_4K.JUMPTRILL
        Chordstream, "Split trill", Chordstream_4K.SPLITTRILL
        Chordstream, "Handstream", Chordstream_4K.HANDSTREAM

        Jack, "Chordjacks", Jacks.CHORDJACKS
        Jack, "Gluts", Jacks.GLUTS
        Jack, "Minijacks", Jacks.MINIJACKS
        Jack, "Longjacks", Jacks.LONGJACKS
        Jack, "Quadstream", Jacks_4K.QUADSTREAM
    |]
    
    let SPECIFIC_PATTERNS_7K = [|
        Chordstream, "Brackets", Chordstream_7K.BRACKETS
        Chordstream, "Light chordstream", Chordstream_7K.LIGHT_CHORDSTREAM
        Chordstream, "Dense chordstream", Chordstream_7K.DENSE_CHORDSTREAM
        Chordstream, "Chord rolls", Chordstream_7K.CHORD_ROLL
        Chordstream, "Double streams", Chordstream_7K.DOUBLE_STREAMS
    
        Jack, "Chordjacks", Jacks.CHORDJACKS
        Jack, "Minijacks", Jacks.MINIJACKS
        Jack, "Longjacks", Jacks.LONGJACKS
    |]
    
    let SPECIFIC_PATTERNS_OTHER = [|
        Chordstream, "Light chordstream", Chordstream_Other.LIGHT_CHORDSTREAM
        Chordstream, "Dense chordstream", Chordstream_Other.DENSE_CHORDSTREAM
        Chordstream, "Chord rolls", Chordstream_Other.CHORD_ROLL
        Chordstream, "Double streams", Chordstream_Other.DOUBLE_STREAMS
    
        Jack, "Chordjacks", Jacks.CHORDJACKS
        Jack, "Minijacks", Jacks.MINIJACKS
        Jack, "Longjacks", Jacks.LONGJACKS
    |]
    
    let private PATTERN_STABILITY_THRESHOLD = 5.0f<ms/beat>

    let private matches (specific_patterns: (CorePattern * string * PatternRecogniser) array) (full_data: RowInfo list) : MatchedPattern array =
        let mutable remaining_data = full_data

        let results = ResizeArray()

        while not remaining_data.IsEmpty do
            for pattern_type, pattern in CORE_PATTERNS do
                match pattern remaining_data with
                | 0 -> ()
                | 1 -> 
                    results.Add {
                        Pattern = pattern_type
                        SpecificType = None
                        Time = remaining_data.Head.Time
                        MsPerBeat = remaining_data.Head.MsPerBeat
                        Density = remaining_data.Head.Density
                        Mixed = false
                    }
                | n ->
                    let d = List.take n remaining_data
                    let mean_mspb = d |> List.averageBy _.MsPerBeat

                    results.Add {
                        Pattern = pattern_type
                        SpecificType = None
                        Time = remaining_data.Head.Time
                        MsPerBeat = mean_mspb
                        Density = d |> List.averageBy _.Density
                        Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    }
            for pattern_type, specific_type, pattern in specific_patterns do
                match pattern remaining_data with
                | 0 -> ()
                | 1 -> 
                    results.Add {
                        Pattern = pattern_type
                        SpecificType = Some specific_type
                        Time = remaining_data.Head.Time
                        MsPerBeat = remaining_data.Head.MsPerBeat
                        Density = remaining_data.Head.Density
                        Mixed = false
                    }
                | n ->
                    let d = List.take n remaining_data
                    let mean_mspb = d |> List.averageBy _.MsPerBeat

                    results.Add { 
                        Pattern = pattern_type
                        SpecificType = Some specific_type
                        Time = remaining_data.Head.Time
                        MsPerBeat = mean_mspb
                        Density = d |> List.averageBy _.Density
                        Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    }
            remaining_data <- List.tail remaining_data

        results.ToArray()

    let find_patterns (chart: Chart) : Density array * MatchedPattern array =
        let density, primitives = Primitives.process_chart chart

        density,
        if chart.Keys = 4 then 
            matches SPECIFIC_PATTERNS_4K primitives
        elif chart.Keys = 7 then 
            matches SPECIFIC_PATTERNS_7K primitives
        else
            matches SPECIFIC_PATTERNS_OTHER primitives