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

module PatternFinder =
    
    let private PATTERN_STABILITY_THRESHOLD = 5.0f<ms/beat>

    let private matches (specific_patterns: SpecificPatterns) (full_data: RowInfo list) : MatchedPattern array =
        let mutable remaining_data = full_data

        let results = ResizeArray()

        let specific_type (specific_types: (string * PatternRecogniser) list) =
            specific_types
            |> List.tryFind (fun (name, p) -> p remaining_data > 0)
            |> Option.map fst

        while not remaining_data.IsEmpty do
            match Core.STREAM remaining_data with
            | 0 -> ()
            | 1 -> 
                results.Add {
                    Pattern = Stream
                    SpecificType = specific_type specific_patterns.Stream
                    Time = remaining_data.Head.Time
                    MsPerBeat = remaining_data.Head.MsPerBeat
                    Density = remaining_data.Head.Density
                    Mixed = false
                }
            | n ->
                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Stream
                    SpecificType = specific_type specific_patterns.Stream
                    Time = remaining_data.Head.Time
                    MsPerBeat = mean_mspb
                    Density = d |> List.averageBy _.Density
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                }

            
            match Core.CHORDSTREAM remaining_data with
            | 0 -> ()
            | 1 -> 
                results.Add {
                    Pattern = Chordstream
                    SpecificType = specific_type specific_patterns.Chordstream
                    Time = remaining_data.Head.Time
                    MsPerBeat = remaining_data.Head.MsPerBeat
                    Density = remaining_data.Head.Density
                    Mixed = false
                }
            | n ->
                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Chordstream
                    SpecificType = specific_type specific_patterns.Chordstream
                    Time = remaining_data.Head.Time
                    MsPerBeat = mean_mspb
                    Density = d |> List.averageBy _.Density
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                }

            
            match Core.JACKS remaining_data with
            | 0 -> ()
            | 1 -> 
                results.Add {
                    Pattern = Jack
                    SpecificType = specific_type specific_patterns.Jack
                    Time = remaining_data.Head.Time
                    MsPerBeat = remaining_data.Head.MsPerBeat
                    Density = remaining_data.Head.Density
                    Mixed = false
                }
            | n ->
                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Jack
                    SpecificType = specific_type specific_patterns.Jack
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
            matches SpecificPatterns.SPECIFIC_4K primitives
        elif chart.Keys = 7 then 
            matches SpecificPatterns.SPECIFIC_7K primitives
        else
            matches SpecificPatterns.SPECIFIC_OTHER primitives