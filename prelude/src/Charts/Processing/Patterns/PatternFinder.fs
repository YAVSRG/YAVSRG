namespace Prelude.Charts.Processing.Patterns

open Percyqaz.Data
open Prelude
open Prelude.Charts

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
        | Stream -> ( 0.98, 0.94, 0.90 )
        | Chordstream -> ( 0.985, 0.95, 0.915 )
        | Jacks -> ( 0.99, 0.96, 0.93 )

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

        while not remaining_data.IsEmpty do
            match Core.STREAM remaining_data with
            | 0 -> ()
            | n ->
                let n, specific_type =
                    specific_patterns.Stream
                    |> List.tryPick (fun (name, p) -> p remaining_data |> function 0 -> None | n -> Some (n, name))
                    |> function None -> (n, None) | Some (m, specific_type) -> max n m, Some specific_type

                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Stream
                    SpecificType = specific_type
                    Time = remaining_data.Head.Time
                    MsPerBeat = mean_mspb
                    Density = d |> List.averageBy _.Density
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                }

            
            match Core.CHORDSTREAM remaining_data with
            | 0 -> ()
            | n ->
                let n, specific_type =
                    specific_patterns.Chordstream
                    |> List.tryPick (fun (name, p) -> p remaining_data |> function 0 -> None | n -> Some (n, name))
                    |> function None -> (n, None) | Some (m, specific_type) -> max n m, Some specific_type

                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Chordstream
                    SpecificType = specific_type
                    Time = remaining_data.Head.Time
                    MsPerBeat = mean_mspb
                    Density = d |> List.averageBy _.Density
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                }

            
            match Core.JACKS remaining_data with
            | 0 -> ()
            | n ->
                let n, specific_type =
                    specific_patterns.Jack
                    |> List.tryPick (fun (name, p) -> p remaining_data |> function 0 -> None | n -> Some (n, name))
                    |> function None -> (n, None) | Some (m, specific_type) -> max n m, Some specific_type

                let d = List.take n remaining_data
                let mean_mspb = d |> List.averageBy _.MsPerBeat

                results.Add {
                    Pattern = Jacks
                    SpecificType = specific_type
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