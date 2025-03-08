namespace Prelude.Calculator.Patterns

open Prelude
open Prelude.Charts
open Prelude.Calculator

type FoundPattern =
    {
        Pattern: CorePattern
        SpecificType: string option
        Mixed: bool
        Start: Time
        End: Time
        MsPerBeat: float32<ms/beat>
        Strains: float32 array
        Density: float32</rate>
    }

module Patterns =

    let private PATTERN_STABILITY_THRESHOLD = 5.0f<ms/beat>
    let private matches (specific_patterns: SpecificPatterns) (last_note: Time, primitives: RowInfo list) : FoundPattern array =
        let mutable remaining_data = primitives

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
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    Start = remaining_data.Head.Time
                    End = remaining_data |> List.skip n |> List.tryHead |> function None -> last_note | Some r -> r.Time
                    MsPerBeat = mean_mspb
                    Strains = remaining_data.Head.Strains
                    Density = d |> List.averageBy _.Density
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
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    Start = remaining_data.Head.Time
                    End = remaining_data |> List.skip n |> List.tryHead |> function None -> last_note | Some r -> r.Time
                    MsPerBeat = mean_mspb
                    Strains = remaining_data.Head.Strains
                    Density = d |> List.averageBy _.Density
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
                    Mixed = d |> List.forall (fun d -> abs(d.MsPerBeat - mean_mspb) < PATTERN_STABILITY_THRESHOLD) |> not
                    Start = remaining_data.Head.Time
                    End =
                        max
                            (remaining_data.Head.Time + remaining_data.Head.MsPerBeat * 0.5f<beat>)
                            (remaining_data |> List.skip n |> List.tryHead |> function None -> last_note | Some r -> r.Time)
                    MsPerBeat = mean_mspb
                    Strains = remaining_data.Head.Strains
                    Density = d |> List.averageBy _.Density
                }

            remaining_data <- List.tail remaining_data

        results.ToArray()

    let find (density: Density array, difficulty_info: Difficulty, chart: Chart) : FoundPattern array =
        let primitives = Primitives.calculate (density, difficulty_info, chart)

        let keymode_patterns =
            if chart.Keys = 4 then
                SpecificPatterns.SPECIFIC_4K
            elif chart.Keys = 7 then
                SpecificPatterns.SPECIFIC_7K
            else
                SpecificPatterns.SPECIFIC_OTHER

        matches keymode_patterns (chart.LastNote, primitives)