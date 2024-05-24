namespace Prelude.Charts.Formats.Quaver

open System
open System.IO
open Percyqaz.Common

// Quaver files are YAML files

type QuaverHitObject =
    {
        StartTime: int
        Lane: int
        EndTime: int
    }

type QuaverTimingPoint =
    {
        StartTime: float32
        Bpm: float32
    }

type QuaverSliderVelocity =
    {
        StartTime: float32
        Multiplier: float32
    }

type QuaverChart =
    {
        AudioFile: string
        SongPreviewTime: int
        BackgroundFile: string
        MapId: int
        MapSetId: int
        Mode: int
        Title: string
        Artist: string
        Source: string
        Tags: string list
        Creator: string
        DifficultyName: string
        Description: string
        Genre: string
        BPMDoesNotAffectScrollVelocity: bool
        InitialScrollVelocity: float32
        HasScratchKey: bool
        TimingPoints: QuaverTimingPoint list
        SliderVelocities: QuaverSliderVelocity list
        HitObjects: QuaverHitObject list
    }

// only parses a smaller subset of yaml that quaver uses
module Yaml =
    type ParsedYamlNestedObject = Map<string, string>
    type ParsedYamlValue = String of string | Array of ResizeArray<ParsedYamlNestedObject>
    type ParsedYamlObject = Map<string, ParsedYamlValue>

    let rec private parse_nested_list (lines: string list) : ResizeArray<ParsedYamlNestedObject> * string list =
        let mutable lines_remaining = lines
        
        let mutable object : ParsedYamlNestedObject = Map.empty
        let parsed_objects = ResizeArray<ParsedYamlNestedObject>()

        let mutable end_of_list = lines_remaining = []

        let indent = (List.head lines_remaining).IndexOf("-")

        while lines_remaining <> [] && not end_of_list do
            let line = List.head lines_remaining
            lines_remaining <- List.tail lines_remaining

            let is_indented = line.StartsWith(" ") || line.StartsWith ("-")
            let is_new_item = line.Trim().StartsWith("-")

            if not is_indented then 
                end_of_list <- true
                lines_remaining <- line :: lines_remaining
            elif is_new_item && line.IndexOf("-") <> indent then
                end_of_list <- true
                lines_remaining <- line :: lines_remaining
            else
                if is_new_item && object <> Map.empty then
                    parsed_objects.Add object
                    object <- Map.empty

                let key_value = line.Trim().TrimStart('-').Trim().Split(":", 2)
                
                if key_value.[0] = "{}" then
                    ()
                elif key_value.Length > 1 && key_value.[1] <> "" then
                    object <- Map.add key_value.[0] (key_value.[1].Trim()) object
                else
                    let _, new_lines_remaining = parse_nested_list lines_remaining
                    lines_remaining <- new_lines_remaining

        parsed_objects.Add object
        parsed_objects, lines_remaining

    let from_lines (lines: string list) : ParsedYamlObject =
        let mutable object : ParsedYamlObject = Map.empty
        let mutable lines_remaining = lines
        while lines_remaining <> [] do
            let line = List.head lines_remaining
            lines_remaining <- List.tail lines_remaining

            let key_value = line.Trim().Split(':', 2)
            if key_value.Length > 1 && key_value.[1] <> "" then
                let stripped_value = key_value.[1].Trim().Trim('\'')
                let value = if stripped_value = "[]" then Array (ResizeArray<ParsedYamlNestedObject>()) else String stripped_value
                object <- Map.add key_value.[0] value object
            else
                let nested, ls = parse_nested_list lines_remaining
                object <- Map.add key_value.[0] (Array nested) object
                lines_remaining <- ls

        object

    let get_string (key: string) (parsed: ParsedYamlObject) : string =
        match parsed.TryFind key with
        | Some (String s) -> s
        | x -> failwithf "Expected string present for key '%s' but got %A" key x

    let get_string_or (key: string) (otherwise: string) (parsed: ParsedYamlObject) : string =
        match parsed.TryFind key with
        | Some (String s) -> s
        | x -> otherwise

    let get_int (key: string) (parsed: ParsedYamlObject) : int =
        match parsed.TryFind key with
        | Some (String s) -> 
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | x -> failwithf "Expected (integer valued) string present for key '%s' but got %A" key x

    let get_int_or (key: string) (otherwise: int) (parsed: ParsedYamlObject) : int =
        match parsed.TryFind key with
        | Some (String s) -> 
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | x -> otherwise

    let get_array<'T> (key: string) (parser: ParsedYamlNestedObject -> 'T) (parsed: ParsedYamlObject) : 'T list =
        match parsed.TryFind key with
        | Some (Array xs) -> Seq.map parser xs |> List.ofSeq
        | x -> failwithf "Expected array present for key '%s' but got %A" key x

    let get_nested_float (key: string) (parsed: ParsedYamlNestedObject) : float32 =
        match parsed.TryFind key with
        | Some s ->
            match Single.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected float '%s' for key '%s'" s key
        | None -> failwithf "Expected string present for key '%s'" key

    let get_nested_int (key: string) (parsed: ParsedYamlNestedObject) : int =
        match parsed.TryFind key with
        | Some s ->
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | None -> failwithf "Expected string present for key '%s'" key

    let get_nested_float_or (key: string) (otherwise: float32) (parsed: ParsedYamlNestedObject) : float32 =
        match parsed.TryFind key with
        | Some s ->
            match Single.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected float '%s' for key '%s'" s key
        | None -> otherwise

    let get_nested_int_or (key: string) (otherwise: int) (parsed: ParsedYamlNestedObject) : int =
        match parsed.TryFind key with
        | Some s ->
            match Int32.TryParse(s, Globalization.CultureInfo.InvariantCulture) with
            | true, res -> res
            | _ -> failwithf "Failed to parse expected int '%s' for key '%s'" s key
        | None -> otherwise

module QuaverChart =

    let private timing_point_from_yaml (parsed: Yaml.ParsedYamlNestedObject) : QuaverTimingPoint =
        {
            StartTime = Yaml.get_nested_float_or "StartTime" 0.0f parsed
            Bpm = Yaml.get_nested_float_or "Bpm" 0.0f parsed
        }

    let private sv_from_yaml (parsed: Yaml.ParsedYamlNestedObject) : QuaverSliderVelocity =
        {
            StartTime = Yaml.get_nested_float_or "StartTime" 0.0f parsed
            Multiplier = Yaml.get_nested_float_or "Multiplier" 0.0f parsed
        }

    let private object_from_yaml (parsed: Yaml.ParsedYamlNestedObject) : QuaverHitObject =
        {
            StartTime = Yaml.get_nested_int_or "StartTime" 0 parsed
            Lane = Yaml.get_nested_int "Lane" parsed
            EndTime = Yaml.get_nested_int_or "EndTime" 0 parsed
        }

    let private from_yaml (parsed: Yaml.ParsedYamlObject) : QuaverChart =
        {
            AudioFile = Yaml.get_string "AudioFile" parsed
            SongPreviewTime = Yaml.get_int_or "SongPreviewTime" 0 parsed
            BackgroundFile = Yaml.get_string_or "BackgroundFile" "" parsed
            MapId = Yaml.get_int_or "MapId" 0 parsed
            MapSetId = Yaml.get_int_or "MapSetId" 0 parsed
            Mode = if Yaml.get_string "Mode" parsed = "Keys7" then 7 else 4
            Title = Yaml.get_string_or "Title" "" parsed
            Artist = Yaml.get_string_or "Artist" "" parsed
            Source = Yaml.get_string_or "Source" "" parsed
            Tags = (Yaml.get_string_or "Tags" "" parsed).Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> List.ofArray
            Creator = Yaml.get_string_or "Creator" "" parsed
            DifficultyName = Yaml.get_string_or "DifficultyName" "" parsed
            Description = Yaml.get_string_or "Description" "" parsed
            Genre = Yaml.get_string_or "Genre" "" parsed
            BPMDoesNotAffectScrollVelocity = (Yaml.get_string_or "BPMDoesNotAffectScrollVelocity" "true" parsed).ToLower().StartsWith('t')
            InitialScrollVelocity = Single.Parse(Yaml.get_string_or "InitialScrollVelocity" "1" parsed, Globalization.CultureInfo.InvariantCulture)
            HasScratchKey = (Yaml.get_string_or "HasScratchKey" "true" parsed).ToLower().StartsWith('t')
            TimingPoints = Yaml.get_array<QuaverTimingPoint> "TimingPoints" timing_point_from_yaml parsed
            SliderVelocities = Yaml.get_array<QuaverSliderVelocity> "SliderVelocities" sv_from_yaml parsed
            HitObjects = Yaml.get_array<QuaverHitObject> "HitObjects" object_from_yaml parsed
        }
    
    let from_file (path: string) : Result<QuaverChart, string> =
        try
            let lines = File.ReadAllLines path
            let parsed_yaml = Yaml.from_lines (List.ofArray lines)
            let chart = from_yaml parsed_yaml
            Ok chart
        with err ->
            Logging.Warn(sprintf "Error loading quaver chart: %O" err)
            Error err.Message