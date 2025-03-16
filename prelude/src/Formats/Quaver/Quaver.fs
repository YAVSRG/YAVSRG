namespace Prelude.Formats.Quaver

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
            HasScratchKey = (Yaml.get_string_or "HasScratchKey" "false" parsed).ToLower().StartsWith('t')
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
            Logging.Warn "Error loading Quaver chart: %O" err
            Error err.Message

type QuaverChart with
    static member FromFile(path: string) : Result<QuaverChart, string> = QuaverChart.from_file path

    /// The internal hash osu! uses for a .osu file
    static member Hash(stream: Stream) =
        let md5 = Security.Cryptography.MD5.Create()
        md5.ComputeHash(stream) |> Convert.ToHexString |> _.ToLower()

    static member HashFromFile(path: string) : Result<string, string> =
        try
            use fs = File.OpenRead(path)
            Ok(QuaverChart.Hash fs)
        with err ->
            Error err.Message