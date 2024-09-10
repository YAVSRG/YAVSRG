namespace Prelude.Charts.Formats.osu

open System
open System.Globalization
open System.IO
open System.Text

//https://osu.ppy.sh/wiki/en/Client/File_formats/osu_(file_format)
//https://osu.ppy.sh/community/forums/topics/1869?start=12468#p12468

(*
  Not currently supported:
    Parsing storyboard objects (only writing a newly created storyboard is available)
    Loops and triggers in storyboards (both reading and writing)
*)

type Beatmap =
    {
        General: General
        Editor: Editor
        Metadata: Metadata
        Difficulty: Difficulty
        Events: StoryboardObject list
        Objects: HitObject list
        Timing: TimingPoint list
    }
    member this.Filename =
        let clean =
            String.filter (fun c -> System.Char.IsWhiteSpace c || System.Char.IsLetterOrDigit c)

        sprintf
            "%s - %s (%s) [%s].osu"
            (clean this.Metadata.ArtistUnicode)
            (clean this.Metadata.TitleUnicode)
            (clean this.Metadata.Creator)
            (clean this.Metadata.Version)
    member this.ToLines =
        seq {
            yield "osu file format v14"
            yield ""
            yield "[General]"
            for key, value in this.General.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Editor]"
            for key, value in this.Editor.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Metadata]"
            for key, value in this.Metadata.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Difficulty]"
            for key, value in this.Difficulty.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Events]"
            for object in this.Events do
                yield object.ToString()
            yield ""
            yield "[TimingPoints]"
            for object in this.Timing do
                yield object.ToString()
            yield ""
            // todo: [Colours]
            yield "[HitObjects]"
            for object in this.Objects do
                yield object.ToString()
            yield ""
        }


type Storyboard =
    {
        // todo: Variables
        Events: StoryboardObject list
    }
    member this.ToLines =
        seq {
            yield "[Events]"
            for object in this.Events do
                yield object.ToString()
        }

module private Parser =

    let parse_failure message line =
        failwithf "osu! parse error: %s\nat: %s" message line

    let parse_storyboard_event (csv: string array) =
        match csv.[0].ToLowerInvariant() with
        | "0"
        | "background" ->   
            Background(
                (CsvHelpers.string_or 2 "" csv).Trim('"'),
                CsvHelpers.int_or 3 0 csv,
                CsvHelpers.int_or 4 0 csv
            )
            |> Some
        | "1"
        | "video" ->
            Video(
                CsvHelpers.int_or 1 0 csv,
                (CsvHelpers.string_or 2 "" csv).Trim('"'),
                CsvHelpers.int_or 3 0 csv,
                CsvHelpers.int_or 4 0 csv
            )
            |> Some
        | "2"
        | "break" ->
            Break(
                CsvHelpers.int_or 1 0 csv,
                CsvHelpers.int_or 2 0 csv
            )
            |> Some
        | "sample" ->
            Sample(
                CsvHelpers.int_or 1 0 csv,
                CsvHelpers.enum_or 2 Layer.Background csv,
                (CsvHelpers.string_or 3 "" csv).Trim('"'),
                CsvHelpers.int_or 4 0 csv
            )
            |> Some
        | "sprite"
        | "animation"
        | _ -> None

    let parse_timing_point (csv: string array) =
        let uninherited = CsvHelpers.int_or 6 1 csv <> 0
        if uninherited then
            Uninherited {
                Time = CsvHelpers.float_or 0 0.0 csv
                MsPerBeat = CsvHelpers.float_or 1 500.0 csv
                Meter = CsvHelpers.int_or 2 4 csv
                SampleSet = CsvHelpers.enum_or 3 SampleSet.Default csv
                SampleIndex = CsvHelpers.int_or 4 0 csv
                Volume = CsvHelpers.int_or 5 0 csv
                Effects = CsvHelpers.enum_or 7 TimingEffect.None csv
            }
        else
            Inherited {
                Time = CsvHelpers.float_or 0 0.0 csv
                Multiplier = -100.0 / (CsvHelpers.float_or 1 1.0 csv)
                SampleSet = CsvHelpers.enum_or 3 SampleSet.Default csv
                SampleIndex = CsvHelpers.int_or 4 0 csv
                Volume = CsvHelpers.int_or 5 0 csv
                Effects = CsvHelpers.enum_or 7 TimingEffect.None csv
            }

    let parse_hit_sample (colon_separated_values: string array) =
        {
            NormalSet = CsvHelpers.enum_or 0 SampleSet.Default colon_separated_values
            AdditionSet = CsvHelpers.enum_or 1 SampleSet.Default colon_separated_values
            Index = CsvHelpers.int_or 2 0 colon_separated_values
            Volume = CsvHelpers.int_or 3 0 colon_separated_values
            Filename = (CsvHelpers.string_or 4 "" colon_separated_values).Trim('"')
        }

    let parse_hit_object (line: string) (csv: string array) =
        let x = CsvHelpers.int_or 0 0 csv
        let y = CsvHelpers.int_or 1 0 csv
        let time = CsvHelpers.int_or 2 0 csv
        let obj_type = CsvHelpers.int_or 3 0 csv
        let hitsound = CsvHelpers.enum_or 4 HitSound.Default csv

        let starts_new_combo = obj_type &&& 4 <> 0
        let color_hax = (obj_type >>> 4) &&& 7

        if obj_type &&& 1 > 0 then
            HitCircle {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                HitSample = 
                    CsvHelpers.string_or 5 "" csv
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        elif obj_type &&& 2 > 0 then
            let curve = CsvHelpers.string_or 5 "" csv |> fun s -> s.Split([|'|'|], 2, StringSplitOptions.TrimEntries)
            let curve_shape, curve_points =
                if curve.Length < 2 then
                    parse_failure "Invalid slider curve" line
                else
                    match curve.[0].ToUpperInvariant() with
                    | "B" -> Bezier
                    | "C" -> Catmull
                    | "L" -> Linear
                    | "P" -> PerfectCircle
                    | _ -> Bezier
                    ,
                    curve.[1].Split('|', StringSplitOptions.TrimEntries)
                    |> Seq.map (fun s -> 
                        let xy = s.Split(':', StringSplitOptions.TrimEntries)
                        CsvHelpers.int_or 0 0 xy, CsvHelpers.int_or 1 0 xy
                    )
                    |> List.ofSeq
            Slider {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                CurveType = curve_shape
                CurvePoints = curve_points
                Slides = CsvHelpers.int_or 6 1 csv
                Length = CsvHelpers.float_or 7 100.0 csv
                EdgeSounds = 
                    CsvHelpers.string_or 8 "" csv
                    |> fun s -> s.Split("|", StringSplitOptions.TrimEntries)
                    |> Seq.choose (fun n ->
                        match Int32.TryParse(n, CultureInfo.InvariantCulture) with
                        | true, v -> Some (enum v)
                        | false, _ -> None
                    )
                    |> List.ofSeq
                EdgeSets = 
                    CsvHelpers.string_or 9 "" csv
                    |> fun s -> s.Split("|", StringSplitOptions.TrimEntries)
                    |> Seq.map (fun s -> 
                        let sets = s.Split(':', StringSplitOptions.TrimEntries)
                        CsvHelpers.enum_or 0 SampleSet.None sets, CsvHelpers.enum_or 1 SampleSet.None sets
                    )
                    |> List.ofSeq
                HitSample = 
                    CsvHelpers.string_or 10 "" csv
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        elif obj_type &&& 8 > 0 then
            Spinner {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                EndTime = CsvHelpers.int_or 5 time csv
                HitSample = 
                    CsvHelpers.string_or 6 "" csv
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        elif obj_type &&& 128 > 0 then
            let endtime_and_sample = CsvHelpers.string_or 5 "" csv |> fun s -> s.Split([|':'|], 2, StringSplitOptions.TrimEntries)
            Hold {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                EndTime = CsvHelpers.int_or 0 time endtime_and_sample
                HitSample = 
                    CsvHelpers.string_or 1 "" endtime_and_sample
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        else
            parse_failure "Unrecognised object type" line

    [<Struct>]
    type private ParserState =
        | Nothing
        | Header
        | Events
        | TimingPoints
        | Objects
        | Colors

    let beatmap_from_stream (stream: Stream) : Beatmap =
        use reader = new StreamReader(stream)

        let mutable state = Nothing
        let general = ref Map.empty
        let editor = ref Map.empty
        let metadata = ref Map.empty
        let difficulty = ref Map.empty
        let mutable section_ref = general

        let objects = ResizeArray<HitObject>()
        let timing = ResizeArray<TimingPoint>()
        let events = ResizeArray<StoryboardObject>()

        while reader.Peek() >= 0 do
            let line = reader.ReadLine()
            let trimmed = line.Trim()
            match trimmed with
            | "" -> ()
            | _ when line.StartsWith("//") -> ()
            | "[General]" ->
                state <- Header
                section_ref <- general
            | "[Editor]" ->
                state <- Header
                section_ref <- editor
            | "[Metadata]" ->
                state <- Header
                section_ref <- metadata
            | "[Difficulty]" ->
                state <- Header
                section_ref <- difficulty
            | "[Events]" ->
                state <- Events
            | "[TimingPoints]" ->
                state <- TimingPoints
            | "[HitObjects]" ->
                state <- Objects
            | "[Colours]" -> // todo: support colors header
                state <- Colors
            | _ ->

            match state with
            | Nothing -> ()
            | Header ->
                let parts = trimmed.Split([|':'|], 2, StringSplitOptions.TrimEntries)
                if parts.Length = 2 then
                    section_ref.Value <- Map.add parts.[0] parts.[1] section_ref.Value
            | Events ->
                let csv = trimmed.Split(',', StringSplitOptions.TrimEntries)
                if csv.Length > 1 then
                    match parse_storyboard_event csv with
                    | Some event -> events.Add event
                    | None -> ()
            | TimingPoints ->
                let csv = trimmed.Split(',', StringSplitOptions.TrimEntries)
                if csv.Length > 1 && csv.Length < 8 then
                    parse_failure "Failed to parse timing point" line
                elif csv.Length >= 8 then
                    parse_timing_point csv |> timing.Add
            | Objects ->
                let csv = trimmed.Split(',', StringSplitOptions.TrimEntries)
                if csv.Length > 1 && csv.Length < 5 then
                    parse_failure "Failed to parse hit object" line
                elif csv.Length >= 5 then
                    parse_hit_object line csv |> objects.Add
            | Colors -> ()

        {
            General = General.FromMap general.Value
            Editor = Editor.FromMap editor.Value
            Metadata = Metadata.FromMap metadata.Value
            Difficulty = Difficulty.FromMap difficulty.Value
            Events = List.ofSeq events
            Objects = objects |> Seq.sortBy (_.Time) |> List.ofSeq
            Timing = timing |> Seq.sortBy (_.Time) |> List.ofSeq 
        }

type Beatmap with
    static member FromFile(path: string) =
        try
            use stream = File.OpenRead(path)
            Ok (Parser.beatmap_from_stream stream)
        with err ->
            printfn "%s" err.StackTrace
            Error err.Message
    member this.ToFile(path: string) =
        this.ToLines |> String.concat "\n" |> fun contents -> File.WriteAllText(path, contents, Encoding.UTF8)
    member this.ToStream(stream: Stream) =
        use writer = new StreamWriter(stream, Encoding.UTF8)
        this.ToLines |> Seq.iter writer.WriteLine

type Storyboard with
    // todo: there is currently no support for reading a storyboard file, only generating one
    member this.ToFile(path: string) =
        this.ToLines |> String.concat "\n" |> fun contents -> File.WriteAllText(path, contents)