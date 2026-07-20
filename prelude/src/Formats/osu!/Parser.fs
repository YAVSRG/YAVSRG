namespace Prelude.Formats.Osu

open System
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

    member this.Filename : string =
        let clean (s: string) =
            s
            |> String.filter(fun c -> c = ' ' || Char.IsAsciiLetterOrDigit c)
            |> fun s -> s.Substring(0, min s.Length 200)

        sprintf
            "%s - %s (%s) [%s].osu"
            (clean this.Metadata.ArtistUnicode)
            (clean this.Metadata.TitleUnicode)
            (clean this.Metadata.Creator)
            (clean this.Metadata.Version)

    member this.ToLines : string seq =
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

module OsuParser =

    let private parse_failure (message: string) (line: string) =
        failwithf "osu! parse error: %s\nat: %s" message line

    let parse_storyboard_event (line: string) : StoryboardObject option =
        
        let inline unsupported() = None
        
        let inline parse_background(values: SplitValues) =
            Background(
                values.StringOrDefault(2, ""),
                values.IntOrDefault(3, 0),
                values.IntOrDefault(4, 0)
            )
            
        let inline parse_video(values: SplitValues) =
            Video(
                values.IntOrDefault(1, 0),
                values.StringOrDefault(2, ""),
                values.IntOrDefault(3, 0),
                values.IntOrDefault(4, 0)
            )
            
        let inline parse_break(values: SplitValues) =
            Break(
                values.IntOrDefault(1, 0),
                values.IntOrDefault(2, 0)
            )
            
        let inline parse_sample(values: SplitValues) =
            Sample(
                values.IntOrDefault(1, 0),
                values.EnumOrDefault(2, Layer.Background, false),
                values.StringOrDefault(3, ""),
                values.IntOrDefault(4, 0)
            )
        
        let inline parse(values: SplitValues) =
            match values.[0].ToLowerInvariant() with
            | "0"
            | "background" -> Some(parse_background(values))
            | "1"
            | "video" -> Some(parse_video(values))
            | "2"
            | "break" -> Some(parse_break(values))
            | "sample" -> Some(parse_sample(values))
            | "sprite"
            | "animation"
            | _ -> unsupported()
        
        let values = SplitValues.Parse(line, ',')
        if values.Length = 0 then
            parse_failure "Empty line" line
        else parse(values)

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
            let line = reader.ReadLine().Trim()
            match line with
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
            | "[Colours]" ->
                state <- Colors
            | _ ->

            match state with
            | Nothing -> ()
            | Header ->
                let parts = line.Split([|':'|], 2, StringSplitOptions.TrimEntries)
                if parts.Length = 2 then
                    section_ref.Value <- Map.add parts.[0] parts.[1] section_ref.Value
            | Events ->
                line
                |> parse_storyboard_event
                |> Option.iter events.Add
            | TimingPoints -> timing.Add(TimingPoint.FromString(line))
            | Objects -> objects.Add(HitObject.FromString(line))
            | Colors ->
                () // todo: support colors header

        {
            General = General.FromMap general.Value
            Editor = Editor.FromMap editor.Value
            Metadata = Metadata.FromMap metadata.Value
            Difficulty = Difficulty.FromMap difficulty.Value
            Events = List.ofSeq events
            // The osu! client sorts all hitobjects and timing points by timestamp for when users have put them in the wrong order via notepad
            // IMPORTANT: If multiple timing points are stacked on the same timestamp,
            //   osu! will act UNPREDICTABLY and put them in an arbitrary order since it does not use a stable sort
            // I have ruled to use a stable sort (`Seq.sortBy`)
            //   This means stacked timing points will stay in the order they are in the .osu file after being sorted
            // This gives a defined behaviour to my parse and conversion but anyone writing their own should be aware that it's UB
            Objects = objects |> Seq.sortBy _.Time |> List.ofSeq
            Timing = timing |> Seq.sortBy _.Time |> List.ofSeq
        }

type Beatmap with
    static member FromFile(path: string) =
        try
            use stream = File.OpenRead(path)
            Ok (OsuParser.beatmap_from_stream stream)
        with err ->
            Error err.Message
    member this.ToFile(path: string) =
        this.ToLines |> String.concat "\n" |> fun contents -> File.WriteAllText(path, contents, Encoding.UTF8)
    member this.ToStream(stream: Stream, leave_stream_open: bool) =
        use writer = new StreamWriter(stream, Encoding.UTF8, leaveOpen = leave_stream_open)
        this.ToLines |> Seq.iter writer.WriteLine

    /// The internal hash osu! uses for a .osu file
    static member Hash(stream: Stream) =
        let md5 = Security.Cryptography.MD5.Create()
        md5.ComputeHash(stream) |> Convert.ToHexString |> _.ToLower()

    static member Hash(beatmap: Beatmap) =
        use ms = new MemoryStream()
        beatmap.ToStream(ms, true)
        ms.Position <- 0
        Beatmap.Hash ms

    static member HashFromFile(path: string) : Result<string, string> =
        try
            use fs = File.OpenRead(path)
            Ok(Beatmap.Hash fs)
        with err ->
            Error err.Message

type Storyboard with
    // todo: there is currently no support for reading a storyboard file, only generating one
    member this.ToFile(path: string) =
        this.ToLines |> String.concat "\n" |> fun contents -> File.WriteAllText(path, contents)