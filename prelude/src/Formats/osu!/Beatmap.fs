namespace Prelude.Formats.Osu

open System
open System.IO
open System.Text

[<Struct>]
type private BeatmapParserState =
    | Nothing
    | Header
    | Events
    | TimingPoints
    | Objects
    | Colors

//https://osu.ppy.sh/wiki/en/Client/File_formats/osu_(file_format)
//https://osu.ppy.sh/community/forums/topics/1869?start=12468#p12468

type Beatmap =
    {
        General: General
        Editor: Editor
        Metadata: Metadata
        Difficulty: Difficulty
        Events: StoryboardEvent list
        Objects: HitObject list
        Timing: TimingPoint list
    }

    member this.Filename() : string =
        let clean (s: string) =
            s
            |> String.filter(fun c -> c = ' ' || Char.IsAsciiLetterOrDigit(c))
            |> fun s -> s.Substring(0, min s.Length 200)

        sprintf
            "%s - %s (%s) [%s].osu"
            (clean this.Metadata.ArtistUnicode)
            (clean this.Metadata.TitleUnicode)
            (clean this.Metadata.Creator)
            (clean this.Metadata.Version)

    member this.ToLines() : string seq =
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

    static member FromStream(stream: Stream) : Beatmap =
        use reader = new StreamReader(stream)

        let mutable state = Nothing
        let general = ref Map.empty
        let editor = ref Map.empty
        let metadata = ref Map.empty
        let difficulty = ref Map.empty
        let mutable section_ref = general

        let objects = ResizeArray<HitObject>()
        let timing = ResizeArray<TimingPoint>()
        let events = ResizeArray<StoryboardEvent>()

        while reader.Peek() >= 0 do
            let line = reader.ReadLine().TrimEnd()

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
            | "[Events]" -> state <- Events
            | "[TimingPoints]" -> state <- TimingPoints
            | "[HitObjects]" -> state <- Objects
            | "[Colours]" -> state <- Colors
            | _ ->

            match state with
            | Nothing -> ()
            | Header ->
                let parts = line.Split(':', 2, StringSplitOptions.TrimEntries)

                if parts.Length = 2 then
                    section_ref.Value <- Map.add parts.[0] parts.[1] section_ref.Value
            | Events -> Option.iter events.Add (StoryboardEvent.TryParse(line))
            | TimingPoints -> timing.Add(TimingPoint.FromString(line))
            | Objects -> objects.Add(HitObject.FromString(line))
            | Colors -> () // todo: support colors header

        {
            General = General.FromMap(general.Value)
            Editor = Editor.FromMap(editor.Value)
            Metadata = Metadata.FromMap(metadata.Value)
            Difficulty = Difficulty.FromMap(difficulty.Value)
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

    static member TryReadFromFile(path: string) : Result<Beatmap, string> =
        try
            use stream = File.OpenRead(path)
            Ok(Beatmap.FromStream(stream))
        with err ->
            Error(err.Message)

    member this.WriteToFile(path: string) : unit =
        this.ToLines() |> fun contents -> File.WriteAllLines(path, contents, Encoding.UTF8)

    member this.WriteToStream(stream: Stream, leave_stream_open: bool) : unit =
        use writer = new StreamWriter(stream, Encoding.UTF8, leaveOpen = leave_stream_open)
        this.ToLines() |> Seq.iter writer.WriteLine

    /// The internal hash osu! uses for a .osu file
    static member Hash(stream: Stream) : string =
        let md5 = Security.Cryptography.MD5.Create()
        md5.ComputeHash(stream) |> Convert.ToHexString |> _.ToLower()

    member this.GenerateExportHash() : string =
        use ms = new MemoryStream()
        this.WriteToStream(ms, true)
        ms.Position <- 0
        Beatmap.Hash(ms)

    static member HashFromFile(path: string) : Result<string, string> =
        try
            use fs = File.OpenRead(path)
            Ok(Beatmap.Hash(fs))
        with err ->
            Error err.Message
