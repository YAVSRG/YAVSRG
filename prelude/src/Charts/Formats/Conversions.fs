namespace Prelude.Charts.Formats

open System
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic
open System.Linq
open Percyqaz.Common
open Prelude.Common
open Prelude.Charts.Formats.``osu!``
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Formats.StepMania

module Conversions =

    type ConversionActionConfig =
        {
            CopyMediaFiles: bool
            StepmaniaPackId: int option
            ChangedAfter: DateTime option
            PackName: string
        }
        static member Default =
            {
                CopyMediaFiles = true
                StepmaniaPackId = None
                ChangedAfter = None
                PackName = "Singles"
            }

    type ConversionAction =
        {
            Config: ConversionActionConfig
            Source: string
            TargetDirectory: string
        }

    module ``osu! to Interlude`` =

        let private listToDotNet (list: 't list) = 
            let result = ResizeArray<'t> list
            result.Reverse()
            result

        let private convertHitObjects (objects: HitObject list) (keys: int) : TimeData<NoteRow> =
            let holds = Array.create keys -1.0f<ms>

            let xToColumn (x: float) = x / 512.0 * float keys |> int

            let createRow () =
                let nr = NoteRow.createEmpty keys
                for i = 0 to (holds.Length - 1) do
                    if holds.[i] >= 0.0f<ms> then nr.[i] <- NoteType.HOLDBODY
                nr

            let updateHolds time snaps =
                let mutable s = snaps
                let mutable minT = Array.fold (fun y x -> if x = -1.0f<ms> then y else min y x) Time.infinity holds
                while minT < time do
                    for k in 0..(keys - 1) do
                        if (holds.[k] >= 0.0f<ms> && holds.[k] = minT) then 
                            match s with
                            | (time: Time, nr: NoteRow) :: ss ->
                                holds.[k] <- -1.0f<ms>
                                if time = minT then
                                    nr.[k] <- NoteType.HOLDTAIL
                                else s <- (minT, let nr = createRow() in nr.[k] <- NoteType.HOLDTAIL; nr) :: s
                            | [] -> failwith "impossible"
                    minT <- Array.fold (fun y x -> if x = -1.0f<ms> then y else min y x) Time.infinity holds
                s

            let note k time snaps =
                if holds.[k] <> -1.0f<ms> then
                    Logging.Debug(sprintf "Skipping stacked note (inside LN) at %f" time)
                    snaps
                else

                match snaps with
                | (t: Time, nr: NoteRow) :: ss ->
                    if t = time then 
                        if nr.[k] <> NoteType.NOTHING then 
                            Logging.Debug(sprintf "Skipping stacked note at %f" time)
                        else nr.[k] <- NoteType.NORMAL
                        snaps
                    else (time, let row = createRow() in row.[k] <- NoteType.NORMAL; row) :: snaps
                | [] -> (time, let row = createRow() in row.[k] <- NoteType.NORMAL; row) :: snaps

            let hold (k: int) (time: Time) (release: Time) snaps =
                if holds.[k] <> -1.0f<ms> then 
                    Logging.Debug(sprintf "Skipping stacked LN (inside LN) at %f" time)
                    snaps
                elif release < time then 
                    Logging.Debug(sprintf "Skipping LN that ends at %f but starts at %f" release time)
                    snaps
                elif release = time then
                    Logging.Debug(sprintf "Treating LN that ends & starts at %f as a note" time)
                    note k time snaps
                else

                holds.[k] <- release
                match snaps with
                | (t: Time, nr: NoteRow) :: ss ->
                    if t = time then nr.[k] <- NoteType.HOLDHEAD; snaps
                    else (time, let row = createRow() in row.[k] <- NoteType.HOLDHEAD; row) :: snaps
                | [] -> (time, let row = createRow() in row.[k] <- NoteType.HOLDHEAD; row) :: snaps

            let f snaps (hitObj: HitObject) =
                match hitObj with
                | HitCircle ((x, y), time, _, _) -> note (xToColumn x) time (updateHolds time snaps)
                | HoldNote ((x, y), time, endTime, _, _) -> hold (xToColumn x) time endTime (updateHolds time snaps)
                | _ -> snaps

            let states = updateHolds Time.infinity (List.fold f [] objects)
            TimeData<NoteRow>(listToDotNet states)

        let private convertTimingPoints (points: TimingPoint list) (keys: int) (endTime: Time) : TimeData<BPM> * MultiTimeData<float32> =
            let rec bpmDurations points = 
                if List.isEmpty points then failwith "no bpm point"
                match List.head points with
                | TimingPoint.BPM (offset, msPerBeat, _, _, _) ->
                    let mutable current: float32<ms/beat> = msPerBeat
                    let mutable t: Time = offset
                    let data = new Dictionary<float32<ms/beat>, Time>()

                    for p in points do
                        if (not (data.ContainsKey current)) then data.Add(current, 0.0f<ms>)
                        match p with
                        | (TimingPoint.SV _) -> ()
                        | (TimingPoint.BPM (offset, msPerBeat, _, _, _)) -> data.[current] <- data.[current] + offset - t; t <- offset; current <- msPerBeat
                    if (not (data.ContainsKey current)) then data.Add(current, 0.0f<ms>)
                    data.[current] <- data.[current] + endTime - t
                    data
                | _ -> bpmDurations (List.tail points)

            let mostCommonBpm = (bpmDurations points).OrderByDescending(fun p -> p.Value).First().Key

            let addOrSkipSV (offset, value) sv =
                match sv with
                | [] -> [(offset, value)]
                | (time, oldValue) :: s -> if oldValue = value then sv else (offset, value) :: sv

            let (bpm, sv, _) = 
                let func ((bpm, sv, scroll): (TimeDataItem<BPM> list * TimeDataItem<float32> list * float32)) (point: TimingPoint) : (TimeDataItem<BPM> list * TimeDataItem<float32> list * float32) =
                    match point with
                    | (TimingPoint.BPM (offset, msPerBeat, meter, _, _)) -> 
                        (((offset, (meter, msPerBeat)) :: bpm), addOrSkipSV (offset, (mostCommonBpm / msPerBeat)) sv, mostCommonBpm / msPerBeat)
                    | (TimingPoint.SV (offset, value, _, _)) ->
                        (bpm, addOrSkipSV (offset, (value * scroll)) sv, scroll)
                List.fold func ([], [], 1.0f) points

            let svData = new MultiTimeData<float32>(keys)
            svData.SetChannelData(-1, listToDotNet sv)
            (TimeData(listToDotNet bpm), svData)

        let private rateRegex = Regex("""((^|\s)([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)($|\s))|(x([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?))|(([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)x)""");
        let looks_like_a_rate (b: Beatmap) : bool = rateRegex.IsMatch b.Metadata.Version

        let convert (b: Beatmap) (action: ConversionAction) : Chart =
            let keys = b.Difficulty.CircleSize |> int
            let rec findBackgroundFile e =
                match e with
                | (Background (path, _)) :: _ -> path
                | _ :: es -> findBackgroundFile es
                | [] -> ""
            let header =
                {
                    Title = b.Metadata.Title.Trim()
                    TitleNative = let t = b.Metadata.TitleUnicode.Trim() in if t.Length > 0 && t <> b.Metadata.Title.Trim() then Some t else None
                    Artist = b.Metadata.Artist.Trim()
                    ArtistNative = let t = b.Metadata.ArtistUnicode.Trim() in if t.Length > 0 && t <> b.Metadata.Artist.Trim() then Some t else None
                    Creator = b.Metadata.Creator
                    DiffName = b.Metadata.Version
                    Subtitle = None
                    Source = let t = b.Metadata.Source.Trim() in if t.Length > 0 then Some t else None
                    Tags = b.Metadata.Tags

                    PreviewTime = b.General.PreviewTime
                    BackgroundFile = findBackgroundFile b.Events |> Relative
                    AudioFile = b.General.AudioFilename |> Relative

                    SourcePack = "osu!"
                    ChartSource = Osu (b.Metadata.BeatmapSetID, b.Metadata.BeatmapID)
                }
            let snaps = convertHitObjects b.Objects keys
            let (bpm, sv) = (convertTimingPoints b.Timing keys (offsetOf (snaps.GetPointAt Time.infinity)))
            Chart(keys, header, snaps, bpm, sv, String.Join("_", (b.Metadata.Title + " [" + b.Metadata.Version + "].yav").Split(Path.GetInvalidFileNameChars())))

    (*
        Conversion code for StepMania -> Interlude
        Todo: Support freezes, warps, stops
    *)

    module ``StepMania to Interlude`` =

        let private convert_measures (keys: int) (measures: string list list) (bpms: (float32<beat> * float32<beat/minute>) list) start = 
            let mutable bpms = bpms
            let meter = 4<beat>
            let fmeter = float32 meter * 1.0f<beat>
            let states = new List<Time * NoteRow>()
            let points = new List<Time * BPM>()
            let mutable ln: Bitmap = 0us
            let mutable now = start
            let (_, b) = List.head bpms in points.Add(start, (meter, 60000.0f<ms/minute> / b))
            let mutable msPerBeat = 60000.0f<ms/minute> / b
            bpms <- List.tail bpms
            let mutable totalBeats = 0.0f<beat>;
            let mutable lo = 0.0f<beat>
            let mutable hi = 0.0f<beat>

            let convert_measure (m: string list) (lo: float32<beat>) (hi: float32<beat>) =
                let l = List.length m |> float32
                let sep = msPerBeat * fmeter / l
                let start = Math.Ceiling(lo * l / fmeter |> float) |> int
                let finish = Math.Ceiling(hi * l / fmeter |> float) |> int
                let offset = now + (float32 start * sep) - (lo * msPerBeat)

                for i in start .. (finish - 1) do
                    let nr = NoteRow.createLnBodies keys ln
                    Seq.iteri (fun k c ->
                        match c with
                        | '0' -> ()
                        | '1' -> nr.[k] <- NoteType.NORMAL
                        | '2' | '4' ->
                            nr.[k] <- NoteType.HOLDHEAD
                            ln <- Bitmap.setBit k ln
                        | '3' ->
                            nr.[k] <- NoteType.HOLDTAIL
                            ln <- Bitmap.unsetBit k ln
                        | 'M' -> () //ignore mines
                        | _ -> failwith ("unknown note type " + c.ToString())
                        ) m.[i]
                    if NoteRow.isEmpty nr |> not then states.Add((offset + float32 (i - start) * sep), nr)

            List.iteri (fun i m -> 
                totalBeats <- totalBeats + fmeter
                lo <- 0.0f<beat>
                while ((not (List.isEmpty bpms)) && fst (List.head bpms) < totalBeats) do
                    hi <- fst (List.head bpms) - totalBeats + fmeter
                    convert_measure m lo hi
                    now <- now + msPerBeat * (hi - lo)
                    lo <- hi
                    let (_, b) = List.head bpms in points.Add(now, (meter, 60000.0f<ms/minute> / b))
                    msPerBeat <- 60000.0f<ms/minute> / b
                    bpms <- List.tail bpms
                convert_measure m lo fmeter
                now <- now + msPerBeat * (fmeter - lo)
                ) measures
            (new TimeData<NoteRow>(states), new TimeData<BPM>(points))

        let convert (sm: StepmaniaData) (action: ConversionAction) = 

            let path = Path.GetDirectoryName action.Source

            let rec metadataFallback x =
                match x with
                | "" :: xs -> metadataFallback xs
                | s :: _ -> s.Trim()
                | [] -> ""

            let rec metadataFallbackOpt x =
                match x with
                | "" :: xs -> metadataFallbackOpt xs
                | s :: _ -> Some (s.Trim())
                | [] -> None

            let findBackground () : string =
                let guess = sm.TITLE + "-bg.jpg"
                if not (File.Exists (Path.Combine (path, guess))) then
                    Directory.GetFiles path
                    |> Array.tryPick
                        (fun s ->
                            let filename = Path.GetFileNameWithoutExtension(s).ToLower()
                            if (filename.Contains "bg" || filename.Contains "background") then Some <| Path.GetFileName s else None)
                    |> function Some s -> s | None -> ""
                else guess

            let findAuthor () : string =
                let folderName = Path.GetFileName path
                let paren = folderName.LastIndexOf('(')
                if paren > 1 then
                    let guess = folderName.Substring (paren + 1)
                    if guess.EndsWith(')') then guess.TrimEnd ')' else ""
                else
                    let paren = folderName.LastIndexOf('[')
                    let guess = folderName.Substring (paren + 1)
                    if guess.EndsWith(']') then guess.TrimEnd ']' else ""
        
            let convert_difficulty (i: int) (diff: ChartData) : Chart = 
                let keys = keyCount diff.STEPSTYPE
                let title = metadataFallback [sm.TITLETRANSLIT; sm.TITLE]
                let artist = metadataFallback [sm.ARTISTTRANSLIT; sm.ARTIST]
                let header = 
                    {
                        Title = title
                        TitleNative = match metadataFallbackOpt [sm.TITLETRANSLIT] with Some t when t = title -> None | x -> x
                        Artist = artist
                        ArtistNative = match metadataFallbackOpt [sm.ARTISTTRANSLIT] with Some t when t = artist -> None | x -> x
                        Creator = metadataFallback [findAuthor(); sm.CREDIT; diff.CREDIT]
                        DiffName = metadataFallback [sm.SUBTITLETRANSLIT; sm.SUBTITLE;
                            diff.CHARTNAME; diff.DESCRIPTION; diff.CHARTSTYLE; diff.STEPSTYPE.ToString() + " " + diff.METER.ToString()]
                        Subtitle = metadataFallbackOpt [sm.SUBTITLETRANSLIT; sm.SUBTITLE]
                        Tags = sm.GENRE.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
                        Source = None

                        PreviewTime = sm.SAMPLESTART * 1000.0f<ms>
                        AudioFile = metadataFallback [sm.MUSIC; "audio.mp3"] |> Relative
                        BackgroundFile = metadataFallback [(if File.Exists (Path.Combine (path, sm.BACKGROUND)) then sm.BACKGROUND else ""); findBackground ()] |> Relative

                        SourcePack = "Singles"
                        ChartSource = Unknown
                    }
                let filepath = Path.Combine (path, diff.STEPSTYPE.ToString() + " " + diff.METER.ToString() + " [" + (string i) + "].yav")
                let (notes, bpm) = convert_measures keys diff.NOTES sm.BPMS (-sm.OFFSET * 1000.0f<ms>)
                Chart(keys, header, notes, bpm, new MultiTimeData<float32>(keys), filepath)
            sm.Charts |> List.mapi convert_difficulty

    module ``Interlude to osu!`` =

        let private convertSnapsToHitobjects snaps keys =
            let columnToX k = (float k + 0.5) * 512.0 / float keys |> round
            let rec ln_lookahead k snaps =
                match snaps with
                | (offset: Time, nr: NoteRow) :: ss -> if nr.[k] = NoteType.HOLDTAIL then offset else ln_lookahead k ss
                | [] -> failwith "hold note has no end"

            let rec convert snaps = seq {
                match snaps with
                | (offset, nr) :: ss ->
                    for k in nr |> NoteRow.noteData NoteType.NORMAL |> Bitmap.toSeq do
                        yield HitCircle ((columnToX k, 240.0), offset, enum 0, (enum 0, enum 0, 0, 0, ""))
                    for k in nr |> NoteRow.noteData NoteType.HOLDHEAD |> Bitmap.toSeq do
                        yield HoldNote ((columnToX k, 240.0), offset, ln_lookahead k ss, enum 0, (enum 0, enum 0, 0, 0, ""))
                    yield! convert ss
                | [] -> ()
            }
            convert snaps |> Seq.toList

        let rec private bpmDurations points (endTime: Time) = 
            if List.isEmpty points then failwith "no bpm point"
            let (offset, (_, msPerBeat)) = (List.head points)
            let mutable current : float32<ms/beat> = msPerBeat
            let mutable t : Time = offset
            let data = new Dictionary<float32<ms/beat>, Time>()

            for (offset, (_, msPerBeat)) in points do
                if (not (data.ContainsKey current)) then data.Add(current, 0.0f<ms>)
                data.[current] <- data.[current] + offset - t; t <- offset; current <- msPerBeat
            if (not (data.ContainsKey current)) then data.Add(current, 0.0f<ms>)
            data.[current] <- data.[current] + endTime - t
            data

        let minMaxBPM bs endTime =
            let d = (bpmDurations bs endTime).OrderBy(fun p -> p.Key)
            (d.First().Key, d.Last().Key)

        let private convertToTimingPoints (bpm: TimeData<BPM>) (sv: MultiTimeData<float32>) (endTime: Time) =
            let corrective_sv offset mult = 
                if (sv.GetChannelData -1).Count = 0 then None else
                    match (sv.GetChannelData -1).IndexAt offset with
                    | (-1, false) -> None
                    | (i, false) ->
                        let (offset, value) = (sv.GetChannelData -1).GetPointAt offset in
                            Some (TimingPoint.SV (offset, mult * value, (SampleSet.Soft, 0, 10), enum 0))
                    | _ -> None

            let svs time1 time2 mult =
                seq {
                    match corrective_sv time1 mult with
                    | None -> ()
                    | Some x -> yield x
                    for (offset, value) in (sv.GetChannelData -1).EnumerateBetween time1 time2 do
                        yield TimingPoint.SV (offset, value / mult, (SampleSet.Soft, 0, 10), enum 0)
                }

            let tps =
                seq {
                    let mutable bs = bpm.Data |> List.ofSeq
                    if List.isEmpty bs then ()
                    else
                        let mostCommonBpm = (bpmDurations bs endTime).OrderByDescending(fun p -> p.Value).First().Key
                        yield! svs (-Time.infinity) (bs |> List.head |> offsetOf) 1.0f
                        while bs |> List.isEmpty |> not do
                            match bs with
                            | (offset, (meter, beatLength)) :: (offset2, _) :: rs ->
                                yield TimingPoint.BPM (offset, beatLength, meter, (SampleSet.Soft, 0, 10), enum 0)
                                yield! svs offset offset2 (mostCommonBpm / beatLength)
                                bs <- List.tail bs
                            | (offset, (meter, beatLength)) :: [] ->
                                yield TimingPoint.BPM (offset, beatLength, meter, (SampleSet.Soft, 0, 10), enum 0)
                                yield! svs offset Time.infinity (mostCommonBpm / beatLength)
                                bs <- List.tail bs
                            | _ -> failwith "impossible"
                }
            tps |> List.ofSeq

        let convert (chart: Chart) : Beatmap =
            let general = 
                { General.Default with
                    AudioFilename = match chart.Header.AudioFile with Relative s -> s | Absolute s -> Path.GetFileName s
                    PreviewTime = chart.Header.PreviewTime
                    SampleSet = SampleSet.Soft
                    Mode = GameMode.Mania
                }
            let editor = Editor.Default
            let meta =
                { Metadata.Default with
                    Title = chart.Header.Title
                    TitleUnicode = chart.Header.Title
                    Artist = chart.Header.Artist
                    ArtistUnicode = chart.Header.Artist
                    Creator = chart.Header.Creator
                    Version = chart.Header.DiffName
                }
            let diff =
                { Difficulty.Default with
                    CircleSize = float chart.Keys
                    OverallDifficulty = 8.0
                    HPDrainRate = 8.0
                }
            {
                General = general
                Editor = editor
                Metadata = meta
                Difficulty = diff
                Events = [ Background ((match chart.Header.BackgroundFile with Relative s -> s | Absolute s -> Path.GetFileName s), (0.0, 0.0)) ]
                Objects = convertSnapsToHitobjects (List.ofSeq (chart.Notes.Data)) chart.Keys
                Timing = convertToTimingPoints chart.BPM chart.SV chart.LastNote
            }

    (*
        Conversion code for Interlude -> StepMania
    *)

    let convert_interlude_stepmania (chart: Chart) : StepmaniaData = failwith "nyi"

    (*
        Overall utilities to dynamically load different chart files and convert to Interlude format
    *)

    let (|ChartFile|_|) (path: string) =
        let s = Path.GetExtension(path).ToLower()
        match s with
        | ".yav" | ".sm" | ".osu" -> Some s
        | _ -> None

    let (|ChartArchive|_|) (path: string) = 
        match Path.GetExtension(path).ToLower() with
        | ".osz" | ".zip" -> Some ()
        | _ -> None

    let (|SongFolder|_|) (path: string) =
        if Directory.Exists path then
            Directory.EnumerateFiles path
            |> Seq.tryPick (fun x -> match x with ChartFile s -> Some s | _ -> None)
        else None
    
    let (|PackFolder|_|) (path: string) =
        if Directory.Exists path then
            Directory.EnumerateDirectories path
            |> Seq.forall (fun x -> match x with SongFolder _ -> false | _ -> true)
            |> fun b -> if b then None else Some ()
        else None

    let (|FolderOfPacks|_|) (path: string) =
        if Directory.Exists path then
            Directory.EnumerateDirectories path
            |> Seq.forall (fun x -> match x with PackFolder -> false | _ -> true)
            |> fun b -> if b then None else Some ()
        else None

    let loadAndConvertFile (action: ConversionAction) : Chart list =
        match Path.GetExtension(action.Source).ToLower() with
        | ".yav" ->
            match Chart.fromFile action.Source with
            | Some chart -> [chart]
            | None -> []
        | ".sm" -> ``StepMania to Interlude``.convert (loadStepmaniaFile action.Source) action
        | ".osu" -> 
            try
                let map = loadBeatmapFile action.Source
                if 
                    map.General.Mode = GameMode.Mania
                    && (let keys = map.Difficulty.CircleSize |> int in 3 <= keys && keys <= 10)
                    && not (``osu! to Interlude``.looks_like_a_rate map)
                then
                    [ ``osu! to Interlude``.convert (loadBeatmapFile action.Source) action ] else []
            with err -> Logging.Debug ("Skipped .osu file: " + action.Source, err); []
        | _ -> []

    /// Writes chart to new location, including copying its background and audio files if needed
    let relocateChart (action: ConversionAction) (chart: Chart) =

        let sourceFolder = Path.GetDirectoryName action.Source
        let targetFolder = action.TargetDirectory

        let copyFile (file: MediaPath) =
            match file with
            | Relative "" -> Relative ""
            | Absolute s -> Absolute s
            | Relative file ->
                if action.Config.CopyMediaFiles then
                    let source = Path.Combine (sourceFolder, file)
                    let target = Path.Combine (targetFolder, file)
                    if File.Exists source then
                        if not (File.Exists target) then
                            try File.Copy (source, target)
                            with err -> Logging.Error ("Could not copy media file from " + source, err)
                    else Logging.Warn ("Missing media file at " + source)
                    Relative file
                else
                    Absolute (Path.Combine (sourceFolder, file))

        Directory.CreateDirectory targetFolder |> ignore

        let c = 
            Chart(
                chart.Keys,
                { chart.Header with 
                    SourcePack = action.Config.PackName
                    BackgroundFile = copyFile chart.Header.BackgroundFile
                    AudioFile = copyFile chart.Header.AudioFile
                },
                chart.Notes, chart.BPM, chart.SV,
                Path.Combine (targetFolder, Path.ChangeExtension (Path.GetFileName chart.FileIdentifier, ".yav"))
            )

        Chart.save c
        c