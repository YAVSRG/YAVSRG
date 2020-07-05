namespace Prelude.Charts

open System
open System.IO
open System.Collections.Generic
open System.Linq
open Prelude.Common
open Prelude.Charts.osu
open Prelude.Charts.Interlude
open Prelude.Charts.StepMania

module ChartConversions =

    (*
        Conversion code for osu -> Interlude
    *)

    let private listToDotNet (list : 't list) = 
        let result = ResizeArray<'t> list
        result.Reverse()
        result

    let private convertHitObjects (objects : HitObject list) (keys: int) : TimeData<NoteRow> =
        let xToColumn (x : float) = (x / (512.0 / (keys |> float))) |> int

        let getMiddles (holds : (Time array)) = 
            makeBitmap (seq {
            for i = 0 to (holds.Length - 1) do
                if holds.[i] >= 0.0f<ms> then yield i
            })

        let updateHolds time (snaps, holds) =
            let mutable s = snaps
            let mutable minT = Array.fold (fun y x -> if x = -1.0f<ms> then y else min y x) (infinityf * 1.0f<ms>) holds
            while minT < time do
                for k in 0..(keys - 1) do
                    if (holds.[k] >= 0.0f<ms> && holds.[k] = minT) then 
                        match s with
                          | (time, nr) :: ss ->
                            holds.[k] <- -1.0f<ms>;
                            if time = minT then
                                applyToNoteData NoteType.HOLDTAIL (setBit k) nr
                                applyToNoteData NoteType.HOLDBODY (unsetBit k) nr
                            else s <- (minT, (makeNoteRow 0us 0us (getMiddles holds) (makeBitmap (seq [k])) 0us 0us 0us)) :: s
                          | [] -> failwith "impossible"
                minT <- Array.fold (fun y x -> if x = -1.0f<ms> then y else min y x) (infinityf * 1.0f<ms>) holds
            (s, holds)

        let note k time (snaps, holds) =
            match snaps with
                (t, nr) :: ss ->
                    if t = time then applyToNoteData NoteType.NORMAL (setBit k) nr; (snaps, holds)
                    else ((time, (makeNoteRow (makeBitmap (seq [k])) 0us (getMiddles holds) 0us 0us 0us 0us)) :: snaps, holds)
               | [] -> ((time, (makeNoteRow (makeBitmap (seq [k])) 0us (getMiddles holds) 0us 0us 0us 0us)) :: snaps, holds)

        let hold (k : int) time (release : Time) (snaps, (holds : Time array)) =
            assert (holds.[k] = -1.0f<ms>)
            let middles = getMiddles holds
            holds.[k] <- release;
            match snaps with
                | (t, nr) :: ss ->
                    if t = time then applyToNoteData NoteType.HOLDHEAD (setBit k) nr; (snaps, holds)
                    else ((time, (makeNoteRow 0us (makeBitmap (seq [k])) middles 0us 0us 0us 0us)) :: snaps, holds)
                | [] -> ((time, (makeNoteRow 0us (makeBitmap (seq [k])) middles 0us 0us 0us 0us)) :: snaps, holds)

        let f (snaps, holds) (hitObj : HitObject) =
            match hitObj with
                HitCircle ((x,y), time, _, _) -> note (xToColumn x) time (updateHolds time (snaps, holds))
              | HoldNote ((x,y), time, endTime, _, _) -> hold (xToColumn x) time endTime (updateHolds time (snaps, holds))
              | _ -> (snaps, holds)

        let (states, _) = updateHolds (infinityf * 1.0f<ms>) (List.fold f ([], Array.create 4 -1.0f<ms>) objects)
        TimeData<NoteRow>(listToDotNet states)

    let private convertTimingPoints (points: TimingPoint list) (keys : int) (endTime : Time) : (TimeData<BPM> * MultiTimeData<float32>) =
        let rec bpmDurations points = 
            if List.isEmpty points then failwith "no bpm point"
            match (List.head points) with
                | (TimingPoint.BPM (offset, msPerBeat, _, _, _)) ->
                    let mutable current : float32<ms/beat> = msPerBeat
                    let mutable t : Time = offset
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
            let func ((bpm, sv, scroll) : (TimeDataItem<BPM> list * TimeDataItem<float32> list * float32)) (point : TimingPoint) : (TimeDataItem<BPM> list * TimeDataItem<float32> list * float32) =
                match point with
                    | (TimingPoint.BPM (offset, msPerBeat, meter, _, _)) -> 
                        (((offset, (meter, msPerBeat)) :: bpm), addOrSkipSV (offset, (mostCommonBpm / msPerBeat)) sv, mostCommonBpm / msPerBeat)
                    | (TimingPoint.SV (offset, value, _, _)) ->
                        (bpm, addOrSkipSV (offset, (value * scroll)) sv, scroll)
            List.fold func ([], [], 1.0f) points

        let svData = MultiTimeData<float32>(keys)
        svData.SetChannelData(-1, listToDotNet sv)
        (TimeData(listToDotNet bpm), svData)

    let convert_osu_interlude ((general, _, meta, diff, events, notes, timing) : Beatmap) =
        let keys = diff.CircleSize |> int
        let rec findBGFile e =
            match e with
                | (Background (path, _)) :: _ -> path
                | _ :: es -> findBGFile es
                | [] -> ""
        let header = { 
            ChartHeader.Default with
                Title = meta.Title; Artist = meta.Artist; Creator = meta.Creator; SourcePack = "osu!"
                DiffName = meta.Version; PreviewTime = general.PreviewTime; BGFile = findBGFile events; AudioFile = general.AudioFilename }
        let snaps = convertHitObjects notes keys
        let (bpm, sv) = (convertTimingPoints timing keys (offsetOf (snaps.GetPointAt(infinityf * 1.0f<ms>))))
        Chart(keys, header, snaps, bpm, sv, String.Join("_", (meta.Title + " [" + meta.Version + "].yav").Split(Path.GetInvalidFileNameChars())))

    (*
        Conversion code for StepMania -> Interlude
        Todo: Support freezes, warps, stops
    *)

    let private convert_measures measures (bpms: (float32<beat> * float32<beat/minute>) list) start = 
        let mutable bpms = bpms
        let meter = 4<beat>
        let fmeter = float32 meter * 1.0f<beat>
        let states = new List<(Time * NoteRow)>()
        let points = new List<(Time * BPM)>()
        let mutable ln : Bitmap = 0us
        let mutable now = start
        let (_, b) = List.head bpms in points.Add(start, (meter, 60000.0f<ms/minute> / b))
        let mutable msPerBeat = 60000.0f<ms/minute> / b
        bpms <- List.tail bpms
        let mutable totalBeats = 0.0f<beat>;
        let mutable lo = 0.0f<beat>
        let mutable hi = 0.0f<beat>

        let convert_measure (m : string list) (lo: float32<beat>) (hi: float32<beat>) =
            let l = List.length m |> float32
            let sep = msPerBeat * fmeter / l
            let start = Math.Ceiling(lo * l / fmeter |> float) |> int
            let finish = Math.Ceiling(hi * l / fmeter |> float) |> int
            let offset = now + (float32 start * sep) - (lo * msPerBeat)

            for i in start..(finish-1) do
                let nr = makeNoteRow 0us 0us ln 0us 0us 0us 0us
                Seq.iteri (fun k c ->
                    match c with
                    | '0' -> ()
                    | '1' -> applyToNoteData NoteType.NORMAL (setBit i) nr
                    | '2' | '4' ->
                        applyToNoteData NoteType.HOLDHEAD (setBit i) nr
                        ln <- setBit i ln
                    | '3' ->
                        applyToNoteData NoteType.HOLDTAIL (setBit i) nr
                        applyToNoteData NoteType.HOLDBODY (unsetBit i) nr
                        ln <- unsetBit i ln
                    | 'M' -> applyToNoteData NoteType.MINE (setBit i) nr
                    | _ -> failwith ("unknown note type " + c.ToString())
                    ) m.[i]
                if isEmptyNoteRow nr then states.Add((offset + float32 (i - start) * sep),nr)

        List.iteri (fun i m -> 
            totalBeats <- totalBeats + float32 meter * 1.0f<beat>
            lo <- 0.0f<beat>
            while (not (List.isEmpty bpms) && fst (List.head bpms) < totalBeats) do
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


    let convert_stepmania_interlude (sm : StepmaniaData) path = 
        let rec metadataFallback x =
            match x with
                | "" :: xs -> metadataFallback xs
                | s :: xs -> s
                | [] -> "----"
        let findBackground guess : string =
            if (not (File.Exists(Path.Combine(path, guess)))) then
                let mutable result = ""
                //todo: fix this going through all files
                for s in Directory.GetFiles(path) do
                    let filename = Path.GetFileNameWithoutExtension(s).ToLower()
                    if (filename.Contains("bg") || filename.Contains("background")) then result <- Path.GetFileName(s)
                result
            else guess
        
        let convert_difficulty (i : int) (diff : ChartData) : Chart option = 
            let keys = keyCount diff.STEPSTYPE
            let header = {
                ChartHeader.Default with
                    Title = metadataFallback [sm.TITLETRANSLIT; sm.TITLE]
                    Artist = metadataFallback [sm.ARTISTTRANSLIT; sm.ARTIST]
                    Creator = metadataFallback [sm.CREDIT; diff.CREDIT]
                    SourcePack = "Singles"
                    DiffName = metadataFallback [sm.SUBTITLETRANSLIT; sm.SUBTITLE;
                        diff.CHARTNAME; diff.DESCRIPTION; diff.CHARTSTYLE; diff.STEPSTYPE.ToString() + " " + diff.METER.ToString()]
                    PreviewTime = sm.SAMPLESTART * 1000.0f<ms>
                    AudioFile = metadataFallback [sm.MUSIC; "audio.mp3"]
                    BGFile = metadataFallback [sm.BACKGROUND; findBackground (sm.TITLE + "-bg.jpg")]
            }
            let filepath = Path.Combine(path, diff.STEPSTYPE.ToString() + " " + diff.METER.ToString() + " [" + (string i) + "].yav")
            let (notes, bpm) = convert_measures diff.NOTES sm.BPMS (-sm.OFFSET * 1000.0f<ms>)
            Some (Chart(keys, header, notes, bpm, MultiTimeData<float32>(keys), filepath))
        sm.Charts |> List.mapi convert_difficulty |> List.choose id

    (*
        Conversion code for Interlude -> osu
    *)

    let private convertSnapsToHitobjects snaps keys =
        let columnToX k = (float k + 0.5) * 512.0 / (float keys) |> round
        let rec ln_lookahead k snaps =
            match snaps with
            | (offset, nr) :: ss -> if testForNote k NoteType.HOLDTAIL nr then offset else ln_lookahead k ss
            | [] -> failwith "hold note has no end"

        let rec convert snaps = seq {
            match snaps with
            | (offset, nr) :: ss ->
                for k in nr |> noteData NoteType.NORMAL |> getBits do
                    yield HitCircle ((columnToX k, 240.0), offset, enum 0, (enum 0, enum 0, 0, 0, ""))
                for k in nr |> noteData NoteType.HOLDHEAD |> getBits do
                    yield HoldNote ((columnToX k, 240.0), offset, ln_lookahead k ss, enum 0, (enum 0, enum 0, 0, 0, ""))
                yield! convert ss
            | [] -> ()
        }
        convert snaps |> Seq.toList

    let rec private bpmDurations points (endTime : Time) = 
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

    let private convertToTimingPoints (bpm : TimeData<BPM>) (sv : MultiTimeData<float32>) (endTime : Time) =
        let corrective_sv offset mult = 
            if (sv.GetChannelData -1).Count = 0 then None else
                match (sv.GetChannelData -1).IndexAt offset with
                | (-1, false) -> None
                | (i, false) ->
                    let (offset, value) = (sv.GetChannelData -1).GetPointAt offset in
                        Some (TimingPoint.SV (offset, mult * value, (SampleSet.Soft, 0, 10), enum 0))
                | _ -> None

        let svs time1 time2 mult = seq {
            match corrective_sv time1 mult with
            | None -> ()
            | Some x -> yield x
            for (offset, value) in (sv.GetChannelData -1).EnumerateBetween time1 time2 do
                yield TimingPoint.SV (offset, value / mult, (SampleSet.Soft, 0, 10), enum 0)
        }

        let tps =
            seq {
                //todo: can be refactored as recursion
                let mutable bs = bpm.Data |> List.ofSeq
                if List.isEmpty bs then ()
                else
                    let mostCommonBpm = (bpmDurations bs endTime).OrderByDescending(fun p -> p.Value).First().Key
                    yield! svs (-infinityf * 1.0f<ms>) (bs |> List.head |> offsetOf) 1.0f
                    while bs |> List.isEmpty |> not do
                        match bs with
                        | (offset, (meter, beatLength)) :: (offset2, _) :: rs ->
                            yield TimingPoint.BPM (offset, beatLength, meter, (SampleSet.Soft, 0, 10), enum 0)
                            yield! svs offset offset2 (mostCommonBpm / beatLength)
                            bs <- List.tail bs
                        | (offset, (meter, beatLength)) :: [] ->
                            yield TimingPoint.BPM (offset, beatLength, meter, (SampleSet.Soft, 0, 10), enum 0)
                            yield! svs offset (infinityf * 1.0f<ms>) (mostCommonBpm / beatLength)
                            bs <- List.tail bs
                        | _ -> failwith "impossible"
            }
        tps |> List.ofSeq

    let convert_interlude_osu (chart : Chart) : Beatmap =
        let general = {
            General.Default with
                AudioFilename = chart.Header.AudioFile
                PreviewTime = chart.Header.PreviewTime
                SampleSet = SampleSet.Soft
                Mode = GameMode.Mania
        }
        let editor = Editor.Default
        let meta = {
            Metadata.Default with
                Title = chart.Header.Title
                TitleUnicode = chart.Header.Title
                Artist = chart.Header.Artist
                ArtistUnicode = chart.Header.Artist
                Creator = chart.Header.Creator
                Version = chart.Header.DiffName
        }
        let diff = {
            Difficulty.Default with
                CircleSize = float chart.Keys
                OverallDifficulty = 8.0
                HPDrainRate = 8.0
        }
        (general, editor, meta, diff, [(Background (chart.Header.BGFile, (0.0, 0.0)))],
            convertSnapsToHitobjects (List.ofSeq (chart.Notes.Data)) chart.Keys,
            convertToTimingPoints chart.BPM chart.SV (offsetOf (chart.Notes.GetPointAt(infinityf * 1.0f<ms>))))

    (*
        Conversion code for Interlude -> StepMania
    *)

    let convert_interlude_stepmania (chart : Chart) : StepmaniaData = failwith "nyi"

    (*
        Overall utilities to dynamically load different chart files and convert to interlude format
    *)

    let (|ChartFile|_|) (path : string) = 
        match Path.GetExtension(path).ToLower() with
        | ".yav" | ".sm" | ".osu" -> Some ()
        | _ -> None

    let (|ChartArchive|_|) (path : string) = 
        match Path.GetExtension(path).ToLower() with
        | ".osz" -> Some ()
        | ".zip" -> failwith "nyi"
        | _ -> None

    let (|SongFolder|_|) (path : string) =
        Directory.EnumerateFiles(path)
        |> Seq.forall (fun x -> match x with ChartFile -> false | _ -> true)
        |> fun b -> if b then None else Some ()
    
    let (|PackFolder|_|) (path : string) =
        Directory.EnumerateDirectories(path)
        |> Seq.forall (fun x -> match x with SongFolder -> false | _ -> true)
        |> fun b -> if b then None else Some ()

    let (|FolderOfPacks|_|) (path : string) =
        Directory.EnumerateDirectories(path)
        |> Seq.forall (fun x -> match x with PackFolder -> false | _ -> true)
        |> fun b -> if b then None else Some ()

    let loadAndConvertFile (path : string) : Chart list =
        match Path.GetExtension(path).ToLower() with
        | ".yav" ->
            match loadChartFile path with
            | Some chart -> [chart]
            | None -> []
        | ".sm" -> convert_stepmania_interlude (loadStepmaniaFile path) (Path.GetDirectoryName path)
        | ".osu" -> 
            try
                let map = loadBeatmapFile path
                if getGameMode map = GameMode.Mania && (let (_,_,_,d,_,_,_) = map in (let keys = d.CircleSize |> int in 3 <= keys && keys <= 10)) then
                    [convert_osu_interlude (loadBeatmapFile path)] else []
            with
            | err -> Logging.Error("Could not load .osu file: " + path) (err.ToString()); []
        | _ -> []

    //Writes chart to new location, including copying its background and audio files
    let relocateChart (chart : Chart) (sourceFolder : string) (targetFolder : string) =
        let c =
            Chart(chart.Keys, { chart.Header with SourcePack = Path.GetFileName(Path.GetDirectoryName(targetFolder)) },
                chart.Notes, chart.BPM, chart.SV, Path.Combine(targetFolder, Path.ChangeExtension(Path.GetFileName(chart.FileIdentifier), ".yav")))
    
        Directory.CreateDirectory(targetFolder) |> ignore
        let copyFile source target =
            if (File.Exists(source)) then
                if (not (File.Exists(target))) then
                    try
                        File.Copy(source, target)
                    with
                    | err -> Logging.Error ("Could not copy media file from " + source) (err.ToString())
                else () //fail silently when repeatedly copying
            else Logging.Warn ("Missing media file at " + source) ""
    
        copyFile (Path.Combine(sourceFolder, c.Header.AudioFile)) (Path.Combine(targetFolder, c.Header.AudioFile))
        copyFile (Path.Combine(sourceFolder, c.Header.BGFile)) (Path.Combine(targetFolder, c.Header.BGFile))
        saveChartFile c
        c