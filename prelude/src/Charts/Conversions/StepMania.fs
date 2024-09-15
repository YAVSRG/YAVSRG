namespace Prelude.Charts.Conversions

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Charts.Formats.StepMania
open Prelude.Charts

module StepMania_To_Interlude =

    let private convert_measures
        (keys: int)
        (measures: string list list)
        (bpms: (float32<beat> * float32<beat / minute>) list)
        (stops: (float32<beat> * float32) list)
        start
        =

        let mutable bpms = bpms
        let mutable stops = stops
        let meter = 4<beat>
        let fmeter = float32 meter * 1.0f<beat>
        let states = new List<TimeItem<NoteRow>>()
        let points = new List<TimeItem<BPM>>()
        let mutable ln: Bitmask = 0us
        let mutable now = start
        let (_, b) = List.head bpms

        points.Add(
            {
                Time = start
                Data =
                    {
                        Meter = meter
                        MsPerBeat = 60000.0f<ms / minute> / b
                    }
            }
        )

        let mutable ms_per_beat = 60000.0f<ms / minute> / b
        if ms_per_beat < 0.0f<ms/beat> then skip_conversion "SM file has negative BPMs"
        bpms <- List.tail bpms
        let mutable total_beats = 0.0f<beat>
        let mutable lo = 0.0f<beat>
        let mutable hi = 0.0f<beat>
        let mutable point_at_end_of_measure = false

        let convert_measure (m: string list) (lo: float32<beat>) (hi: float32<beat>) =
            let l = List.length m |> float32
            let sep = ms_per_beat * fmeter / l
            let start = Math.Ceiling(lo * l / fmeter |> float) |> int
            let finish = Math.Ceiling(hi * l / fmeter |> float) |> int
            let offset = float32 start * sep - lo * ms_per_beat

            for i in start .. (finish - 1) do
                let beat = total_beats + float32 i * fmeter / l

                while ((not (List.isEmpty stops)) && fst (List.head stops) <= beat) do
                    let (_, s) = List.head stops in
                    now <- now + s * 1000.0f<ms>
                    stops <- List.tail stops
                    point_at_end_of_measure <- true

                let nr = NoteRow.create_ln_bodies keys ln

                Seq.iteri
                    (fun k c ->
                        match c with
                        | '0' -> ()
                        | '1' -> 
                            if not (Bitmask.has_key k ln) then
                                nr.[k] <- NoteType.NORMAL
                        | '2'
                        | '4' ->
                            if not (Bitmask.has_key k ln) then
                                nr.[k] <- NoteType.HOLDHEAD
                                ln <- Bitmask.set_key k ln
                        | '3' ->
                            if Bitmask.has_key k ln then
                                nr.[k] <- NoteType.HOLDTAIL
                                ln <- Bitmask.unset_key k ln
                        | 'M'
                        | 'L'
                        | 'F' -> () // ignore mines, lifts, fakes
                        | _ -> skip_conversion (sprintf "Unknown note type '%c'" c)
                    )
                    m.[i]

                if NoteRow.is_empty nr |> not then
                    states.Add(
                        {
                            Time = now + offset + float32 (i - start) * sep
                            Data = nr
                        }
                    )

        List.iteri
            (fun i m ->
                point_at_end_of_measure <- false
                lo <- 0.0f<beat>

                while ((not (List.isEmpty bpms)) && fst (List.head bpms) < total_beats + fmeter) do
                    hi <- fst (List.head bpms) - total_beats
                    convert_measure m lo hi
                    now <- now + ms_per_beat * (hi - lo)
                    lo <- hi
                    point_at_end_of_measure <- true
                    let (_, b) = List.head bpms in

                    points.Add(
                        {
                            Time = now
                            Data =
                                {
                                    Meter = meter
                                    MsPerBeat = 60000.0f<ms / minute> / b
                                }
                        }
                    )

                    ms_per_beat <- 60000.0f<ms / minute> / b
                    if ms_per_beat < 0.0f<ms/beat> then skip_conversion "SM file has negative BPMs"
                    bpms <- List.tail bpms

                convert_measure m lo fmeter
                now <- now + ms_per_beat * (fmeter - lo)
                total_beats <- total_beats + fmeter

                if point_at_end_of_measure then
                    points.Add(
                        {
                            Time = now
                            Data = { Meter = meter; MsPerBeat = ms_per_beat }
                        }
                    )
            )
            measures

        (states |> Array.ofSeq, points |> Array.ofSeq)

    let convert (sm: StepManiaFile) (action: ConversionAction) : Result<ImportChart, SkippedConversion> list =

        let path = Path.GetDirectoryName action.Source

        let rec metadata_fallback x =
            match x with
            | "" :: xs -> metadata_fallback xs
            | s :: _ -> s.Trim()
            | [] -> ""

        let rec metadata_fallback_opt x =
            match x with
            | "" :: xs -> metadata_fallback_opt xs
            | s :: _ -> Some(s.Trim())
            | [] -> None

        let find_background () : string option =
            let guesses =
                [
                    sm.BACKGROUND
                    Path.ChangeExtension(sm.BACKGROUND, ".png")
                    Path.ChangeExtension(sm.BACKGROUND, ".jpg")
                    sm.TITLE + "-bg.jpg"
                    sm.TITLE + "-bg.png"
                    "bg.png"
                    "bg.jpg"
                    "background.png"
                    "background.jpg"
                ]

            match List.tryFind (fun guess -> File.Exists(Path.Combine(path, guess))) guesses with
            | Some p -> Some p
            | None ->

            let files = Directory.GetFiles path |> Array.map Path.GetFileName

            match
                files
                |> Array.tryFind (fun filename ->
                    filename.ToLower().Contains "bg" || filename.ToLower().Contains "back"
                )
            with
            | Some p -> Some p
            | None ->

            let image_files =
                files
                |> Array.filter (fun f -> f.ToLower().Contains ".jpg" || f.ToLower().Contains ".png")
                |> Array.map (fun file ->
                    try
                        let info = Bitmap.Identify(Path.Combine(path, file))
                        file, info.Width * info.Height
                    with err ->
                        file, 0
                )

            if image_files.Length >= 3 then //expect a bg, bn and cdtitle
                fst (Array.sortByDescending snd image_files).[0] |> Some
            else
                None

        let find_audio () : string option =
            if File.Exists(Path.Combine(path, sm.MUSIC)) then
                Some sm.MUSIC
            else

            let files = Directory.GetFiles path |> Array.map Path.GetFileName

            files
            |> Array.tryFind (fun filename ->
                filename.ToLower().Contains "mp3"
                || filename.ToLower().Contains "wav"
                || filename.ToLower().Contains "ogg"
            )

        let guess_author () : string =
            let folder_name = Path.GetFileName path
            let round_paren = folder_name.LastIndexOf('(')
            let square_paren = folder_name.LastIndexOf('[')

            if round_paren > square_paren then
                let guess = folder_name.Substring(round_paren + 1)
                if guess.EndsWith(')') then guess.TrimEnd ')' else ""
            elif square_paren > 1 then
                let guess = folder_name.Substring(square_paren + 1)
                if guess.EndsWith(']') then guess.TrimEnd ']' else ""
            else
                "Unknown"

        let choose_translit (original: string) (translit: string) : string * string option =
            if translit = "" then
                original, None
            elif original = "" then
                translit, None
            elif (original |> Seq.filter Char.IsAscii |> Seq.length) > (translit |> Seq.filter Char.IsAscii |> Seq.length) then
                original, Some translit
            else translit, Some original

        let convert_difficulty (i: int) (diff: StepManiaChart) : Result<ImportChart, SkippedConversion> =
            try
                let keys = diff.STEPSTYPE.Keycount
                let title, title_native = choose_translit sm.TITLE sm.TITLETRANSLIT
                let artist, artist_native = choose_translit sm.ARTIST sm.ARTISTTRANSLIT

                let header =
                    {
                        Title = title
                        TitleNative = title_native
                        Artist = artist
                        ArtistNative = artist_native
                        Creator = metadata_fallback [ if diff.CREDIT.Contains("Copied f") then "" else diff.CREDIT.Trim(); sm.CREDIT.Trim(); guess_author () ]
                        DiffName = sprintf "%O %O %O" diff.STEPSTYPE diff.DIFFICULTY diff.METER
                        Subtitle = metadata_fallback_opt [ sm.SUBTITLETRANSLIT; sm.SUBTITLE ]
                        Tags =
                            sm.GENRE.Trim().Split(" ", StringSplitOptions.RemoveEmptyEntries)
                            |> List.ofArray
                        Source = None

                        PreviewTime = sm.SAMPLESTART * 1000.0f<ms>
                        AudioFile =
                            match find_audio () with
                            | Some file ->
                                if action.Config.MoveAssets then
                                    ChartImportAssetPath.Relative file
                                else
                                    ChartImportAssetPath.Absolute(Path.Combine(path, file))
                            | None ->
                                //Logging.Warn(sprintf "Audio file for %s not found: %s" path sm.MUSIC)
                                ChartImportAssetPath.Missing
                        BackgroundFile =
                            match find_background () with
                            | Some file ->
                                if action.Config.MoveAssets then
                                    ChartImportAssetPath.Relative file
                                else
                                    ChartImportAssetPath.Absolute(Path.Combine(path, file))
                            | None ->
                                //Logging.Warn(sprintf "Background file for %s not found: %s" path sm.BACKGROUND)
                                ChartImportAssetPath.Missing

                        ChartSource = 
                            match action.Config.EtternaPackName with 
                            | Some pack -> ChartImportOrigin.Etterna pack
                            | None -> ChartImportOrigin.Unknown
                    }

                let (notes, bpm) =
                    convert_measures keys diff.NOTES sm.BPMS sm.STOPS (-sm.OFFSET * 1000.0f<ms>)

                if notes.Length = 0 then
                    skip_conversion "StepMania chart has no notes"

                Ok {
                    Header = header
                    LoadedFromPath = action.Source
                    PackName = action.Config.PackName
                    Chart = {
                        Keys = keys
                        Notes = notes
                        BPM = bpm
                        SV = [||]
                    }
                }
            with
            | :? ConversionSkipException as skip_reason -> 
                Error (action.Source, skip_reason.msg)
            | other_error ->
                Logging.Debug(sprintf "Unexpected error converting %s" action.Source, other_error)
                Error (action.Source, other_error.Message)

        sm.Charts |> List.mapi convert_difficulty
