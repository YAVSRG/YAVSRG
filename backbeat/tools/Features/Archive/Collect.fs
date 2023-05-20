namespace Backbeat.Features.Archive

open System
open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Formats.Conversions
open Prelude.Data.Charts.Archive
open Backbeat.Utils

module Collect =

    let simplify_string =
        let regex = Text.RegularExpressions.Regex("[^\sa-zA-Z0-9_]")
        fun (s: string) -> regex.Replace(s.ToLowerInvariant(), "").Trim().Replace(" ", "")

    let add_song(song: Song) : SongId =

        let stitle = simplify_string song.Title
        let mutable existing_id = None
        let mutable similar_id = None
        for other_song_id in songs.Keys do
            if existing_id.IsNone then
                let other_song = songs.[other_song_id]
                if other_song = song then existing_id <- Some other_song_id
                let mutable similarity = 0

                if stitle = simplify_string other_song.Title then similarity <- similarity + 1
                if song.Source = other_song.Source then similarity <- similarity + 1
                if List.contains song.Title other_song.AlternativeTitles then similarity <- similarity + 1
                if List.contains song.Title other_song.AlternativeTitles then similarity <- similarity + 1
                if other_song.Tags.Length - (List.except song.Tags other_song.Tags).Length > 2 then similarity <- similarity + 1
                if other_song.Artists.Length - (List.except song.Artists other_song.Artists).Length > 1 then similarity <- similarity + 1

                if similarity >= 2 && existing_id.IsNone then
                    similar_id <- Some other_song_id
                    Logging.Info(sprintf "Song similarity (but not identical) to %s" other_song_id)

        match existing_id with
        | None ->
            let id = (simplify_string (List.head song.Artists)) + "/" + (simplify_string song.Title)
            let mutable i = 0
            while songs.ContainsKey(if i > 0 then id + "-" + i.ToString() else id) do
                i <- i + 1
            let id = if i > 0 then id + "-" + i.ToString() else id
            songs.Add(id, song)
            Logging.Info(sprintf "+ Song: %s" id)
            match similar_id with
            | Some s -> Queue.append "songs-similar" (s + ", " + id)
            | None -> ()
            id
        | Some id -> id

    let slurp_chart (extraSources: ChartSource list) (chart: Charts.Formats.Interlude.Chart) =
        if not (File.Exists chart.AudioPath) then
            Logging.Info(sprintf "Rejecting %s because it doesn't have audio" chart.Header.Title)
        elif not (File.Exists chart.BackgroundPath) then
            Logging.Info(sprintf "Rejecting %s because it doesn't have a background image" chart.Header.Title)
        else

        let create_entry(song_id) =
            let lastNote = chart.LastNote
            {
                SongId = song_id
                Creators = [chart.Header.Creator]
                Keys = chart.Keys
                DifficultyName = chart.Header.DiffName
                Subtitle = chart.Header.Subtitle
                Tags = chart.Header.Tags
                Duration = lastNote - chart.FirstNote
                Notecount =
                    let mutable count = 0
                    for row in chart.Notes do
                        for k = 0 to chart.Keys - 1 do
                            if row.Data.[k] = NoteType.NORMAL || row.Data.[k] = NoteType.HOLDHEAD then count <- count + 1
                    count
                BPM = ``Interlude to osu!``.minMaxBPM (List.ofSeq chart.BPM) lastNote
                Sources = 
                    match chart.Header.ChartSource with
                    | Charts.Formats.Interlude.ChartSource.Osu (set, id) -> [Osu {| BeatmapSetId = set; BeatmapId = id |}]
                    | Charts.Formats.Interlude.ChartSource.Stepmania (id) -> [Stepmania id]
                    | Charts.Formats.Interlude.ChartSource.Unknown -> []
                    @ extraSources
                LastUpdated = DateTime.Now
            }
        
        let hash = Chart.hash chart
        if charts.ContainsKey hash then
            let chart = charts.[hash]
            let chart_entry = create_entry chart.SongId
            charts.[hash] <- 
                { chart_entry with
                    Tags = List.distinct (chart_entry.Tags @ chart.Tags)
                    Sources = List.distinct (chart_entry.Sources @ chart.Sources)
                }
            Logging.Info(sprintf "^ Chart: %s" hash)
        else

        let song: Song =
            {
                Artists = [chart.Header.Artist]
                OtherArtists = []
                Remixers = []
                Title = chart.Header.Title
                AlternativeTitles = match chart.Header.TitleNative with Some x -> [x] | None -> []
                Source = chart.Header.Source
                Tags = chart.Header.Tags
            }
        let song_id = add_song song

        // todo: record transliterations of artists as alternatives
        //match chart.Header.ArtistNative with Some a -> add_artist_alias (chart.Header.Artist, a) | None -> ()
        
        let chart_entry = create_entry song_id
        charts.Add(hash, chart_entry)
        Logging.Info(sprintf "+ Chart: %s" hash)

    let slurp_song_folder (sm_pack_id: int option) (target: string) =
        for file in Directory.EnumerateFiles target do
            match file with
            | ChartFile _ ->
                try
                    let a = 
                        { 
                            Config = { ConversionActionConfig.Default with StepmaniaPackId = sm_pack_id; CopyMediaFiles = false }
                            Source = file
                            TargetDirectory = Path.Combine (ARCHIVE_PATH, "yav")
                        }
                    loadAndConvertFile a
                    |> List.map (relocateChart a)
                    |> List.iter (fun c -> slurp_chart [] c)
                with err -> Logging.Info(sprintf "Failed to load/convert file: %s" target)
            | _ -> ()

    let slurp_pack_folder (sm_pack_id: int option) (target: string) =
        for songFolder in Directory.EnumerateDirectories target do slurp_song_folder sm_pack_id songFolder

    let slurp_folder_of_oszs (community_pack_id: int) (target: string) =
        for osz in Directory.EnumerateFiles target |> Seq.filter (fun f -> Path.GetExtension(f).ToLower() = ".osz") do
            let extracted_path = Path.GetFileNameWithoutExtension(osz)
            if not (Directory.Exists extracted_path) then
                ZipFile.ExtractToDirectory(osz, Path.GetFileNameWithoutExtension osz)
            for file in Directory.EnumerateFiles extracted_path do
                match file with
                | ChartFile _ ->
                    try
                        let a = 
                            { 
                                Config = { ConversionActionConfig.Default with CopyMediaFiles = false }
                                Source = file
                                TargetDirectory = Path.Combine (ARCHIVE_PATH, "yav")
                            }
                        loadAndConvertFile a
                        |> List.map (relocateChart a)
                        |> List.iter (fun chart -> slurp_chart [ CommunityPack {| PackId = community_pack_id |} ] chart)
                    with err -> Logging.Info(sprintf "Failed to load/convert file: %s" target)
                | _ -> ()

    let rec try_download_mirrors(mirrors, filepath) : Async<bool> =
        async {
            match mirrors with
            | [] -> return false
            | url :: xs ->
                if not (File.Exists filepath) then
                    Logging.Info(sprintf "Downloading from %s ..." url)
                    let! result = Prelude.Data.WebServices.download_file.RequestAsync(url, filepath, ignore)
                    if result then return true 
                    else 
                        Logging.Error(sprintf "Download from %s failed." url)
                        return! try_download_mirrors (xs, filepath)
                else 
                    Logging.Info(sprintf "Download from %s already on disk ..." url)
                    return true
        }

    let download_pack(id: StepmaniaPackId) =
        async {
            let mirrors = packs.Stepmania.[id].Mirrors |> List.map DownloadUrl.unpickle
            let zip = Path.Combine(ARCHIVE_PATH, "tmp", string id + ".zip")
            let folder = Path.Combine(ARCHIVE_PATH, "tmp", string id)
            match! try_download_mirrors(mirrors, zip) with
            | true ->
                if Directory.Exists(folder) then Logging.Info("Looks like this pack was already extracted")
                else ZipFile.ExtractToDirectory(zip, folder)
                let pack_folder = (Directory.GetDirectories folder)[0]
                slurp_pack_folder (Some id) pack_folder
                return true
            | false ->
                Logging.Error(sprintf "All mirrors failed.")
                return false
        }
        
    let download_community_pack(id: CommunityPackId) =
        async {
            let mirrors = packs.Community.[id].Mirrors |> List.map DownloadUrl.unpickle
            let zip = Path.Combine(ARCHIVE_PATH, "tmp", "c-" + string id + ".zip")
            let folder = Path.Combine(ARCHIVE_PATH, "tmp", "c-" + string id)
            match! try_download_mirrors(mirrors, zip) with
            | true ->
                if Directory.Exists(folder) then Logging.Info("Looks like this pack was already extracted")
                else ZipFile.ExtractToDirectory(zip, folder)
                for f in Directory.GetDirectories folder do
                    slurp_folder_of_oszs id f
                return true
            | false ->
                Logging.Error(sprintf "All mirrors failed.")
                return false
        }

    let slurp() =
        for c in Queue.get "cpacks-add" do
            let s = c.Split('\t')
            let mutable i = 0
            while packs.Community.ContainsKey i do
                i <- i + 1
            packs.Community.Add(i, { Title = s.[0]; Description = None; Mirrors = [DownloadUrl.create s.[1]]; Size = 0 })
            Queue.append "cpack-imports" (i.ToString())
        save()
        Queue.save "cpacks-add" []

        let mutable packs = Queue.get "cpack-imports"
        while packs <> [] do
            if download_community_pack(int (List.head packs)) |> Async.RunSynchronously then 
                Queue.append "cpack-imports-success" (List.head packs)
                save()
            else Queue.append "cpack-imports-failed" (List.head packs)
            packs <- List.tail packs
            Queue.save "cpack-imports" packs

        let mutable packs = Queue.get "pack-imports"
        while packs <> [] do
            if download_pack(int (List.head packs)) |> Async.RunSynchronously then 
                Queue.append "pack-imports-success" (List.head packs)
                save()
            else Queue.append "pack-imports-failed" (List.head packs)
            packs <- List.tail packs
            Queue.save "pack-imports" packs

    let script() =
        let keywords = ["skwid"; "nuclear"; "compulsive"; "valedumps"; "minty"; "dumpass"; "cola"; " end"; "yolo"; "d e n p a"; "denpa"]
        Queue.save "pack-imports" []
        for p in packs.Stepmania.Keys do
            let title = packs.Stepmania.[p].Title.ToLower()
            if keywords |> List.forall (fun w -> title.Contains(w) |> not) then ()
            else
                Logging.Info packs.Stepmania.[p].Title
                Queue.append "pack-imports" (p.ToString())

