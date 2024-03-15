namespace Backbeat.Features.Archive

open System
open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Conversions
open Prelude.Data.Library.Caching
open Prelude.Backbeat.Archive
open Backbeat.Utils

module Collect =

    // matches lowercase text that could have been transliterated directly from japanese kana
    let romaji_regex =
        Text.RegularExpressions.Regex("^((ssh|cch|ss|kk|tt|pp|ch|sh|[kstnhfmyrwgzdbpj])?y?[aiuoe]|n|tsu|dzu|\s)*$")

    let simplify_string =
        let regex = Text.RegularExpressions.Regex("[^\sa-zA-Z0-9_]")
        fun (s: string) -> regex.Replace(s.ToLowerInvariant(), "").Trim().Replace(" ", "")

    let add_song (song: Song) : SongId =

        let existing_id =
            songs.Keys |> Seq.tryFind (fun other_song_id -> song = songs.[other_song_id])

        match existing_id with
        | None ->
            let id =
                (simplify_string (List.head song.Artists)) + "/" + (simplify_string song.Title)

            let mutable i = 0

            while songs.ContainsKey(if i > 0 then id + "-" + i.ToString() else id) do
                i <- i + 1

            let id = if i > 0 then id + "-" + i.ToString() else id
            songs.Add(id, song)
            Logging.Info(sprintf "+ Song: %s" id)
            id
        | Some id -> id

    let slurp_chart (extraSources: ChartSource list) (chart: Charts.Chart) =
        if chart.Header.AudioFile = Missing then
            Logging.Info(sprintf "Rejecting %s because it doesn't have audio" chart.Header.Title)
        elif chart.Header.BackgroundFile = Missing then
            Logging.Info(sprintf "Rejecting %s because it doesn't have a background image" chart.Header.Title)
        else

        let audio_hash =
            match chart.Header.AudioFile with
            | Asset s -> s
            | _ -> (Cache.audio_path chart backbeat_cache).Value |> Cache.compute_hash

        let bg_hash =
            match chart.Header.BackgroundFile with
            | Asset s -> s
            | _ -> (Cache.background_path chart backbeat_cache).Value |> Cache.compute_hash

        let create_entry (song_id) =
            let last_note = chart.LastNote

            {
                SongId = song_id
                Creators = [ chart.Header.Creator ]
                Keys = chart.Keys
                DifficultyName = chart.Header.DiffName
                Subtitle = chart.Header.Subtitle
                Tags = chart.Header.Tags
                Duration = last_note - chart.FirstNote
                Notecount =
                    let mutable count = 0

                    for row in chart.Notes do
                        for k = 0 to chart.Keys - 1 do
                            if row.Data.[k] = NoteType.NORMAL || row.Data.[k] = NoteType.HOLDHEAD then
                                count <- count + 1

                    count
                BPM = Chart.find_min_max_bpm chart
                Sources =
                    match chart.Header.ChartSource with
                    | Origin.Osu(-1, 0) -> []
                    | Origin.Osu(set, id) -> [ Osu {| BeatmapSetId = set; BeatmapId = id |} ]
                    | Origin.Stepmania(id) -> [ Stepmania id ]
                    | Origin.Unknown -> []
                    @ extraSources
                LastUpdated = DateTime.UtcNow
                PreviewTime = chart.Header.PreviewTime
                BackgroundFile = bg_hash
                AudioFile = audio_hash
            }

        let hash = Chart.hash chart

        if charts.ContainsKey hash then
            let old_entry = charts.[hash]
            let new_entry = create_entry old_entry.SongId

            if new_entry.Sources.IsEmpty then
                Logging.Info(sprintf "Rejecting %s because it doesn't have a source" chart.Header.Title)
            else if new_entry.Notecount < 50 then
                Logging.Info(sprintf "Rejecting %s because it doesn't have enough notes" chart.Header.Title)
            else

                if
                    { new_entry with
                        LastUpdated = old_entry.LastUpdated
                    }
                    <> old_entry
                then
                    charts.[hash] <-
                        { new_entry with
                            Tags = List.distinct (new_entry.Tags @ old_entry.Tags)
                            Sources = List.distinct (new_entry.Sources @ old_entry.Sources)
                        }

                    Logging.Info(sprintf "^ Chart: %s" hash)

                upload_chart chart new_entry
        else

        let song: Song =
            {
                Artists = [ chart.Header.Artist ]
                OtherArtists = []
                Remixers = []
                Title = chart.Header.Title
                AlternativeTitles =
                    match chart.Header.TitleNative with
                    | Some x -> [ x ]
                    | None -> []
                Source = chart.Header.Source
                Tags = chart.Header.Tags
            }

        let song_id = add_song song

        // todo: record transliterations of artists as alternatives
        //match chart.Header.ArtistNative with Some a -> add_artist_alias (chart.Header.Artist, a) | None -> ()

        let chart_entry = create_entry song_id

        if chart_entry.Sources.IsEmpty then
            Logging.Info(sprintf "Rejecting %s because it doesn't have a source" chart.Header.Title)
        else if chart_entry.Notecount < 50 then
            Logging.Info(sprintf "Rejecting %s because it doesn't have enough notes" chart.Header.Title)
        else

            charts.Add(hash, chart_entry)
            Logging.Info(sprintf "+ Chart: %s" hash)

            upload_chart chart chart_entry

    let slurp_folder (extra_sources: ChartSource list) (folder: string) =
        Directory.EnumerateFiles(Path.Combine(backbeat_cache.RootPath, folder))
        |> Seq.map Chart.from_file
        |> Seq.iter (Option.iter (slurp_chart extra_sources))

        save ()

    let cache_song_folder (config: ConversionOptions) (target: string) =
        Directory.EnumerateFiles target
        |> Seq.collect (
            function
            | ChartFile _ as file ->
                try
                    let action = { Config = config; Source = file }
                    convert_chart_file action
                with err ->
                    Logging.Error("Failed to convert/cache file: " + file, err)
                    []
            | _ -> []
        )
        |> List.ofSeq
        |> fun charts -> Cache.add_new config.PackName charts backbeat_cache

    let cache_pack_folder (sm_pack_id: int) (target: string) =
        let pack_name = sm_pack_id.ToString()

        for song_folder in Directory.EnumerateDirectories target do
            cache_song_folder
                { ConversionOptions.Default with
                    StepmaniaPackId = Some sm_pack_id
                    MoveAssets = false
                    PackName = pack_name
                }
                song_folder

        Cache.save backbeat_cache
        slurp_folder [] pack_name

    let cache_folder_of_oszs (community_pack_id: int) (target: string) =
        let pack_name = "c-" + community_pack_id.ToString()

        for osz in
            Directory.EnumerateFiles target
            |> Seq.filter (fun f -> Path.GetExtension(f).ToLower() = ".osz") do
            let extracted_path = Path.GetFileNameWithoutExtension osz

            if not (Directory.Exists extracted_path) then
                ZipFile.ExtractToDirectory(osz, Path.GetFileNameWithoutExtension osz)

            cache_song_folder
                { ConversionOptions.Default with
                    MoveAssets = false
                    PackName = pack_name
                }
                extracted_path

        Cache.save backbeat_cache
        slurp_folder [ ChartSource.CommunityPack {| PackId = community_pack_id |} ] pack_name

    let slurp_osu () = slurp_folder [] "osu!"

    let rec try_download_mirrors (mirrors, filepath) : Async<bool> =
        async {
            match mirrors with
            | [] -> return false
            | url :: xs ->
                if not (File.Exists filepath) then
                    Logging.Info(sprintf "Downloading from %s ..." url)
                    let! result = Prelude.Data.WebServices.download_file.RequestAsync(url, filepath, ignore)

                    if result then
                        return true
                    else
                        Logging.Error(sprintf "Download from %s failed." url)
                        return! try_download_mirrors (xs, filepath)
                else
                    Logging.Info(sprintf "Download from %s already on disk ..." url)
                    return true
        }

    let download_pack (id: StepmaniaPackId) =
        async {
            let mirrors = packs.Stepmania.[id].Mirrors |> List.map Archive.DownloadUrl.unpickle
            let zip = Path.Combine(ARCHIVE_PATH, "tmp", string id + ".zip")
            let folder = Path.Combine(ARCHIVE_PATH, "tmp", string id)

            match! try_download_mirrors (mirrors, zip) with
            | true ->
                if Directory.Exists folder then
                    Logging.Info(sprintf "Looks like this pack was already extracted to %s" folder)
                else
                    ZipFile.ExtractToDirectory(zip, folder)

                let pack_folder = (Directory.GetDirectories folder)[0]
                cache_pack_folder id pack_folder
                return true
            | false ->
                Logging.Error "All mirrors failed."
                return false
        }

    let download_community_pack (id: CommunityPackId) =
        async {
            let mirrors = packs.Community.[id].Mirrors |> List.map Archive.DownloadUrl.unpickle
            let zip = Path.Combine(ARCHIVE_PATH, "tmp", "c-" + string id + ".zip")
            let folder = Path.Combine(ARCHIVE_PATH, "tmp", "c-" + string id)

            match! try_download_mirrors (mirrors, zip) with
            | true ->
                if Directory.Exists folder then
                    Logging.Info(sprintf "Looks like this pack was already extracted to %s" folder)
                else
                    ZipFile.ExtractToDirectory(zip, folder)

                for f in Directory.GetDirectories folder do
                    cache_folder_of_oszs id f

                return true
            | false ->
                Logging.Error "All mirrors failed."
                return false
        }

    let download_osu_set (set_id: int) =
        async {
            let zip = Path.Combine(ARCHIVE_PATH, "tmp", "osu-" + string set_id + ".osz")
            let folder = Path.Combine(ARCHIVE_PATH, "tmp", "osu-" + string set_id)

            match! try_download_mirrors ([ sprintf "https://api.chimu.moe/v1/download/%i?n=1" set_id ], zip) with
            | true ->
                if Directory.Exists folder then
                    Logging.Info(sprintf "Looks like this pack was already extracted to %s" folder)
                else
                    ZipFile.ExtractToDirectory(zip, folder)

                cache_song_folder
                    { ConversionOptions.Default with
                        MoveAssets = false
                        PackName = "osu!"
                    }
                    folder

                Cache.save backbeat_cache
                return true
            | false ->
                Logging.Error "Download failed."
                return false
        }

    let slurp_sm (name: string) =
        let name = name.ToLower().Trim()

        match packs.Stepmania |> Seq.tryFind (fun x -> x.Value.Title.ToLower() = name) with
        | Some kv -> kv.Key |> download_pack |> Async.RunSynchronously |> ignore
        | None -> Logging.Error "Not found"

    let slurp_community (id: int) =
        if packs.Community.ContainsKey id then
            download_community_pack id |> Async.RunSynchronously |> ignore
        else
            Logging.Error "Not found"

    let search_sm (name: string) =
        let name = name.ToLower().Trim()

        for kvp in packs.Stepmania do
            if kvp.Value.Title.ToLower().Contains(name) then
                printfn "%s" kvp.Value.Title
