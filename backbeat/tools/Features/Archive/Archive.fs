namespace Backbeat.Features

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Json
open Prelude
open Prelude.Charts.Formats.Conversions
open Backbeat.Utils

module Archive =

    type ArtistName = string
    [<Json.AutoCodec>]
    type Artist =
        {
            mutable Alternatives: string list // first element is primary name
        }
        member this.Add(alias: string) =
            this.Alternatives <- this.Alternatives @ [alias] |> List.distinct
    
    type SongId = string
    [<Json.AutoCodec>]
    type Song =
        {
            Artists: ArtistName list // never empty
            OtherArtists: ArtistName list
            Remixers: ArtistName list
            Title: string
            AlternativeTitles: string list
            Source: string option
            Tags: string list
        }

    type StepmaniaPackId = int
    [<Json.AutoCodec>]
    type StepmaniaPack =
        {
            Title: string
            Mirrors: string list
            Size: int64
        }
    type CommunityPackId = int
    [<Json.AutoCodec>]
    type CommunityPack =
        {
            Title: string
            Description: string option
            Mirrors: string list
            Size: int64
        }

    [<Json.AutoCodec>]
    type Packs =
        { 
            Stepmania: Dictionary<StepmaniaPackId, StepmaniaPack>
            Community: Dictionary<CommunityPackId, CommunityPack>
        }
    
    [<Json.AutoCodec>]
    type ChartSource =
        | Osu of {| BeatmapId: int; BeatmapSetId: int |}
        | Stepmania of StepmaniaPackId
        | CommunityPack of {| PackId: CommunityPackId |}

    type ChartHash = string
    
    [<Json.AutoCodec>]
    type Chart =
        {
            SongId: SongId
            Creators: string list // never empty
            Keys: int
            DifficultyName: string
            Subtitle: string option
            Tags: string list
            Duration: Time
            Notecount: int
            BPM: (float32<ms/beat> * float32<ms/beat>)
            Sources: ChartSource list
            LastUpdated: DateTime
        }

    let artists = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "artists.json")) with
        | Ok d -> d
        | Error e -> 
            printfn "Error loading artists.json: %s" e.Message;
            Dictionary<ArtistName, Artist>()
    
    let songs = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "songs.json")) with
        | Ok d -> d
        | Error e -> 
            printfn "Error loading songs.json: %s" e.Message;
            Dictionary<SongId, Song>()
    
    let charts = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "charts.json")) with
        | Ok d -> d
        | Error e -> 
            printfn "Error loading charts.json: %s" e.Message;
            Dictionary<ChartHash, Chart>()
    
    let packs = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "packs.json")) with
        | Ok d -> d
        | Error e -> 
            printfn "Error loading packs.json: %s" e.Message
            {
                Stepmania = Dictionary<StepmaniaPackId, StepmaniaPack>()
                Community = Dictionary<CommunityPackId, CommunityPack>()
            }

    let save() =
        JSON.ToFile (Path.Combine(ARCHIVE_PATH, "artists.json"), true) artists
        JSON.ToFile (Path.Combine(ARCHIVE_PATH, "songs.json"), true) songs
        JSON.ToFile (Path.Combine(ARCHIVE_PATH, "charts.json"), true) charts
        JSON.ToFile (Path.Combine(ARCHIVE_PATH, "packs.json"), true) packs

    [<Json.AutoCodec>]
    type EOPackAttrs =
        {
            name: string
            average: float
            download: string
            mirror: string
            size: int64
        }
    
    [<Json.AutoCodec>]
    type EOPack =
        {
            ``type``: string
            id: int
            attributes: EOPackAttrs
        }

    let into_base64 (str: string) =
        str
        |> Text.Encoding.UTF8.GetBytes
        |> Convert.ToBase64String

    let from_base64 (str: string) =
        str
        |> Convert.FromBase64String
        |> Text.Encoding.UTF8.GetString

    let archive_download_link(str: string) =
        str.Replace("https://", "").Replace("http://", "") |> into_base64

    let load_stepmania_packs() =
        async {
            match! Prelude.Data.WebServices.download_json_async("https://api.etternaonline.com/v2/packs/") with
            | None -> printfn "Failed to get EO packs"
            | Some (d: {| data: ResizeArray<EOPack> |}) ->
                for p in d.data do
                    if packs.Stepmania.ContainsKey(p.id) then packs.Stepmania.Remove(p.id) |> ignore
                    packs.Stepmania.Add(p.id,
                            {
                                Title = p.attributes.name
                                Mirrors = [archive_download_link p.attributes.download; archive_download_link p.attributes.mirror] |> List.distinct
                                Size = p.attributes.size
                            })
        }
        |> Async.RunSynchronously
        save()

    module Queue =
        let get id =
            try
                File.ReadAllLines (Path.Combine(ARCHIVE_PATH, "queue", id+".txt"))
                |> List.ofArray
            with :? FileNotFoundException -> []

        let save id xs =
            File.WriteAllLines (Path.Combine(ARCHIVE_PATH, "queue", id+".txt"), Array.ofList xs)

        let append id x =
            File.AppendAllText (Path.Combine(ARCHIVE_PATH, "queue", id+".txt"), x + "\n")

    let simplify_string =
        let regex = Text.RegularExpressions.Regex("[^\sa-zA-Z0-9_-]")
        fun (s: string) -> regex.Replace(s.ToLowerInvariant(), "").Trim().Replace(" ", "-")

    let simplify_string_10 =
        simplify_string >> (fun s -> s.Substring(0, min s.Length 10))

    let random_ident =
        let r = Random()
        fun () -> sprintf "#%4i" (r.Next(0, 10000))

    let add_artist(artist: string) =
        if artists.ContainsKey(artist) then ()
        else artists.Add(artist, { Alternatives = [artist] })

    let add_artist_alias(artist, alias) =
        if artists.ContainsKey(artist) then 
            artists.[artist].Add alias
        elif artists.ContainsKey(alias) then
            artists.[alias].Add artist
        else printfn "Can't add alias %s to %s because neither exists in artist list" alias artist

    let add_song(song: Song) : SongId =
        List.iter add_artist song.Artists
        List.iter add_artist song.OtherArtists
        List.iter add_artist song.Remixers

        let stitle = simplify_string song.Title
        let mutable existing_id = None
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

                if similarity >= 2 then
                    printfn "Incoming song %A may be similar to existing song %s" song other_song_id
                    // add to human oversight queue
                    // existing_id <- Some other_song_id

        match existing_id with
        | None ->
            let id = (simplify_string_10 (List.head song.Artists)) + (simplify_string_10 song.Title) + random_ident()
            songs.Add(id, song)
            printfn "+ Song: %s" id
            id
        | Some id -> id

    let slurp_chart(chart: Prelude.Charts.Formats.Interlude.Chart) =
        if not (File.Exists chart.AudioPath) then
            printfn "Rejecting %s because it doesn't have audio" chart.Header.Title
        elif not (File.Exists chart.BackgroundPath) then
            printfn "Rejecting %s because it doesn't have a background image" chart.Header.Title
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
        let id = add_song song
        match chart.Header.ArtistNative with Some a -> add_artist_alias (chart.Header.Artist, a) | None -> ()
        
        let chartEntry: Chart = 
            {
                SongId = id
                Creators = [chart.Header.Creator]
                Keys = chart.Keys
                DifficultyName = chart.Header.DiffName
                Subtitle = chart.Header.Subtitle
                Tags = chart.Header.Tags
                Duration = chart.LastNote - chart.FirstNote
                Notecount = -1 //todo
                BPM = (0.0f<ms/beat>, 0.0f<ms/beat>) //todo
                Sources = 
                    match chart.Header.ChartSource with
                    | Prelude.Charts.Formats.Interlude.ChartSource.Osu (set, id) -> [Osu {| BeatmapSetId = set; BeatmapId = id |}]
                    | Prelude.Charts.Formats.Interlude.ChartSource.Stepmania (id) -> [Stepmania id]
                    | Prelude.Charts.Formats.Interlude.ChartSource.Unknown -> []
                LastUpdated = DateTime.Now
            }
        let hash = Prelude.Charts.Formats.Interlude.Chart.hash chart
        charts.Add(hash, chartEntry)
        printfn "+ Chart: %s" hash

    let slurp_song_folder(target: string) =
        for file in Directory.EnumerateFiles target do
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
                    |> List.iter slurp_chart
                with err -> printfn "Failed to load/convert file: %s" target
            | _ -> ()

    let slurp_pack_folder(target: string) =
        for songFolder in Directory.EnumerateDirectories target do slurp_song_folder songFolder

    let rec try_download_mirrors(mirrors, filepath) : Async<bool> =
        async {
            match mirrors with
            | [] -> return false
            | url :: xs ->
                if File.Exists filepath then File.Delete filepath
                printfn "Downloading from %s ..." url
                let! result = Prelude.Data.WebServices.download_file.RequestAsync(url, filepath, ignore)
                if result then return true 
                else 
                    printfn "Download from %s failed." url
                    return! try_download_mirrors (xs, filepath)
        }

    open System.IO.Compression

    let download_pack(id: StepmaniaPackId) =
        async {
            let mirrors = packs.Stepmania.[id].Mirrors |> List.map from_base64 |> List.map (fun s -> "http://" + s)
            let zip = Path.Combine(ARCHIVE_PATH, "tmp", string id + ".zip")
            let folder = Path.Combine(ARCHIVE_PATH, "tmp", string id)
            match! try_download_mirrors(mirrors, zip) with
            | true ->
                ZipFile.ExtractToDirectory(zip, folder)
                let pack_folder = (Directory.GetDirectories folder)[0]
                slurp_pack_folder pack_folder
                return true
            | false ->
                printfn "All mirrors failed."
                return false
        }

    let slurp() =
        let mutable packs = Queue.get "pack-imports"
        while packs <> [] do
            if download_pack(int (List.head packs)) |> Async.RunSynchronously then save()
            else Queue.append "failed-packs" (List.head packs)
            packs <- List.tail packs
            Queue.save "pack-imports" packs
        
    open Percyqaz.Shell

    let register (ctx: Context) =
        ctx
            .WithCommand("get_eo", 
            Command.create "Archives EO packs locally" [] <| Impl.Create(load_stepmania_packs))
            .WithCommand("slurp", 
            Command.create "Processes queues of chart data to add to database" [] <| Impl.Create(slurp))

