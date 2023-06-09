namespace Interlude.Web.Server.Bot

open System.Collections.Generic
open Percyqaz.Common
open Prelude.Data
open Prelude.Data.Charts.Archive

module Charts =

    let mutable songs = Songs()

    let mutable charts = Charts()

    let mutable packs = { Stepmania = new Dictionary<StepmaniaPackId, StepmaniaPack>(); Community = new Dictionary<CommunityPackId, CommunityPack>() }

    let init() =
        WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Backbeat/main/archive/songs.json", 
            function Some data -> songs <- data | None -> Logging.Error("Failed to get song data from Backbeat repo"))
        WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Backbeat/main/archive/charts.json", 
            function Some data -> charts <- data | None -> Logging.Error("Failed to get chart data from Backbeat repo"))
        WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Backbeat/main/archive/packs.json", 
            function Some data -> packs <- data; Logging.Info(sprintf "Backbeat downloads complete, %i Charts and %i Songs" charts.Count songs.Count) | None -> Logging.Error("Failed to get pack data from Backbeat repo"))

    let search_for_charts(query: string) =
        let query = query.ToLower()
        seq {
            for chart_hash in charts.Keys do
                let chart = charts.[chart_hash]
                let song = songs.[chart.SongId]
                if song.FormattedTitle.ToLower().Contains(query) then
                    yield (chart, song)
        } 
        |> Seq.groupBy snd
        |> Seq.truncate 30
        |> Seq.map (fun (song, stuff) -> song, Seq.map fst stuff |> List.ofSeq)

    let format_source (source: ChartSource) =
        match source with
        | Osu data -> sprintf "[osu! Beatmap](https://osu.ppy.sh/beatmapsets/%i#mania/%i)" data.BeatmapSetId data.BeatmapId
        | Stepmania id -> sprintf "[%s](%s)" packs.Stepmania.[id].Title (DownloadUrl.unpickle packs.Stepmania.[id].Mirrors.Head)
        | CommunityPack data -> sprintf "[%s](%s)" packs.Community.[data.PackId].Title (DownloadUrl.unpickle packs.Community.[data.PackId].Mirrors.Head)

