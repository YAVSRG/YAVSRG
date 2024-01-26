namespace Interlude.Web.Server.Domain.Services

open System.Linq
open System.Collections.Generic
open FParsec
open Percyqaz.Common
open Prelude.Data
open Prelude.Gameplay
open Prelude.Backbeat.Archive
open Prelude.Data.Charts.Tables

module Backbeat =

    let mutable tables: Map<string, Table> = Map.empty

    let rulesets = Dictionary<string, Ruleset>()

    let mutable songs = Songs()

    let mutable charts = Charts()

    let mutable packs =
        {
            Stepmania = new Dictionary<StepmaniaPackId, StepmaniaPack>()
            Community = new Dictionary<CommunityPackId, CommunityPack>()
        }

    // load seed data from Backbeat repo
    // todo: stop storing seed data. this server should be the source of truth
    let init () =
        WebServices.download_json (
            "https://raw.githubusercontent.com/YAVSRG/YAVSRG/main/backbeat/archive/songs.json",
            function
            | Some _songs ->
                WebServices.download_json (
                    "https://raw.githubusercontent.com/YAVSRG/YAVSRG/main/backbeat/archive/charts.json",
                    function
                    | Some _charts ->
                        WebServices.download_json (
                            "https://raw.githubusercontent.com/YAVSRG/YAVSRG/main/backbeat/archive/packs.json",
                            function
                            | Some _packs ->
                                packs <- _packs
                                charts <- _charts
                                songs <- _songs

                                Logging.Info(
                                    sprintf
                                        "Backbeat downloads complete, %i Charts and %i Songs"
                                        charts.Count
                                        songs.Count
                                )
                            | None -> Logging.Error("Failed to get pack data from Backbeat")
                        )
                    | None -> Logging.Error("Failed to get chart data from Backbeat")
                )
            | None -> Logging.Error("Failed to get song data from Backbeat")
        )

        WebServices.download_json (
            "https://raw.githubusercontent.com/YAVSRG/YAVSRG/main/backbeat/rulesets/rulesets.json",
            function
            | Some(archive: PrefabRulesets.Repo) ->
                for rs in archive.Rulesets.Values do
                    rulesets.[Ruleset.hash rs] <- rs
            | None -> Logging.Error("Failed to get ruleset data from Backbeat")
        )

        WebServices.download_json (
            "https://raw.githubusercontent.com/YAVSRG/YAVSRG/main/backbeat/tables/crescent.table",
            function
            | Some(t: Table) ->
                tables <- Map.ofList [ "crescent", t ]
                //TableWithSuggestions.update_if_newer ("crescent", t)
            | None -> Logging.Error("Failed to get Crescent from Backbeat")
        )

    // chart search by text
    module Charts =

        type private QueryFragment =
            | Artist of string
            | Title of string
            | Charter of string
            | Any of string

        let private parse_query =
            let artist = (pstring "artist:" <|> pstring "a:")
            let title = (pstring "title:" <|> pstring "t:")

            let charter =
                (pstring "charter:"
                 <|> pstring "creator:"
                 <|> pstring "mapper:"
                 <|> pstring "m:"
                 <|> pstring "c:")

            let words =
                manyCharsTill anyChar (followedBy (artist <|> title <|> charter) <|> eof)
                |>> fun s -> s.Trim()

            let parser =
                words
                .>>. many (
                    (artist >>. words |>> Artist)
                    <|> (title >>. words |>> Title)
                    <|> (charter >>. words |>> Charter)
                )
                |>> fun (a: string, xs: QueryFragment list) -> if a <> "" then Any a :: xs else xs

            fun query ->
                match run parser query with
                | Success(res, _, _) -> res
                | Failure(reason, _, _) ->
                    failwithf "It should be impossible for this parser to fail, but it did: %s" reason

        let search (query: string) =
            let query = query.ToLower() |> parse_query

            seq {
                for chart_hash in charts.Keys do
                    let chart = charts.[chart_hash]
                    let song = songs.[chart.SongId]

                    let mutable meets_search = true

                    for criteria in query do
                        meets_search <-
                            meets_search
                            && match criteria with
                               | Any s ->
                                   song.Title.ToLower().Contains(s)
                                   || song.AlternativeTitles.Any(fun x -> x.ToLower().Contains(s))
                                   || song.Artists.Any(fun x -> x.ToLower().Contains(s))
                                   || song.OtherArtists.Any(fun x -> x.ToLower().Contains(s))
                                   || song.Remixers.Any(fun x -> x.ToLower().Contains(s))
                                   || chart.Creators.Any(fun x -> x.ToLower().Contains(s))
                               | Title s ->
                                   song.Title.ToLower().Contains(s)
                                   || song.AlternativeTitles.Any(fun x -> x.ToLower().Contains(s))
                               | Artist s ->
                                   song.Artists.Any(fun x -> x.ToLower().Contains(s))
                                   || song.OtherArtists.Any(fun x -> x.ToLower().Contains(s))
                                   || song.Remixers.Any(fun x -> x.ToLower().Contains(s))
                               | Charter s -> chart.Creators.Any(fun x -> x.ToLower().Contains(s))

                    if meets_search then
                        yield (chart, song)
            }
            |> Seq.groupBy snd
            |> Seq.truncate 30
            |> Seq.map (fun (song, stuff) -> song, Seq.map fst stuff |> List.ofSeq)

        // sources (for human page visiting and automatic downloading)

        let format_source (source: ChartSource) =
            match source with
            | Osu data ->
                sprintf "[osu! Beatmap](https://osu.ppy.sh/beatmapsets/%i#mania/%i)" data.BeatmapSetId data.BeatmapId
            | Stepmania id -> sprintf "[%s](https://etternaonline.com/pack/%i)" packs.Stepmania.[id].Title id
            | CommunityPack data ->
                sprintf
                    "[%s](%s)"
                    packs.Community.[data.PackId].Title
                    (Archive.DownloadUrl.unpickle packs.Community.[data.PackId].Mirrors.Head)

        let mirrors (sources: ChartSource list) =
            seq {
                for source in sources do
                    match source with
                    | Osu data -> yield sprintf "https://api.chimu.moe/v1/download/%i?n=1" data.BeatmapSetId
                    | Stepmania id -> yield Archive.DownloadUrl.unpickle packs.Stepmania.[id].Mirrors.Head
                    | CommunityPack data ->
                        yield! Seq.map Archive.DownloadUrl.unpickle packs.Community.[data.PackId].Mirrors
            }

        // find a chart by hash

        let by_hash (hash: string) =
            if charts.ContainsKey hash then
                let c = charts.[hash]
                Some(c, songs.[c.SongId])
            else
                None

        open Prelude.Charts

        let fetch =
            let cache = Dictionary<string, Chart>()
            let http_client = new System.Net.Http.HttpClient()

            { new Async.Service<string, Chart option>() with
                override this.Handle(hash) =
                    async {
                        if cache.ContainsKey hash then
                            return Some cache.[hash]
                        else

                            match by_hash hash with
                            | None -> return None
                            | Some(chart, song) ->

                            let header = Archive.make_chart_header (chart, song)
                            let! message = http_client.GetAsync("https://cdn.yavsrg.net/" + hash) |> Async.AwaitTask
                            use stream = message.Content.ReadAsStream()
                            use br = new System.IO.BinaryReader(stream)

                            match Chart.read_headless chart.Keys header "" br with
                            | Some chart ->
                                cache.[hash] <- chart
                                return Some chart
                            | None -> return None
                    }
            }
