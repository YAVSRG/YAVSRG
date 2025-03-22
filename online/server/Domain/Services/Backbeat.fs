namespace Interlude.Web.Server.Domain.Services

open System.Collections.Generic
open Percyqaz.Common
open Prelude.Gameplay.Rulesets
open Prelude.Backbeat
open Prelude.Backbeat.Archive
open Interlude.Web.Server
open Interlude.Web.Server.Domain.Core

module Backbeat =

    let rulesets = Dictionary<string, Ruleset>()
    do
        let add_ruleset (ruleset: Ruleset) =
            rulesets.[Ruleset.hash ruleset] <- ruleset

        add_ruleset SC_J4
        add_ruleset (OsuMania.create 8.0f OsuMania.NoMod)
        add_ruleset (Wife3.create 4)

    module Tables =

        let CRESCENT: TableInfo =
            {
                Name = "Crescent"
                Description = "A variety of interesting 4K charts to improve your skills with!"
                Keymode = 4
                RulesetId = Score.PRIMARY_RULESET
                RatingCalculation = TableRatingCalculation.AverageTop50
                Sections =
                    [
                        {
                            Name = "ROOKIE"
                            Description = "Charts for complete beginners to get into the game!"
                            Color = 0xFF_43ef70

                            LevelStart = 0
                            LevelEnd = 9
                        }
                        {
                            Name = "ADVANCED"
                            Description = "Charts that should be interesting and challenging to an experienced player."
                            Color = 0xFF_43e0ef

                            LevelStart = 10
                            LevelEnd = 19
                        }
                        {
                            Name = "EXPERT"
                            Description =
                                "Fast, tiring, and technical charts for those who have played a serious amount of 4K."
                            Color = 0xFF_ef5d57

                            LevelStart = 20
                            LevelEnd = 29
                        }
                        {
                            Name = "XTREME"
                            Description =
                                "Extremely difficult charts to push even the most powerful keyboard smashers to their limit!"
                            Color = 0xFF_ff85c0

                            LevelStart = 30
                            LevelEnd = 39
                        }
                    ]
                LevelDisplayNames =
                    Map.ofSeq
                    <| seq {
                        for i = 0 to 9 do
                            yield i, sprintf "R-%02i" i

                        for i = 10 to 19 do
                            yield i, sprintf "A-%02i" i

                        for i = 20 to 29 do
                            yield i, sprintf "E-%02i" i

                        for i = 30 to 39 do
                            yield i, sprintf "X-%02i" i
                    }
            }

        // table ids must be lowercase
        let TABLES: Map<string, TableInfo> = Map.ofList [ "crescent", CRESCENT ]

        let exists (table_id: string) = TABLES.ContainsKey table_id
        let get (table_id: string) = TABLES.TryFind table_id

    //// chart search by text
    module Charts =

    //    type private QueryFragment =
    //        | Artist of string
    //        | Title of string
    //        | Charter of string
    //        | Any of string

    //    let private parse_query =
    //        let artist = (pstring "artist:" <|> pstring "a:")
    //        let title = (pstring "title:" <|> pstring "t:")

    //        let charter =
    //            (pstring "charter:"
    //             <|> pstring "creator:"
    //             <|> pstring "mapper:"
    //             <|> pstring "m:"
    //             <|> pstring "c:")

    //        let words =
    //            manyCharsTill anyChar (followedBy (artist <|> title <|> charter) <|> eof)
    //            |>> fun s -> s.Trim()

    //        let parser =
    //            words
    //            .>>. many (
    //                (artist >>. words |>> Artist)
    //                <|> (title >>. words |>> Title)
    //                <|> (charter >>. words |>> Charter)
    //            )
    //            |>> fun (a: string, xs: QueryFragment list) -> if a <> "" then Any a :: xs else xs

    //        fun query ->
    //            match run parser query with
    //            | Success(res, _, _) -> res
    //            | Failure(reason, _, _) ->
    //                failwithf "It should be impossible for this parser to fail, but it did: %s" reason

    //    let search (query: string) =
    //        let query = query.ToLower() |> parse_query

    //        seq {
    //            for chart_hash in charts.Keys do
    //                let chart = charts.[chart_hash]
    //                let song = songs.[chart.SongId]

    //                let mutable meets_search = true

    //                for criteria in query do
    //                    meets_search <-
    //                        meets_search
    //                        && match criteria with
    //                           | Any s ->
    //                               song.Title.ToLower().Contains(s)
    //                               || song.AlternativeTitles.Any(fun x -> x.ToLower().Contains(s))
    //                               || song.Artists.Any(fun x -> x.ToLower().Contains(s))
    //                               || song.OtherArtists.Any(fun x -> x.ToLower().Contains(s))
    //                               || song.Remixers.Any(fun x -> x.ToLower().Contains(s))
    //                               || chart.Creators.Any(fun x -> x.ToLower().Contains(s))
    //                           | Title s ->
    //                               song.Title.ToLower().Contains(s)
    //                               || song.AlternativeTitles.Any(fun x -> x.ToLower().Contains(s))
    //                           | Artist s ->
    //                               song.Artists.Any(fun x -> x.ToLower().Contains(s))
    //                               || song.OtherArtists.Any(fun x -> x.ToLower().Contains(s))
    //                               || song.Remixers.Any(fun x -> x.ToLower().Contains(s))
    //                           | Charter s -> chart.Creators.Any(fun x -> x.ToLower().Contains(s))

    //                if meets_search then
    //                    yield (chart, song)
    //        }
    //        |> Seq.groupBy snd
    //        |> Seq.truncate 30
    //        |> Seq.map (fun (song, stuff) -> song, Seq.map fst stuff |> List.ofSeq)

    //    // sources (for human page visiting and automatic downloading)

    //    let format_source (source: ChartSource) =
    //        match source with
    //        | Osu data ->
    //            sprintf "[osu! Beatmap](https://osu.ppy.sh/beatmapsets/%i#mania/%i)" data.BeatmapSetId data.BeatmapId
    //        | Stepmania id -> sprintf "[%s](https://etternaonline.com/pack/%i)" packs.Stepmania.[id].Title id
    //        | CommunityPack data ->
    //            sprintf
    //                "[%s](%s)"
    //                packs.Community.[data.PackId].Title
    //                (Archive.DownloadUrl.unpickle packs.Community.[data.PackId].Mirrors.Head)

    //    let mirrors (sources: ChartSource list) =
    //        seq {
    //            for source in sources do
    //                match source with
    //                | Osu data -> yield sprintf "https://api.chimu.moe/v1/download/%i?n=1" data.BeatmapSetId
    //                | Stepmania id -> yield Archive.DownloadUrl.unpickle packs.Stepmania.[id].Mirrors.Head
    //                | CommunityPack data ->
    //                    yield! Seq.map Archive.DownloadUrl.unpickle packs.Community.[data.PackId].Mirrors
    //        }

    //    // find a chart by hash

        let by_hash (hash: string) =
            match Domain.Backbeat.Songs.chart_and_song_by_id hash with
            | Some (_, chart, song) -> Some (chart, song)
            | None -> None

        open Prelude.Charts

        let fetch =
            let cache = Dictionary<string, Chart>()
            let http_client = new System.Net.Http.HttpClient()

            { new Async.Queue<string, Chart option>() with
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

                            if message.IsSuccessStatusCode then
                                use stream = message.Content.ReadAsStream()
                                use br = new System.IO.BinaryReader(stream)

                                match Chart.read_headless chart.Keys br with
                                | Ok chart ->
                                    cache.[hash] <- chart
                                    return Some chart
                                | Error reason ->
                                    Discord.debug_log (sprintf "CDN contains nonsense data for %s: %s" hash reason)
                                    return None
                            else return None
                    }
            }