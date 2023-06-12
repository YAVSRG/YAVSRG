namespace Interlude.Web.Server.Bot

open System.Linq
open System.Collections.Generic
open FParsec
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

    type QueryFragment =
        | Artist of string
        | Title of string
        | Charter of string
        | Any of string

    let parse_query =
        let artist = spaces >>. (pstring "artist:" <|> pstring "a:")
        let title = spaces >>. (pstring "title:" <|> pstring "t:")
        let charter = spaces >>. (pstring "charter:" <|> pstring "creator:" <|> pstring "mapper:" <|> pstring "m:" <|> pstring "c:")
        let words = manyCharsTill anyChar (followedBy (artist <|> title <|> charter) <|> eof)

        let parser = 
            words .>>. many ((artist >>. words |>> Artist) <|> (title >>. words |>> Title) <|> (charter >>. words |>> Charter))
            |>> fun (a: string, xs: QueryFragment list) -> if a <> "" then Any a :: xs else xs

        fun query ->
            match run parser query with
            | Success(res, _, _) -> res
            | Failure(reason, _, _) -> failwithf "It should be impossible for this parser to fail, but it did: %s" reason

    let search (query: string) =
        let query = query.ToLower() |> parse_query
        seq {
            for chart_hash in charts.Keys do
            let chart = charts.[chart_hash]
            let song = songs.[chart.SongId]
            
            let mutable meets_search = true
            for criteria in query do
                meets_search <- 
                    meets_search &&
                    match criteria with
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
                    | Charter s ->
                        chart.Creators.Any(fun x -> x.ToLower().Contains(s))

            if meets_search then yield (chart, song)
        } 
        |> Seq.groupBy snd
        |> Seq.truncate 30
        |> Seq.map (fun (song, stuff) -> song, Seq.map fst stuff |> List.ofSeq)

    let format_source (source: ChartSource) =
        match source with
        | Osu data -> sprintf "[osu! Beatmap](https://osu.ppy.sh/beatmapsets/%i#mania/%i)" data.BeatmapSetId data.BeatmapId
        | Stepmania id -> sprintf "[%s](%s)" packs.Stepmania.[id].Title (DownloadUrl.unpickle packs.Stepmania.[id].Mirrors.Head)
        | CommunityPack data -> sprintf "[%s](%s)" packs.Community.[data.PackId].Title (DownloadUrl.unpickle packs.Community.[data.PackId].Mirrors.Head)