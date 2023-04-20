namespace Backbeat.Features

open System
open System.IO
open System.Collections.Generic
open Percyqaz.Json
open Prelude.Common
open Backbeat.Utils

module Archive =

    type ArtistName = string
    [<Json.AutoCodec>]
    type Artist =
        {
            Alternatives: string list
        }
    
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
        | Stepmania of {| PackTitle: string; PackId: StepmaniaPackId |}
        | CommunityPack of {| PackId: CommunityPackId |}

    type ChartId = string
    
    [<Json.AutoCodec>]
    type Chart =
        {
            SongId: SongId
            Hash: string
            Creators: string list // never empty
            Keys: int
            DifficultyName: string
            Subtitle: string option
            Tags: string
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
            Dictionary<ChartId, Chart>()
    
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
        |> System.Text.Encoding.UTF8.GetBytes
        |> System.Convert.ToBase64String

    let from_base64 (str: string) =
        str
        |> System.Convert.FromBase64String
        |> System.Text.Encoding.UTF8.GetString

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
        
    open Percyqaz.Shell

    let register (ctx: Context) =
        ctx
            .WithCommand("get_eo", 
            Command.create "Archives EO packs locally" [] <| Impl.Create(load_stepmania_packs))

