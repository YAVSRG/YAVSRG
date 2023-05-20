namespace Backbeat.Features.Archive

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Data.Charts.Archive
open Backbeat.Utils

[<AutoOpen>]
module Storage =
    
    let artists : VerifiedArtists = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "artists.json")) with
        | Ok d -> d
        | Error e -> 
            Logging.Warn(sprintf "Error loading artists.json: %s" e.Message)
            { Artists = Dictionary<string, VerifiedArtist>() }
    
    let songs : Songs = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "songs.json")) with
        | Ok d -> d
        | Error e -> 
            Logging.Warn(sprintf "Error loading songs.json: %s" e.Message)
            Dictionary<SongId, Song>()
    
    let charts : Charts = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "charts.json")) with
        | Ok d -> d
        | Error e -> 
            Logging.Warn(sprintf "Error loading charts.json: %s" e.Message)
            Dictionary<ChartHash, Chart>()
    
    let packs : Packs = 
        match JSON.FromFile (Path.Combine(ARCHIVE_PATH, "packs.json")) with
        | Ok d -> d
        | Error e -> 
            Logging.Warn(sprintf "Error loading packs.json: %s" e.Message)
            {
                Stepmania = Dictionary<StepmaniaPackId, StepmaniaPack>()
                Community = Dictionary<CommunityPackId, CommunityPack>()
            }

    let save() =
        try JSON.ToFile (Path.Combine(ARCHIVE_PATH, "artists.json"), true) artists
        with e -> Logging.Error("Artists not saved", e)
        try JSON.ToFile (Path.Combine(ARCHIVE_PATH, "songs.json"), true) songs
        with e -> Logging.Error("Songs not saved", e)
        try JSON.ToFile (Path.Combine(ARCHIVE_PATH, "charts.json"), true) charts
        with e -> Logging.Error("Charts not saved", e)
        try JSON.ToFile (Path.Combine(ARCHIVE_PATH, "packs.json"), true) packs 
        with e -> Logging.Error("Packs not saved", e)
        Logging.Debug("Saved database")

    module Queue =
        let get id =
            try
                File.ReadAllLines (Path.Combine(ARCHIVE_PATH, "queue", id + ".txt"))
                |> List.ofArray
            with :? FileNotFoundException -> []

        let save id xs =
            File.WriteAllLines (Path.Combine(ARCHIVE_PATH, "queue", id + ".txt"), Array.ofList xs)

        let append id x =
            File.AppendAllText (Path.Combine(ARCHIVE_PATH, "queue", id + ".txt"), x + "\n")