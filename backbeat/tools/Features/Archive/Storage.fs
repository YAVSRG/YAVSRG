﻿namespace Backbeat.Features.Archive

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Data.Charts.Caching
open Prelude.Backbeat.Archive
open Backbeat.Utils

[<AutoOpen>]
module Storage =

    let backbeat_cache = Cache.from_path BACKBEAT_CACHE_FOLDER

    let artists: VerifiedArtists =
        match JSON.FromFile(Path.Combine(ARCHIVE_PATH, "artists.json")) with
        | Ok d -> d
        | Error e ->
            Logging.Warn(sprintf "Error loading artists.json: %s" e.Message)

            {
                Artists = Dictionary<string, VerifiedArtist>()
                Aliases = Dictionary<string, string list>()
            }

    let songs: Songs =
        match JSON.FromFile(Path.Combine(ARCHIVE_PATH, "songs.json")) with
        | Ok d -> d
        | Error e ->
            Logging.Warn(sprintf "Error loading songs.json: %s" e.Message)
            Dictionary<SongId, Song>()

    let charts: Charts =
        match JSON.FromFile(Path.Combine(ARCHIVE_PATH, "charts.json")) with
        | Ok d -> d
        | Error e ->
            Logging.Warn(sprintf "Error loading charts.json: %s" e.Message)
            Dictionary<ChartHash, Chart>()

    let packs: Packs =
        match JSON.FromFile(Path.Combine(ARCHIVE_PATH, "packs.json")) with
        | Ok d -> d
        | Error e ->
            Logging.Warn(sprintf "Error loading packs.json: %s" e.Message)

            {
                Stepmania = Dictionary<StepmaniaPackId, StepmaniaPack>()
                Community = Dictionary<CommunityPackId, CommunityPack>()
            }

    let save () =
        (artists, songs, charts, packs) |> ignore

        let del p =
            if File.Exists p then
                File.Delete p

        let mov p =
            if File.Exists p then
                File.Move(p, p + ".old")

        try
            del (Path.Combine(ARCHIVE_PATH, "artists.json.old"))
            del (Path.Combine(ARCHIVE_PATH, "songs.json.old"))
            del (Path.Combine(ARCHIVE_PATH, "charts.json.old"))
            del (Path.Combine(ARCHIVE_PATH, "packs.json.old"))
            mov (Path.Combine(ARCHIVE_PATH, "artists.json"))
            mov (Path.Combine(ARCHIVE_PATH, "songs.json"))
            mov (Path.Combine(ARCHIVE_PATH, "charts.json"))
            mov (Path.Combine(ARCHIVE_PATH, "packs.json"))
            JSON.ToFile (Path.Combine(ARCHIVE_PATH, "artists.json"), true) artists
            JSON.ToFile (Path.Combine(ARCHIVE_PATH, "songs.json"), true) songs
            JSON.ToFile (Path.Combine(ARCHIVE_PATH, "charts.json"), true) charts
            JSON.ToFile (Path.Combine(ARCHIVE_PATH, "packs.json"), true) packs
        with e ->
            Logging.Error("Something didnt save", e)

        Logging.Debug("Saved database")

    module Queue =
        let get id =
            try
                File.ReadAllLines(Path.Combine(ARCHIVE_PATH, "queue", id + ".txt"))
                |> List.ofArray
            with :? FileNotFoundException ->
                []

        let save id xs =
            File.WriteAllLines(Path.Combine(ARCHIVE_PATH, "queue", id + ".txt"), Array.ofList xs)

        let append id x =
            File.AppendAllText(Path.Combine(ARCHIVE_PATH, "queue", id + ".txt"), x + "\n")
