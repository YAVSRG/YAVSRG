namespace Interlude.UI

open System.Collections.Generic
open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Charts.Formats.Interlude
open Prelude.Data.Charts
open Prelude.Data.Scores
open Prelude.Data.Charts.Caching
open Interlude
open Interlude.Features
open Interlude.Features.Stats
open Interlude.Features.MainMenu
open Interlude.Features.Import
open Interlude.Features.Score
open Interlude.Features.Play
open Interlude.Features.LevelSelect
open Interlude.Features.Multiplayer
open Interlude.Features.Printerlude
open Interlude.Features.Toolbar

module private Migrations =
    
    let run_1 () =
        Logging.Info "---- ---- ----"
        Logging.Info "Hold it! Update 0.7.2 changes how chart hashing work - Please wait while your data gets migrated"
        Logging.Info "This may take a couple of minutes, do not close your game ..."

        // build mapping from cache
        let mapping = Dictionary<string, ResizeArray<string>>()
        for folder in Directory.EnumerateDirectories Library.cache.RootPath |> Seq.filter (fun p -> Path.GetFileName p <> ".assets") do
            for file in Directory.EnumerateFiles folder do
                match Path.GetExtension(file).ToLower() with
                | ".yav" ->
                    match Chart.fromFile file with
                    | Some c ->
                        match Chart.check c with
                        | Error _ ->

                            let fix = Chart.LegacyHash.fix c
                            match Chart.check fix with 
                            | Ok() ->
                                let old_hash = Chart.LegacyHash.hash c
                                let new_hash = Chart.hash fix
                                if not (mapping.ContainsKey new_hash) then mapping.[new_hash] <- ResizeArray<_>()
                                mapping.[new_hash].Add(old_hash)
                                let file_hash = Path.GetFileNameWithoutExtension(file)
                                if file_hash <> old_hash then mapping.[new_hash].Add(file_hash)
                            | Error _ -> ()
                            File.Delete file

                        | Ok() ->
                            let old_hash = Chart.LegacyHash.hash c
                            let new_hash = Chart.hash c
                            if not (mapping.ContainsKey new_hash) then mapping.[new_hash] <- ResizeArray<_>()
                            mapping.[new_hash].Add(old_hash)
                    | None -> ()
                | _ -> ()

        // migrate scores
        Logging.Info "Migrating your scores ..."
        File.Copy(Path.Combine (getDataPath "Data", "scores.json"), Path.Combine (getDataPath "Data", "scores-migration-backup.json"))
        let old_scores = Dictionary(Scores.data.Entries)
        Scores.data.Entries.Clear()

        let mutable migrated_scores = 0

        for new_hash in mapping.Keys do
            let old_hashes = mapping.[new_hash] |> List.ofSeq
            let old_datas = 
                old_hashes 
                |> List.choose (fun old_hash -> 
                    if old_scores.ContainsKey old_hash then
                        let r = old_scores.[old_hash]
                        old_scores.Remove(old_hash) |> ignore
                        Some r 
                    else None)
            match old_datas with
            | data :: ds ->
                for other_data in ds do
                    if data.Comment = "" then data.Comment <- other_data.Comment
                    data.Scores.AddRange(other_data.Scores)
                    data.LastPlayed <- max data.LastPlayed other_data.LastPlayed
                Scores.data.Entries.[new_hash] <- data
                migrated_scores <- migrated_scores + data.Scores.Count
            | [] -> ()

        let mutable unmigrated_scores = 0
        for d in old_scores.Values do
            unmigrated_scores <- unmigrated_scores + d.Scores.Count

        Logging.Info(sprintf "Migrated %i scores, %i have been left behind (probably because they are scores on charts you have deleted)" migrated_scores unmigrated_scores)
        
        Logging.Info "Migrating your collections ..."
        let reverse_mapping = Dictionary<string, string>()
        for new_hash in mapping.Keys do
            for old_hash in mapping.[new_hash] do reverse_mapping.[old_hash] <- new_hash
        for folder in Library.collections.Folders.Values do
            let charts = List(folder.Charts)
            folder.Charts.Clear()
            for c in charts do
                if reverse_mapping.ContainsKey(c.Hash) then 
                    folder.Charts.Add({ Path = c.Path.Replace(c.Hash, reverse_mapping.[c.Hash]); Hash = reverse_mapping.[c.Hash] })
        for pl in Library.collections.Playlists.Values do
            let charts = List(pl.Charts)
            pl.Charts.Clear()
            for c, info in charts do
                if reverse_mapping.ContainsKey(c.Hash) then 
                    pl.Charts.Add(({ Path = c.Path.Replace(c.Hash, reverse_mapping.[c.Hash]); Hash = reverse_mapping.[c.Hash] }, info))

        // now recache which corrects all the hashes
        
        Logging.Info "Running a recache ..."
        Cache.recache_service.RequestAsync Library.cache |> Async.RunSynchronously

    let run_2 () =
        Logging.Info "---- ---- ----"
        Logging.Info "Please wait while Interlude caches what patterns you have in your charts - it shouldn't take long"
        Library.cache_patterns.RequestAsync () |> Async.RunSynchronously
        

module Startup =
    let MIGRATION_VERSION = 2
    let migrate() =
        
        if Stats.total.MigrationVersion.IsNone then
            if Library.cache.Entries.Count > 0 then
                Stats.total.MigrationVersion <- Some 0
            else Stats.total.MigrationVersion <- Some MIGRATION_VERSION

        match Stats.total.MigrationVersion with
        | None -> failwith "impossible"
        | Some i ->
            if i < 1 then
                Migrations.run_1()
                Stats.total.MigrationVersion <- Some 1

            if i < 2 then
                Migrations.run_2()
                Stats.total.MigrationVersion <- Some 2

    let ui_entry_point() =
        Screen.init [|LoadingScreen(); MainMenuScreen(); ImportScreen(); LobbyScreen(); LevelSelectScreen()|]
        
        ScoreScreenHelpers.watchReplay <- fun (modchart, rate, data) -> Screen.changeNew (fun () -> ReplayScreen.replay_screen(ReplayMode.Replay (modchart, rate, data)) :> Screen.T) Screen.Type.Replay Transitions.Flags.Default
        Utils.AutoUpdate.checkForUpdates()
        Mounts.handleStartupImports()
        
        Logging.Subscribe
            ( fun (level, main, details) ->
                sprintf "[%A] %s" level main |> Terminal.add_message )
        |> ignore

        { new Screen.ScreenRoot(Toolbar())
            with override this.Init() = Gameplay.init(); migrate(); base.Init() }