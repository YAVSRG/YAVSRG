﻿namespace Interlude.Features.Printerlude

open System
open System.Threading
open System.IO
open Percyqaz.Common
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Prelude.Common
open Prelude.Charts
open Prelude.Data.Library.Caching
open Prelude.Data.``osu!``
open Prelude.Data
open Prelude.Gameplay
open Interlude
open Interlude.Content
open Interlude.Utils
open Interlude.Features
open Interlude.Web.Shared.Requests

module Printerlude =

    let mutable private ctx: ShellContext = Unchecked.defaultof<_>

    module private Themes =

        let register_commands (ctx: ShellContext) =
            ctx
                .WithCommand(
                    "themes_reload",
                    "Reload the current theme and noteskin",
                    fun () ->
                        Themes.reload_current ()
                        Noteskins.reload_current ()
                )
                .WithCommand(
                    "noteskin_stitch",
                    "Stitch a noteskin texture",
                    "id",
                    fun id -> Content.Noteskin.StitchTexture id
                )
                .WithCommand(
                    "noteskin_split",
                    "Split a noteskin texture",
                    "id",
                    fun id -> Content.Noteskin.SplitTexture id
                )

    module private Utils =

        let mutable cmp = None

        let cmp_1 () =
            match Gameplay.Chart.CHART with
            | None -> failwith "Select a chart"
            | Some c -> cmp <- Some c

        let cmp_2 () =
            match cmp with
            | None -> failwith "Use cmp_1 first"
            | Some cmp ->

            match Gameplay.Chart.CHART with
            | None -> failwith "Select a chart"
            | Some c -> Chart.diff cmp c

        let show_version (io: IOContext) =
            io.WriteLine(sprintf "You are running %s" Utils.version)
            io.WriteLine(sprintf "The latest version online is %s" AutoUpdate.latest_version_name)

        let timescale (io: IOContext) (v: float) =
            UI.Screen.timescale <- System.Math.Clamp(v, 0.01, 10.0)
            io.WriteLine(sprintf "Entering warp speed (%.0f%%)" (UI.Screen.timescale * 100.0))

        let export_osz () =
            match Gameplay.Chart.CHART with
            | None -> failwith "No chart loaded to export"
            | Some c ->
                try
                    create_osz c Content.Cache (get_game_folder "Exports")
                    Logging.Info "Exported."
                with err ->
                    Logging.Error("Error while exporting as osz", err)

        open SixLabors.ImageSharp

        let private banner (hex: string) (emoji: string) =
            use banner =
                Prelude.Data.ImageServices.generate_banner
                    {
                        BaseColor = Color.FromHex hex
                        Emoji = emoji.ToLower()
                    }

            banner.SaveAsPng("banner.png")

        let private sync_table_scores () =
            match Content.Table with
            | None -> ()
            | Some table ->

            Tables.Records.get (
                Interlude.Features.Online.Network.credentials.Username,
                table.Id,
                function
                | None -> ()
                | Some res ->
                    let lookup = res.Scores |> Seq.map (fun s -> s.Hash, s.Score) |> Map.ofSeq

                    for chart in table.Charts do
                        let data = ScoreDatabase.get chart.Hash Content.Scores

                        if
                            data.PersonalBests.ContainsKey(table.Info.RulesetId)
                            && data.PersonalBests.[table.Info.RulesetId].Accuracy
                               |> PersonalBests.get_best_above 1.0f
                               |> Option.defaultValue 0.0
                               |> fun acc -> acc > (Map.tryFind chart.Hash lookup |> Option.defaultValue 0.0)
                        then
                            for score in data.Scores do
                                Charts.Scores.Save.post (
                                    ({
                                        ChartId = chart.Hash
                                        Replay = score.Replay |> Replay.compressed_bytes_to_string
                                        Rate = score.Rate
                                        Mods = score.Mods
                                        Timestamp = Timestamp.to_datetime score.Timestamp
                                    }
                                    : Charts.Scores.Save.Request),
                                    ignore
                                )
            )

        let private personal_best_fixer =
            { new Async.Service<string, unit>() with
                override this.Handle(ruleset_id) =
                    async {
                        match Rulesets.by_hash ruleset_id with
                        | None -> ()
                        | Some ruleset ->

                        for hash in Content.Cache.Entries.Keys |> Seq.toArray do
                            let data = ScoreDatabase.get hash Content.Scores

                            if not data.Scores.IsEmpty then

                                match Cache.by_hash hash Content.Cache with
                                | None -> ()
                                | Some cc ->

                                match Cache.load cc Content.Cache with
                                | None -> ()
                                | Some chart ->

                                let existing_bests = data.PersonalBests
                                let mutable new_bests = existing_bests

                                for score in data.Scores do
                                    let score_info = ScoreInfo.from_score cc chart ruleset score

                                    if data.PersonalBests.ContainsKey ruleset_id then
                                        let ruleset_bests, _ = Bests.update score_info data.PersonalBests.[ruleset_id]
                                        new_bests <- Map.add ruleset_id ruleset_bests new_bests
                                    else
                                        new_bests <- Map.add ruleset_id (Bests.create score_info) new_bests

                                if new_bests <> existing_bests then
                                    data.PersonalBests <- new_bests

                        Logging.Info(sprintf "Finished processing personal bests for %s" ruleset.Name)
                    }
            }

        let fix_personal_bests () =
            personal_best_fixer.Request(Content.Rulesets.current_hash, ignore)
            Logging.Info("Queued a reprocess of personal bests")

        let register_commands (ctx: ShellContext) =
            ctx
                .WithCommand("exit", "Exits the game", (fun () -> UI.Screen.exit <- true))
                .WithCommand("clear", "Clears the terminal", Terminal.Log.clear)
                .WithCommand("export_osz", "Export current chart as osz", export_osz)
                .WithCommand("fix_personal_bests", "Fix personal best display values", fix_personal_bests)
                .WithCommand("sync_table_scores", "Sync local table scores with online server", sync_table_scores)
                .WithIOCommand(
                    "local_server",
                    "Switch to local development server",
                    "flag",
                    fun (io: IOContext) (b: bool) ->
                        Online.Network.credentials.Host <- (if b then "localhost" else "online.yavsrg.net")
                        Online.Network.credentials.Api <- (if b then "localhost" else "api.yavsrg.net")
                        AutoUpdate.restart_on_exit <- true
                        UI.Screen.exit <- true
                )
                .WithIOCommand("timescale", "Sets the timescale of all UI animations, for testing", "speed", timescale)
                .WithCommand("banner", "Generates a banner image (for testing)", "color", "emoji", banner)
                .WithCommand("cmp_1", "Select chart to compare against", cmp_1)
                .WithCommand("cmp_2", "Compare current chart to selected chart", cmp_2)

        let register_ipc_commands (ctx: ShellContext) =
            ctx.WithIOCommand("version", "Shows info about the current game version", show_version)

    let private ms = new MemoryStream()
    let private context_output = new StreamReader(ms)
    let private context_writer = new StreamWriter(ms)

    let io = { In = stdin; Out = context_writer }

    ctx <-
        ShellContext.Empty
        |> Utils.register_ipc_commands
        |> Utils.register_commands
        |> Themes.register_commands

    let exec (s: string) =
        let current_stream_position = ms.Position
        ctx.Evaluate io s
        context_writer.Flush()
        ms.Position <- current_stream_position
        Terminal.add_message (context_output.ReadToEnd())

    let mutable logging_disposable: IDisposable option = None
    let mutable ipc_shutdown_token: CancellationTokenSource option = None

    let ipc_commands = ShellContext.Empty |> Utils.register_ipc_commands

    let init_window (instance: int) =
        Terminal.exec_command <- exec

        logging_disposable <-
            Some
            <| Logging.Subscribe(fun (level, main, details) -> sprintf "[%A] %s" level main |> Terminal.add_message)

        Terminal.add_message @"================================================"
        Terminal.add_message @"=   ___      _      __          __        __   ="
        Terminal.add_message @"=  / _ \____(_)__  / /____ ____/ /_ _____/ /__ ="
        Terminal.add_message @"= / ___/ __/ / _ \/ __/ -_) __/ / // / _  / -_)="
        Terminal.add_message @"=/_/  /_/ /_/_//_/\__/\__/_/ /_/\_,_/\_,_/\__/ ="
        Terminal.add_message @"================================================"

        if instance = 0 then
            ipc_shutdown_token <- Some(IPC.start_server_thread "Interlude" ipc_commands)

    let deinit () =
        logging_disposable |> Option.iter (fun d -> d.Dispose())
        ipc_shutdown_token |> Option.iter (fun token -> token.Cancel())
