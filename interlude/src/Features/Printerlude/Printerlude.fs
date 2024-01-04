﻿namespace Interlude.Features.Printerlude

open System.IO
open Percyqaz.Common
open Percyqaz.Shell
open Percyqaz.Shell.Shell
open Prelude.Common
open Prelude.Charts
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.``osu!``
open Interlude
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
                        Content.Themes.load ()
                        Content.Noteskins.load ()
                )
                .WithCommand(
                    "noteskin_stitch",
                    "Stitch a noteskin texture",
                    "id",
                    fun id -> Content.Noteskins.Current.instance.StitchTexture id
                )
                .WithCommand(
                    "noteskin_split",
                    "Split a noteskin texture",
                    "id",
                    fun id -> Content.Noteskins.Current.instance.SplitTexture id
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
            io.WriteLine(sprintf "The latest version online is %s" Utils.AutoUpdate.latest_version_name)

        let timescale (io: IOContext) (v: float) =
            UI.Screen.timescale <- System.Math.Clamp(v, 0.01, 10.0)
            io.WriteLine(sprintf "Entering warp speed (%.0f%%)" (UI.Screen.timescale * 100.0))

        let export_osz () =
            match Gameplay.Chart.CHART with
            | None -> failwith "No chart loaded to export"
            | Some c ->
                try
                    create_osz c Library.cache (get_game_folder "Exports")
                    Logging.Info "Exported."
                with err ->
                    Logging.Error("Error while exporting as osz", err)

        let private import_osu_scores () =
            Import.Scores.import_osu_scores_service.Request((), ignore)

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
            match Tables.Table.current () with
            | None -> ()
            | Some t ->

            Tables.Records.get (
                Interlude.Features.Online.Network.credentials.Username,
                t.Name.ToLower(),
                function
                | None -> ()
                | Some res ->
                    let lookup = res.Scores |> Seq.map (fun s -> s.Hash, s.Score) |> Map.ofSeq

                    for level in t.Levels do
                        for chart in level.Charts do
                            if
                                Scores.data.Entries.ContainsKey(chart.Hash)
                                && Scores.data.Entries.[chart.Hash].PersonalBests.ContainsKey(t.RulesetId)
                                && Scores.data.Entries.[chart.Hash].PersonalBests.[t.RulesetId].Accuracy
                                   |> Prelude.Gameplay.PersonalBests.get_best_above 1.0f
                                   |> Option.defaultValue 0.0
                                   |> fun acc -> acc > (Map.tryFind chart.Hash lookup |> Option.defaultValue 0.0)
                            then
                                for score in Scores.data.Entries.[chart.Hash].Scores do
                                    Charts.Scores.Save.post (
                                        ({
                                            ChartId = chart.Hash
                                            Replay = score.replay
                                            Rate = score.rate
                                            Mods = score.selectedMods
                                            Timestamp = score.time
                                        }
                                        : Web.Shared.Requests.Charts.Scores.Save.Request),
                                        ignore
                                    )
            )

        let private personal_best_fixer =
            { new Async.Service<string, unit>() with
                override this.Handle(ruleset_id) =
                    async {
                        match Content.Rulesets.try_get_by_hash ruleset_id with
                        | None -> ()
                        | Some ruleset ->

                        for hash in Scores.data.Entries.Keys |> Seq.toArray do
                            let data = Scores.data.Entries.[hash]

                            if data.Scores.Count = 0 then
                                ()
                            else

                                match Cache.by_hash hash Library.cache with
                                | None -> ()
                                | Some cc ->

                                match Cache.load cc Library.cache with
                                | None -> ()
                                | Some chart ->

                                for score in data.Scores do
                                    let info = ScoreInfoProvider(score, chart, ruleset)

                                    if data.PersonalBests.ContainsKey ruleset_id then
                                        let new_bests, _ = Bests.update info data.PersonalBests.[ruleset_id]
                                        data.PersonalBests.[ruleset_id] <- new_bests
                                    else
                                        data.PersonalBests.[ruleset_id] <- Bests.create info

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
                .WithCommand("import_osu_scores", "Import your local scores from osu!mania", import_osu_scores)
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

    let mutable ipc_shutdown_token: System.Threading.CancellationTokenSource option =
        None

    let ipc_commands = ShellContext.Empty |> Utils.register_ipc_commands

    let init (instance: int) =
        Terminal.exec_command <- exec

        if instance = 0 then
            ipc_shutdown_token <- Some(IPC.start_server_thread "Interlude" ipc_commands)

    let shutdown () =
        ipc_shutdown_token |> Option.iter (fun token -> token.Cancel())
