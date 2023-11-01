namespace Interlude.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Charts.Formats.Conversions
open Prelude.Data.Charts.Library.Imports
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

module Mounts =
    
    let mutable dropFunc : (string -> unit) option = None

    type Game =
        | Osu = 0
        | Stepmania = 1
        | Etterna = 2

    type EditorPage(setting: Setting<MountedChartSource option>) as this =
        inherit Page()

        let mount = setting.Value.Value
        let importOnStartup = Setting.simple mount.ImportOnStartup
        let mutable import = false

        do
            this.Content(
                column()
                |+ PageSetting("mount.importatstartup", Selector<_>.FromBool importOnStartup)
                    .Pos(200.0f)
                    .Tooltip(Tooltip.Info("mount.importatstartup"))
                |+ PageButton.Once(
                        "mount.import",
                        fun () -> 
                            import <- true
                            Notifications.action_feedback(Icons.add_to_collection, L"notification.import_queued", "") )
                    .Pos(300.0f)
                    .Tooltip(Tooltip.Info("mount.import"))
                |+ PageButton.Once(
                        "mount.importall",
                        fun () -> 
                            import <- true
                            mount.LastImported <- System.DateTime.UnixEpoch
                            Notifications.action_feedback(Icons.add_to_collection, L"notification.import_queued", "") )
                    .Pos(370.0f)
                    .Tooltip(Tooltip.Info("mount.importall"))
                |> fun col -> 
                    if obj.ReferenceEquals(setting, options.OsuMount) && mount.LastImported <> System.DateTime.UnixEpoch then
                        col 
                        |+ PageButton.Once(
                            "mount.import_osu_scores",
                            fun () ->
                                Scores.import_osu_scores_service.Request((), fun () -> Notifications.task_feedback(Icons.add_to_collection, L"notification.score_import_success", ""))
                                Notifications.action_feedback(Icons.add_to_collection, L"notification.score_import_queued", "")
                            )
                            .Pos(470.0f)
                            .Tooltip(Tooltip.Info("mount.import_osu_scores"))
                    else col
                )

        override this.Title = L"mount.name"
        override this.OnClose() =
            setting.Value <- Some { mount with ImportOnStartup = importOnStartup.Value }
            if import then import_mounted_source.Request(setting.Value.Value, 
                fun () -> Notifications.task_feedback(Icons.add_to_collection, L"notification.import_success", ""))

    let handleStartupImports() =
        // todo: ingame notification if mount has moved
        match options.OsuMount.Value with
        | Some mount ->
            if Directory.Exists mount.SourceFolder then
                if mount.ImportOnStartup then
                    Logging.Info "Checking for new osu! songs to import.."
                    import_mounted_source.Request(mount, ignore)
            else Logging.Warn("osu! Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again.")
        | None -> ()
        match options.StepmaniaMount.Value with
        | Some mount -> 
            if Directory.Exists mount.SourceFolder then
                if mount.ImportOnStartup then 
                    Logging.Info "Checking for new Stepmania songs to import.."
                    import_mounted_source.Request(mount, ignore)
            else Logging.Warn("Stepmania Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again.")
        | None -> ()
        match options.EtternaMount.Value with
        | Some mount ->
            if Directory.Exists mount.SourceFolder then
                if mount.ImportOnStartup then 
                    Logging.Info "Checking for new Etterna songs to import.."
                    import_mounted_source.Request(mount, ignore)
            else Logging.Warn("Etterna Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again.")
        | None -> ()

    type CreateDialog(mountType: Game, setting: Setting<MountedChartSource option>) as this =
        inherit Dialog()

        let text = 
            Text(
                match mountType with
                | Game.Osu -> L"imports.mount.create.osu.hint"
                | Game.Stepmania -> L"imports.mount.create.stepmania.hint"
                | Game.Etterna -> L"imports.mount.create.etterna.hint"
                | _ -> failwith "impossible"
                ,
                Align = Alignment.CENTER,
                Position = { Left = 0.0f %+ 100.0f; Top = 0.5f %- 200.0f; Right = 1.0f %- 100.0f; Bottom = 0.5f %+ 200.0f })

        let button =
            Button(
                L"imports.mount.create.auto",
                (fun () ->
                    match mountType with
                    | Game.Osu -> osuSongFolder
                    | Game.Stepmania -> stepmaniaPackFolder
                    | Game.Etterna -> etternaPackFolder
                    | _ -> failwith "impossible"
                    |> dropFunc.Value),
                Position = { Left = 0.5f %- 150.0f; Top = 0.5f %+ 200.0f; Right = 0.5f %+ 150.0f; Bottom = 0.5f %+ 260.0f })

        do
            dropFunc <-
                fun path ->
                    match mountType, path with
                    | Game.Osu, PackFolder -> setting.Value <- MountedChartSource.Pack ("osu!", path) |> Some
                    | Game.Osu, _ -> Notifications.error (L"imports.mount.create.osu.error", "")
                    | Game.Stepmania, FolderOfPacks
                    | Game.Etterna, FolderOfPacks -> setting.Value <- MountedChartSource.Library path |> Some
                    | Game.Stepmania, _ -> Notifications.error (L"imports.mount.create.stepmania.error", "")
                    | Game.Etterna, _ -> Notifications.error (L"imports.mount.create.etterna.error", "")
                    | _ -> failwith "impossible"
                    if setting.Value.IsSome then 
                        import_mounted_source.Request(setting.Value.Value, ignore)
                        Notifications.action_feedback(Icons.add_to_collection, L"notification.import_queued", "")
                        this.Close()
                |> Some

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            text.Update(elapsedTime, bounds)
            button.Update(elapsedTime, bounds)
            if (+."exit").Tapped() then this.Close()

        override this.Draw() =
            text.Draw()
            button.Draw()

        override this.Close() = 
            dropFunc <- None
            base.Close()

        override this.Init(parent: Widget) =
            base.Init parent
            text.Init this
            button.Init this

    type Control(mountType: Game, setting: Setting<MountedChartSource option>) as this =
        inherit Frame(NodeType.Switch(fun _ -> this.WhoShouldFocus))

        let createButton = 
            Button(sprintf "%s %s" Icons.import_local (L"imports.mount.create"),
                (fun () -> CreateDialog(mountType, setting).Show()),
                Position = Position.SliceBottom(60.0f).Margin 5.0f)
        let editButtons =
            NavigationContainer.Row<Button>(Position = Position.SliceBottom(60.0f).Margin 5.0f)
            |+ Button(sprintf "%s %s" Icons.edit (L"imports.mount.edit"),
                (fun () -> EditorPage(setting).Show()),
                Position = { Position.Default with Right = 0.5f %+ 0.0f })
            |+ Button(sprintf "%s %s" Icons.delete (L"imports.mount.delete"),
                (fun () -> setting.Value <- None),
                Position = { Position.Default with Left = 0.5f %+ 0.0f })

        override this.Init(parent: Widget) =
            this
            |+ Text(
                match mountType with
                | Game.Osu -> "osu!mania"
                | Game.Stepmania -> "Stepmania"
                | Game.Etterna -> "Etterna"
                | _ -> failwith "impossible"
                ,
                Position = Position.SliceTop 80.0f,
                Align = Alignment.CENTER)
            |* Text(
                fun () -> 
                    match setting.Value with
                    | Some s -> Localisation.localiseWith [if s.LastImported = System.DateTime.UnixEpoch then "--" else s.LastImported.ToString()] "imports.mount.lastimported"
                    | None -> L"imports.mount.notlinked"
                ,
                Color = K Colors.text_subheading,
                Position = Position.SliceTop(145.0f).TrimTop(85.0f),
                Align = Alignment.CENTER)
            base.Init parent
            createButton.Init this
            editButtons.Init this

        member private this.WhoShouldFocus : Widget =
            if setting.Value.IsSome then editButtons else createButton

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            this.WhoShouldFocus.Update(elapsedTime, moved)

        override this.Draw() =
            base.Draw()
            this.WhoShouldFocus.Draw()

    let tab =
        NavigationContainer.Column<Widget>()
        |+ Control(Game.Osu, options.OsuMount,
            Position = Position.Row(100.0f, 200.0f) )
        |+ Control(Game.Stepmania, options.StepmaniaMount,
            Position = Position.Row(320.0f, 200.0f) )
        |+ Control(Game.Etterna, options.EtternaMount,
            Position = Position.Row(540.0f, 200.0f) )
        |+ Text(L"imports.mount",
            Align = Alignment.CENTER,
            Position = Position.Row(0.0f, 80.0f) )
        |+ Text(L"imports.drag_and_drop_hint",
            Align = Alignment.CENTER,
            Position = Position.Row(740.0f, 80.0f) )