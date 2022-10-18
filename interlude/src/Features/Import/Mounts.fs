namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Charts.Formats.Conversions
open Prelude.Data.Charts.Library.Imports
open Interlude.Options
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
                |+ PrettySetting("mount.importatstartup", Selector<_>.FromBool importOnStartup).Pos(200.0f)
                |+ PrettyButton.Once(
                        "mount.import",
                        (fun () -> import <- true),
                        Localisation.localiseWith ["Import new songs"] "notification.taskstarted", NotificationType.Task
                    ).Pos(400.0f)
                |+ PrettyButton.Once(
                        "mount.importall",
                        (fun () -> import <- true; mount.LastImported <- System.DateTime.UnixEpoch),
                        Localisation.localiseWith ["Import all songs"] "notification.taskstarted", NotificationType.Task
                    ).Pos(500.0f)
            )

        override this.Title = N"mount"
        override this.OnClose() =
            setting.Value <- Some { mount with ImportOnStartup = importOnStartup.Value }
            if import then import_mounted_source.Request(setting.Value.Value, ignore)

    let handleStartupImports() =
        Logging.Debug("Checking for new songs in other games to import..")
        match options.OsuMount.Value with
        | Some mount -> if mount.ImportOnStartup then import_mounted_source.Request(mount, ignore)
        | None -> ()
        match options.StepmaniaMount.Value with
        | Some mount -> if mount.ImportOnStartup then import_mounted_source.Request(mount, ignore)
        | None -> ()
        match options.EtternaMount.Value with
        | Some mount -> if mount.ImportOnStartup then import_mounted_source.Request(mount, ignore)
        | None -> ()

    type CreateDialog(mountType: Game, setting: Setting<MountedChartSource option>) as this =
        inherit Dialog()

        let text = 
            Text(
                match mountType with
                | Game.Osu -> "Drag your osu! Songs folder onto this window."
                | Game.Stepmania -> "Drag your Stepmania packs folder onto this window."
                | Game.Etterna -> "Drag your Etterna packs folder onto this window."
                | _ -> failwith "impossible"
                ,
                Align = Alignment.CENTER,
                Position = { Left = 0.0f %+ 100.0f; Top = 0.5f %- 200.0f; Right = 1.0f %- 100.0f; Bottom = 0.5f %+ 200.0f })

        let button =
            Button(
                "Try to auto detect it",
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
                    | Game.Osu, _ -> Notifications.add ("Should be the osu! 'Songs' folder itself - This doesn't look like it.", NotificationType.Error)
                    | Game.Stepmania, FolderOfPacks
                    | Game.Etterna, FolderOfPacks -> setting.Value <- MountedChartSource.Library path |> Some
                    | Game.Stepmania, _
                    | Game.Etterna, _ -> Notifications.add ("Should be a Stepmania 'Songs' folder/pack library - This doesn't look like one.", NotificationType.Error)
                    | _ -> failwith "impossible"
                    if setting.Value.IsSome then this.Close()
                |> Some

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            text.Update(elapsedTime, bounds)
            button.Update(elapsedTime, bounds)
            if (!|"exit").Tapped() then this.Close()

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

    type Control(mountType: Game, setting: Setting<MountedChartSource option>) =
        inherit StaticWidget(NodeType.None)

        let createButton = 
            Button(Icons.add,
                (fun () -> CreateDialog(mountType, setting).Show()),
                Position = { Left = 1.0f %- 120.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 60.0f; Bottom = 1.0f %+ 0.0f })
        let editButton = 
            Button(Icons.edit,
                (fun () -> Menu.ShowPage(EditorPage setting)),
                Position = { Left = 1.0f %- 120.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 60.0f; Bottom = 1.0f %+ 0.0f })
        let deleteButton = 
            Button(Icons.delete,
                (fun () -> setting.Value <- None),
                Position = { Left = 1.0f %- 60.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ 0.0f })
        let text = 
            Text(
                match mountType with
                | Game.Osu -> "osu!mania"
                | Game.Stepmania -> "Stepmania"
                | Game.Etterna -> "Etterna"
                | _ -> failwith "impossible"
                ,
                Position = Position.TrimRight 120.0f
            )

        override this.Init(parent: Widget) =
            base.Init parent
            createButton.Init this
            editButton.Init this
            deleteButton.Init this
            text.Init this

        override this.Update(elapsedTime, moved) =
            if setting.Value.IsSome then
                editButton.Update(elapsedTime, moved)
                deleteButton.Update(elapsedTime, moved)
            else
                createButton.Update(elapsedTime, moved)
            text.Update(elapsedTime, moved)

        override this.Draw() =
            if setting.Value.IsSome then
                editButton.Draw()
                deleteButton.Draw()
            else
                createButton.Draw()
            text.Draw()

    let tab =
        StaticContainer(NodeType.None)
        |+ Control(Game.Osu, options.OsuMount,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 200.0f, 360.0f, 60.0f) )
        |+ Control(Game.Stepmania, options.StepmaniaMount,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 270.0f, 360.0f, 60.0f) )
        |+ Control(Game.Etterna, options.EtternaMount,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 340.0f, 360.0f, 60.0f) )
        |+ Text("Import from game",
            Align = Alignment.CENTER,
            Position = Position.Box(0.0f, 0.0f, 0.0f, 150.0f, 250.0f, 50.0f))