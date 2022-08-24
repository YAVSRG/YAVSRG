namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.ChartFormats.Conversions
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
            if import then BackgroundTask.Create TaskFlags.NONE "Import from mounted source" (importMountedSource setting.Value.Value) |> ignore

    let handleStartupImports() =
        Logging.Debug("Checking for new songs in other games to import..")
        match options.OsuMount.Value with
        | Some mount -> if mount.ImportOnStartup then BackgroundTask.Create TaskFlags.NONE "Import new osu! songs" (importMountedSource mount) |> ignore
        | None -> ()
        match options.StepmaniaMount.Value with
        | Some mount -> if mount.ImportOnStartup then BackgroundTask.Create TaskFlags.NONE "Import new StepMania songs" (importMountedSource mount) |> ignore
        | None -> ()
        match options.EtternaMount.Value with
        | Some mount -> if mount.ImportOnStartup then BackgroundTask.Create TaskFlags.NONE "Import new Etterna songs" (importMountedSource mount) |> ignore
        | None -> ()

type CreateMountDialog(mountType: Mounts.Game, setting: Setting<MountedChartSource option>) as this =
    inherit Dialog()

    let text = 
        Text(
            match mountType with
            | Mounts.Game.Osu -> "Drag your osu! Songs folder onto this window."
            | Mounts.Game.Stepmania -> "Drag your Stepmania packs folder onto this window."
            | Mounts.Game.Etterna -> "Drag your Etterna packs folder onto this window."
            | _ -> failwith "impossible"
            ,
            Align = Alignment.CENTER,
            Position = { Left = 0.0f %+ 100.0f; Top = 0.5f %- 200.0f; Right = 1.0f %- 100.0f; Bottom = 0.5f %+ 200.0f })

    let button =
        Button(
            "Try to auto detect it",
            (fun () ->
                match mountType with
                | Mounts.Game.Osu -> osuSongFolder
                | Mounts.Game.Stepmania -> stepmaniaPackFolder
                | Mounts.Game.Etterna -> etternaPackFolder
                | _ -> failwith "impossible"
                |> Mounts.dropFunc.Value),
            "none",
            Position = { Left = 0.5f %- 150.0f; Top = 0.5f %+ 200.0f; Right = 0.5f %+ 150.0f; Bottom = 0.5f %+ 260.0f })

    do
        Mounts.dropFunc <-
            fun path ->
                match mountType, path with
                | Mounts.Game.Osu, PackFolder -> setting.Value <- MountedChartSource.Pack ("osu!", path) |> Some
                | Mounts.Game.Osu, _ -> Notifications.add ("Should be the osu! 'Songs' folder itself - This doesn't look like it.", NotificationType.Error)
                | Mounts.Game.Stepmania, FolderOfPacks
                | Mounts.Game.Etterna, FolderOfPacks -> setting.Value <- MountedChartSource.Library path |> Some
                | Mounts.Game.Stepmania, _
                | Mounts.Game.Etterna, _ -> Notifications.add ("Should be a Stepmania 'Songs' folder/pack library - This doesn't look like one.", NotificationType.Error)
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
        Mounts.dropFunc <- None
        base.Close()

    override this.Init(parent: Widget) =
        base.Init parent
        text.Init this
        button.Init this

type MountControl(mountType: Mounts.Game, setting: Setting<MountedChartSource option>) =
    inherit StaticWidget(NodeType.None)

    let createButton = 
        Button(Icons.add,
            (fun () -> CreateMountDialog(mountType, setting).Show()),
            "none",
            Position = { Left = 1.0f %- 120.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 60.0f; Bottom = 1.0f %+ 0.0f })
    let editButton = 
        Button(Icons.edit,
            (fun () -> Menu.ShowPage(Mounts.EditorPage setting)),
            "none",
            Position = { Left = 1.0f %- 120.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 60.0f; Bottom = 1.0f %+ 0.0f })
    let deleteButton = 
        Button(Icons.delete,
            (fun () -> setting.Value <- None),
            "none",
            Position = { Left = 1.0f %- 60.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ 0.0f })
    let text = 
        Text(
            match mountType with
            | Mounts.Game.Osu -> "osu!mania"
            | Mounts.Game.Stepmania -> "Stepmania"
            | Mounts.Game.Etterna -> "Etterna"
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