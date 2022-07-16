namespace Interlude.UI.Screens.Import

open Percyqaz.Common
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.ChartFormats.Conversions
open Prelude.Data.Charts.Library.Imports
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module Mounts =
    
    let mutable dropFunc : (string -> unit) option = None

    type Types =
        | Osu = 0
        | Stepmania = 1
        | Etterna = 2

    let editor (setting: Setting<MountedChartSource option>) =
        let mount = setting.Value.Value
        let importOnStartup = Setting.simple mount.ImportOnStartup
        let mutable import = false
        {
            Content = fun _ ->
                column [
                    PrettySetting("mount.importatstartup", Selector<_>.FromBool importOnStartup).Position(200.0f)
                    PrettyButton.Once(
                        "mount.import",
                        (fun () -> import <- true),
                        Localisation.localiseWith ["Import new songs"] "notification.taskstarted", NotificationType.Task
                    ).Position(400.0f)
                    PrettyButton.Once(
                        "mount.importall",
                        (fun () -> import <- true; mount.LastImported <- System.DateTime.UnixEpoch),
                        Localisation.localiseWith ["Import all songs"] "notification.taskstarted", NotificationType.Task
                    ).Position(500.0f)
                ] :> Selectable
            Callback = fun () ->
                setting.Value <- Some { mount with ImportOnStartup = importOnStartup.Value }
                if import then BackgroundTask.Create TaskFlags.NONE "Import from mounted source" (importMountedSource setting.Value.Value) |> ignore
        }

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

type CreateMountDialog(mountType: Mounts.Types, setting: Setting<MountedChartSource option>, callback: bool -> unit) as this =
    inherit Dialog()

    do
        Mounts.dropFunc <-
            fun path ->
                match mountType, path with
                | Mounts.Types.Osu, PackFolder -> setting.Value <- MountedChartSource.Pack ("osu!", path) |> Some
                | Mounts.Types.Osu, _ -> Notification.add ("Should be the osu! Songs folder itself - This doesn't look like it.", NotificationType.Error)
                | Mounts.Types.Stepmania, FolderOfPacks
                | Mounts.Types.Etterna, FolderOfPacks -> setting.Value <- MountedChartSource.Library path |> Some
                | Mounts.Types.Stepmania, _
                | Mounts.Types.Etterna, _ -> Notification.add ("Should be a Stepmania pack library - This doesn't look like one.", NotificationType.Error)
                | _ -> failwith "impossible"
                if setting.Value.IsSome then this.BeginClose()
            |> Some

        TextBox(
            match mountType with
            | Mounts.Types.Osu -> "Drag your osu! Songs folder onto this window."
            | Mounts.Types.Stepmania -> "Drag your Stepmania packs folder onto this window."
            | Mounts.Types.Etterna -> "Drag your Etterna packs folder onto this window."
            | _ -> failwith "impossible"
            |> K,
            K (Color.White, Color.Black),
            0.5f)
            .Position { Left = 0.0f %+ 100.0f; Top = 0.5f %- 200.0f; Right = 1.0f %- 100.0f; Bottom = 0.5f %+ 200.0f }
        |> this.Add

        Button(
            (fun () ->
                match mountType with
                | Mounts.Types.Osu -> osuSongFolder
                | Mounts.Types.Stepmania -> stepmaniaPackFolder
                | Mounts.Types.Etterna -> etternaPackFolder
                | _ -> failwith "impossible"
                |> Mounts.dropFunc.Value),
            "Or try to auto detect it")
            .Position { Left = 0.5f %- 150.0f; Top = 0.5f %+ 200.0f; Right = 0.5f %+ 150.0f; Bottom = 0.5f %+ 260.0f }
        |> this.Add

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (!|"exit").Tapped() then this.BeginClose()

    override this.OnClose() = 
        Mounts.dropFunc <- None
        callback setting.Value.IsSome

type MountControl(mountType: Mounts.Types, setting: Setting<MountedChartSource option>) as this =
    inherit Widget()

    let mutable refresh = ignore

    let createButton = Button((fun () -> CreateMountDialog(mountType, setting, fun b -> if b then refresh()).Show()), Icons.add)
    let editButton = Button((fun () -> SelectionMenu(N"mount", Mounts.editor setting).Show()), Icons.edit)
    let deleteButton = Button((fun () -> setting.Value <- None; refresh()), Icons.delete)

    do
        refresh <-
            fun () ->
                if setting.Value.IsSome then
                    createButton.Enabled <- false
                    editButton.Enabled <- true
                    deleteButton.Enabled <- true
                else
                    createButton.Enabled <- true
                    editButton.Enabled <- false
                    deleteButton.Enabled <- false
        refresh()

        this
        |-+ createButton.Position { Left = 1.0f %- 120.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 60.0f; Bottom = 1.0f %+ 0.0f }
        |-+ editButton.Position { Left = 1.0f %- 120.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %- 60.0f; Bottom = 1.0f %+ 0.0f }
        |-+ deleteButton.Position { Left = 1.0f %- 60.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ 0.0f }
        |=+ TextBox(
                match mountType with
                | Mounts.Types.Osu -> "osu!mania"
                | Mounts.Types.Stepmania -> "Stepmania"
                | Mounts.Types.Etterna -> "Etterna"
                | _ -> failwith "impossible"
                |> K,
                K (Color.White, Color.Black), 0.5f )
            .Position( Position.TrimRight 120.0f )