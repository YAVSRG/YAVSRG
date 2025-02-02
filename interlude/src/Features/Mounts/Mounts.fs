namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Options
open Interlude.Content
open Interlude.UI

module Mounts =

    let init_window () =
        match options.OsuMount.Value with
        | Some mount ->
            if Directory.Exists mount.SourceFolder then
                if mount.ImportOnStartup then
                    Logging.Info "Checking for new osu! songs to import.."
                    Imports.import_mounted_source.Request((mount, Content.Library), ignore)
            else
                Logging.Warn(
                    "osu! Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again."
                )
                Notifications.error(%"notification.mount_moved.osu", %"notification.mount_moved.body")
        | None -> ()

        match options.QuaverMount.Value with
        | Some mount ->
            if Directory.Exists mount.SourceFolder then
                if mount.ImportOnStartup then
                    Logging.Info "Checking for new Quaver songs to import.."
                    Imports.import_mounted_source.Request((mount, Content.Library), ignore)
            else
                Logging.Warn(
                    "Quaver Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again."
                )
                Notifications.error(%"notification.mount_moved.quaver", %"notification.mount_moved.body")
        | None -> ()

        match options.StepmaniaMount.Value with
        | Some mount ->
            if Directory.Exists mount.SourceFolder then
                if mount.ImportOnStartup then
                    Logging.Info "Checking for new Stepmania songs to import.."
                    Imports.import_mounted_source.Request((mount, Content.Library), ignore)
            else
                Logging.Warn(
                    "Stepmania Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again."
                )
                Notifications.error(%"notification.mount_moved.stepmania", %"notification.mount_moved.body")
        | None -> ()

        match options.EtternaMount.Value with
        | Some mount ->
            if Directory.Exists mount.SourceFolder then
                if mount.ImportOnStartup then
                    Logging.Info "Checking for new Etterna songs to import.."
                    Imports.import_mounted_source.Request((mount, Content.Library), ignore)
            else
                Logging.Warn(
                    "Etterna Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again."
                )
                Notifications.error(%"notification.mount_moved.etterna", %"notification.mount_moved.body")
        | None -> ()

    let tab =
        NavigationContainer.Column()
        |+ MountControl(MountedGameType.Osu, options.OsuMount, Position = Position.SliceT(100.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ MountControl(MountedGameType.Quaver, options.QuaverMount, Position = Position.SliceT(270.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ MountControl(MountedGameType.Stepmania, options.StepmaniaMount, Position = Position.SliceT(440.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ MountControl(MountedGameType.Etterna, options.EtternaMount, Position = Position.SliceT(610.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ Text(%"imports.mount", Align = Alignment.CENTER, Position = Position.SliceT(0.0f, 80.0f))
        |+ Text(%"imports.drag_and_drop_hint", Align = Alignment.CENTER, Position = Position.SliceB(80.0f).Translate(0.0f, -10.0f))