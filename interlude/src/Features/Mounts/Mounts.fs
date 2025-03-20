namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Imports
open Interlude.Options
open Interlude.Content
open Interlude.UI

module Mounts =

    let init () =

        let import_mount_if_auto_enabled(mount: Setting<MountedChartSource option>, name: string, moved_warning: string) =
            match mount.Value with
            | Some mount ->
                if Directory.Exists mount.SourceFolder then
                    if mount.ImportOnStartup then
                        Logging.Info "Checking for new %s songs to import.." name
                        let task = Mount.import_new(mount, Content.Charts, Content.UserData, ignore)
                        import_queue.Request(task, ignore)
                else
                    Logging.Warn
                        "%s Songs folder has moved or can no longer be found.\n This may break any mounted songs, if so you will need to set up the link again."
                        name
                    Notifications.error(moved_warning, %"notification.mount_moved.body")
            | None -> ()

        import_mount_if_auto_enabled(options.OsuMount, "osu!", %"notification.mount_moved.osu")
        import_mount_if_auto_enabled(options.QuaverMount, "Quaver", %"notification.mount_moved.quaver")
        import_mount_if_auto_enabled(options.EtternaMount, "Etterna", %"notification.mount_moved.etterna")
        import_mount_if_auto_enabled(options.StepmaniaMount, "Stepmania", %"notification.mount_moved.stepmania")

    let tab =
        NavigationContainer.Column()
        |+ MountControl(MountedGameType.Osu, options.OsuMount, Position = Position.SliceT(100.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ MountControl(MountedGameType.Quaver, options.QuaverMount, Position = Position.SliceT(270.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ MountControl(MountedGameType.Etterna, options.EtternaMount, Position = Position.SliceT(440.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ MountControl(MountedGameType.Stepmania, options.StepmaniaMount, Position = Position.SliceT(610.0f, 150.0f).Shrink(200.0f, 0.0f))
        |+ Text(%"imports.mount", Align = Alignment.CENTER, Position = Position.SliceT(0.0f, 80.0f))
        |+ Text(%"imports.drag_and_drop_hint", Align = Alignment.CENTER, Position = Position.SliceB(80.0f).Translate(0.0f, -10.0f))