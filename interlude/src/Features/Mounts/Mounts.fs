namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
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