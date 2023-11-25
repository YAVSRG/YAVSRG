namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Charts
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

[<AutoOpen>]
module Import =

    open System.IO
    open System.IO.Compression
    open System.Text.RegularExpressions
    open Prelude
    open Prelude.Data.Content
    open Prelude.Data.Content.Noteskin
    open Interlude.Options
    open Interlude.Content

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish

    type ImportOsuNoteskinPage(ini: OsuSkin.OsuSkinIni, source_path: string, target_path: string) as this =
        inherit Page()

        let keymode = Setting.simple options.KeymodePreference.Value

        do
            this.Content(
                column ()
                |+ PageSetting("osuskinimport.keymode", Selector<Keymode>.FromEnum(keymode))
                    .Pos(200.0f)
                |+ PageButton
                    .Once(
                        "osuskinimport.confirm",
                        fun () ->
                            try
                                SkinConversions.Osu.convert ini source_path target_path (int keymode.Value)
                            with err ->
                                Logging.Error("Error while converting to noteskin", err)

                            Noteskins.load ()
                            Menu.Back()
                    )
                    .Pos(400.0f)
            )

        override this.Title = ini.General.Name
        override this.OnClose() = ()

    type ConfirmUnlinkedSongsImport(path) as this =
        inherit Page()

        let info =
            Callout.Normal
                .Icon(Icons.ALERT_CIRCLE)
                .Title(%"unlinkedsongsimport.info.title")
                .Body(%"unlinkedsongsimport.info.body")

        do
            this.Content(
                column ()
                |+ PageButton
                    .Once(
                        "unlinkedsongsimport.link_intended",
                        fun () ->
                            Screen.change Screen.Type.Import Transitions.Flags.Default |> ignore
                            Menu.Back()
                    )
                    .Pos(500.0f)
                |+ PageButton
                    .Once(
                        "unlinkedsongsimport.confirm",
                        fun () ->
                            Library.Imports.auto_convert.Request(
                                (path, false),
                                fun success ->
                                    if success then
                                        Notifications.action_feedback (Icons.CHECK, %"notification.import_success", "")
                                        charts_updated_ev.Trigger()
                                    else
                                        Notifications.error (%"notification.import_failure", "")
                            )

                            Menu.Back()
                    )
                    .Pos(600.0f)
                |+ Callout.frame info (fun (w, h) -> Position.Box(0.0f, 0.0f, 100.0f, 200.0f, w, h + 40.0f))
            )

        override this.Title = %"unlinkedsongsimport.name"
        override this.OnClose() = ()

    let import_osu_noteskin (path: string) =
        let id = Regex("[^a-zA-Z0-9_-]").Replace(Path.GetFileName(path), "")

        match SkinConversions.Osu.check_before_convert path with
        | Ok ini ->
            ImportOsuNoteskinPage(
                ini,
                path,
                Path.Combine(get_game_folder "Noteskins", id + "-" + System.DateTime.Now.ToString("ddMMyyyyHHmmss"))
            )
                .Show()
        | Error err -> Logging.Error("Error while parsing osu! skin.ini\n" + err)
        // todo: error toast

    let handle_file_drop (path: string) =
        match Mounts.drop_func with
        | Some f -> f path
        | None ->

        match path with
        | OsuSkinFolder -> import_osu_noteskin path

        | InterludeSkinArchive ->
            try
                File.Copy(path, Path.Combine(get_game_folder "Noteskins", Path.GetFileName path))
                Noteskins.load ()
            with err ->
                Logging.Error("Something went wrong when moving this skin!", err)

        | OsuSkinArchive ->
            let id = Path.GetFileNameWithoutExtension(path)
            let target = Path.Combine(get_game_folder "Downloads", id)
            ZipFile.ExtractToDirectory(path, target)
            import_osu_noteskin target
        // todo: clean up extracted noteskin in downloads

        | Unknown -> // Treat it as a chart/pack/library import

            if Directory.Exists path && Path.GetFileName path = "Songs" then
                ConfirmUnlinkedSongsImport(path).Show()
            else

            Library.Imports.auto_convert.Request(
                (path, false),
                fun success ->
                    if success then
                        Notifications.action_feedback (Icons.CHECK, %"notification.import_success", "")
                        charts_updated_ev.Trigger()
                    else
                        Notifications.error (%"notification.import_failure", "")
            )

type DownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed
