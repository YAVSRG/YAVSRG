﻿namespace Interlude.Features.Import

open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Library
open Prelude
open Prelude.Content.Noteskins.Conversion
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Options
open Interlude.Content

[<AutoOpen>]
module Import =

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish

    type ImportOsuNoteskinPage(ini: SkinIni, source_path: string, target_path: string) =
        inherit Page()

        let keymode: Setting<Keymode> = Setting.simple Keymode.``4K``
        let is_arrows: Setting<bool> = Setting.simple false

        override this.Init(parent: Widget) =
            page_container()
            |+ PageSetting("osuskinimport.keymode", Selector<Keymode>.FromEnum(keymode))
                .Pos(0)
            |+ Conditional(
                (fun () -> keymode.Value = Keymode.``4K``),
                PageSetting("osuskinimport.isarrows", Selector<_>.FromBool(is_arrows))
                    .Pos(2)
            )
            |+ PageButton
                .Once(
                    "osuskinimport.confirm",
                    fun () ->
                        try
                            OsuSkinConverter.convert
                                ini
                                source_path
                                target_path
                                (int keymode.Value)
                                (keymode.Value = Keymode.``4K`` && is_arrows.Value)
                        with err ->
                            Logging.Error("Error while converting to noteskin", err)

                        Noteskins.load ()
                        Menu.Back()
                )
                .Pos(5)
            |> this.Content

            base.Init parent

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
            page_container()
            |+ PageButton
                .Once(
                    "unlinkedsongsimport.link_intended",
                    fun () ->
                        Screen.change Screen.Type.Import Transitions.Flags.Default |> ignore
                        Menu.Back()
                )
                .Pos(6)
            |+ PageButton
                .Once(
                    "unlinkedsongsimport.confirm",
                    fun () ->
                        Imports.auto_convert.Request(
                            (path, false, Content.Library),
                            fun success ->
                                if success then
                                    Notifications.action_feedback (Icons.CHECK, %"notification.import_success", "")
                                    charts_updated_ev.Trigger()
                                else
                                    Notifications.error (%"notification.import_failure", "")
                        )

                        Menu.Back()
                )
                .Pos(9)
            |+ Callout.frame info (fun (w, h) -> Position.Box(0.0f, 0.0f, 100.0f, 200.0f, w, h))
            |> this.Content

        override this.Title = %"unlinkedsongsimport.name"
        override this.OnClose() = ()

    let import_osu_noteskin (path: string) =
        let id = Regex("[^a-zA-Z0-9_-]").Replace(Path.GetFileName(path), "")

        match OsuSkinConverter.check_before_convert path with
        | Ok ini ->
            sync
            <| fun () ->
                Menu.Exit()

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
                sync
                <| fun () ->
                    Menu.Exit()
                    ConfirmUnlinkedSongsImport(path).Show()
            else

            Imports.auto_convert.Request(
                (path, false, Content.Library),
                fun success ->
                    if success then
                        Notifications.action_feedback (Icons.CHECK, %"notification.import_success", "")
                        sync <| fun () -> charts_updated_ev.Trigger()
                    else
                        Notifications.error (%"notification.import_failure", "")
            )

type DownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed
