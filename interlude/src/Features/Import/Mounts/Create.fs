namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Conversions
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type private CreateMountPage(game: MountedGameType, setting: Setting<Imports.MountedChartSource option>) =
    inherit Page()

    let auto_detect_location =
        match game with
        | MountedGameType.Osu -> Imports.OSU_SONG_FOLDER
        | MountedGameType.Stepmania -> Imports.STEPMANIA_PACK_FOLDER
        | MountedGameType.Etterna -> Imports.ETTERNA_PACK_FOLDER
    let folder_detected = System.IO.Directory.Exists auto_detect_location

    let info =
        match game with
            | MountedGameType.Osu -> 
                Callout.Normal
                    .Icon(Icons.DOWNLOAD)
                    .Title(%"mount.create.osu.prompt")
                    .Body(%"mount.create.folder_hint")
            | MountedGameType.Stepmania ->
                Callout.Normal
                    .Icon(Icons.DOWNLOAD)
                    .Title(%"mount.create.stepmania.prompt")
                    .Body(%"mount.create.folder_hint")
            | MountedGameType.Etterna ->
                Callout.Normal
                    .Icon(Icons.DOWNLOAD)
                    .Title(%"mount.create.etterna.prompt")
                    .Body(%"mount.create.folder_hint")

    override this.Content() =
        on_file_drop <-
            fun path ->
                match game, path with
                | MountedGameType.Osu, PackFolder -> setting.Value <- Imports.MountedChartSource.Pack("osu!", path) |> Some
                | MountedGameType.Osu, _ -> Notifications.error (%"mount.create.osu.error", "")
                | MountedGameType.Stepmania, FolderOfPacks
                | MountedGameType.Etterna, FolderOfPacks -> setting.Value <- Imports.MountedChartSource.Library path |> Some
                | MountedGameType.Stepmania, _ -> Notifications.error (%"mount.create.stepmania.error", "")
                | MountedGameType.Etterna, _ -> Notifications.error (%"mount.create.etterna.error", "")

                if setting.Value.IsSome then
                    Imports.import_mounted_source.Request((setting.Value.Value, Content.Library), ignore)
                    Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")
                    Menu.Exit()
            |> Some

        page_container()
        |+ PageButton( "", 
            fun () -> on_file_drop.Value auto_detect_location
            , Enabled = folder_detected,
            Text = if folder_detected then %"mount.create.use_detected_folder" else %"mount.create.game_not_detected"
        ).Pos(6)
        |+ Callout.frame info (fun (w, h) -> pretty_pos (0, 5, PageWidth.Custom w))
        :> Widget

    override this.Title = %"mount.create.name"

    override this.OnClose() = on_file_drop <- None