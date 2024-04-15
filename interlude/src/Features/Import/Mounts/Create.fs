namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Conversions
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI

type private CreateMountDialog(game: MountedGameType, setting: Setting<Imports.MountedChartSource option>) as this =
    inherit Dialog()

    let text =
        Text(
            match game with
            | MountedGameType.Osu -> %"imports.mount.create.osu.hint"
            | MountedGameType.Stepmania -> %"imports.mount.create.stepmania.hint"
            | MountedGameType.Etterna -> %"imports.mount.create.etterna.hint"
            , Align = Alignment.CENTER
            , Position =
                {
                    Left = 0.0f %+ 100.0f
                    Top = 0.5f %- 200.0f
                    Right = 1.0f %- 100.0f
                    Bottom = 0.5f %+ 200.0f
                }
        )

    let button =
        Button(
            %"imports.mount.create.auto",
            (fun () ->
                match game with
                | MountedGameType.Osu -> Imports.OSU_SONG_FOLDER
                | MountedGameType.Stepmania -> Imports.STEPMANIA_PACK_FOLDER
                | MountedGameType.Etterna -> Imports.ETTERNA_PACK_FOLDER
                |> on_file_drop.Value
            ),
            Position =
                {
                    Left = 0.5f %- 150.0f
                    Top = 0.5f %+ 200.0f
                    Right = 0.5f %+ 150.0f
                    Bottom = 0.5f %+ 260.0f
                }
        )

    do
        on_file_drop <-
            fun path ->
                match game, path with
                | MountedGameType.Osu, PackFolder -> setting.Value <- Imports.MountedChartSource.Pack("osu!", path) |> Some
                | MountedGameType.Osu, _ -> Notifications.error (%"imports.mount.create.osu.error", "")
                | MountedGameType.Stepmania, FolderOfPacks
                | MountedGameType.Etterna, FolderOfPacks -> setting.Value <- Imports.MountedChartSource.Library path |> Some
                | MountedGameType.Stepmania, _ -> Notifications.error (%"imports.mount.create.stepmania.error", "")
                | MountedGameType.Etterna, _ -> Notifications.error (%"imports.mount.create.etterna.error", "")
                | _ -> failwith "impossible"

                if setting.Value.IsSome then
                    Imports.import_mounted_source.Request((setting.Value.Value, Content.Library), ignore)
                    Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.import_queued", "")
                    this.Close()
            |> Some

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        text.Update(elapsed_ms, moved)
        button.Update(elapsed_ms, moved)

        if (%%"exit").Tapped() then
            this.Close()

    override this.Draw() =
        text.Draw()
        button.Draw()

    override this.Close() =
        on_file_drop <- None
        base.Close()

    override this.Init(parent: Widget) =
        base.Init parent
        text.Init this
        button.Init this