namespace Interlude.Features.Mounts

open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Formats.Conversions
open Interlude.UI

type private MountFolderPickerPage(game: MountedGameType, callback: string -> unit) =
    inherit Page()

    let info =
        match game with
        | MountedGameType.Osu ->
            Callout.Normal.Icon(Icons.FOLDER_PLUS).Title(%"mount.create.osu.prompt").Body(%"mount.create.folder_hint")
        | MountedGameType.Quaver ->
            Callout.Normal
                .Icon(Icons.FOLDER_PLUS)
                .Title(%"mount.create.quaver.prompt")
                .Body(%"mount.create.folder_hint")
        | MountedGameType.Stepmania ->
            Callout.Normal
                .Icon(Icons.FOLDER_PLUS)
                .Title(%"mount.create.stepmania.prompt")
                .Body(%"mount.create.folder_hint")
        | MountedGameType.Etterna ->
            Callout.Normal
                .Icon(Icons.FOLDER_PLUS)
                .Title(%"mount.create.etterna.prompt")
                .Body(%"mount.create.folder_hint")

    override this.Content() : Widget =
        let path_chosen (path: string) : unit =
            match game, path with
            | MountedGameType.Osu, PackFolder
            | MountedGameType.Quaver, PackFolder
            | MountedGameType.Stepmania, FolderOfPacks
            | MountedGameType.Etterna, FolderOfPacks ->
                Menu.Back()
                callback path

            | MountedGameType.Osu, _ -> Notifications.Error(%"mount.create.osu.error")
            | MountedGameType.Quaver, _ -> Notifications.Error(%"mount.create.quaver.error")
            | MountedGameType.Stepmania, _ -> Notifications.Error(%"mount.create.stepmania.error")
            | MountedGameType.Etterna, _ -> Notifications.Error(%"mount.create.etterna.error")

        let choose_path () : unit =
            FileDialog.pick_folder(
                null,
                function
                | FileDialogResult.Ok path -> path_chosen path
                | FileDialogResult.Cancelled -> ()
                | FileDialogResult.Error -> Notifications.Error(%"notification.file_picker_error")
            )

        page_container().With(PageButton(%"mount.choose_folder", choose_path).Pos(6), CalloutCard(info).Pos(0))

    override this.Title = %"mount.folder_not_detected"
