namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Prelude.Data.Library.Collections
open Interlude.Content
open Interlude.UI
open Interlude.Features.Collections

type PlaylistContextMenu(name: string, playlist: Playlist) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton(%"collections.edit", (fun () -> EditPlaylistPage(name, playlist).Show()), Icon = Icons.EDIT_2)
            .Pos(0)
        |+ PageButton
            .Once(
                %"playlist.play",
                (fun () -> LevelSelect.start_playlist playlist),
                Icon = Icons.PLAY
            )
            .Pos(3)
        |+ PageButton
            .Once(
                %"playlist.play_shuffled",
                (fun () -> LevelSelect.start_playlist_shuffled playlist),
                Icon = Icons.SHUFFLE
            )
            .Pos(5)
        :> Widget

    override this.Title = name
    override this.OnClose() = ()

type GroupContextMenu(name: string, charts: ChartMeta seq, context: LibraryGroupContext) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton(
            %"group.delete",
            (fun () -> GroupContextMenu.ConfirmDelete(name, charts, true)),
            Icon = Icons.TRASH
        )
            .Help(Help.Info("group.delete"))
            .Pos(0)
        :> Widget

    override this.Title = name
    override this.OnClose() = ()

    static member ConfirmDelete(name, charts, is_submenu) =
        let group_name = sprintf "%s (%i charts)" name (Seq.length charts)

        ConfirmPage(
            [ group_name ] %> "misc.confirmdelete",
            fun () ->
                ChartDatabase.delete_many charts Content.Charts
                LevelSelect.refresh_all ()

                if is_submenu then
                    Menu.Back()
        )
            .Show()

    static member Show(name, charts, context) =
        match context with
        | LibraryGroupContext.None -> GroupContextMenu(name, charts, context).Show()
        | LibraryGroupContext.Folder id -> EditFolderPage(id, Content.Collections.GetFolder(id).Value).Show()
        | LibraryGroupContext.Playlist id -> PlaylistContextMenu(id, Content.Collections.GetPlaylist(id).Value).Show()
        | LibraryGroupContext.Table lvl -> ()