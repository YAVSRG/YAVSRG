namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Collections

type PlaylistContextMenu(name: string, playlist: Playlist) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton("collections.edit", (fun () -> EditPlaylistPage(name, playlist).Show()), Icon = Icons.EDIT_2)
            .Pos(0)
        |+ PageButton
            .Once(
                "playlist.play",
                (fun () ->
                    Suggestions.begin_endless_mode (EndlessModeState.create_from_playlist 0 playlist Content.Library) true
                ),
                Icon = Icons.PLAY
            )
            .Pos(3)
        |+ PageButton
            .Once(
                "playlist.play_shuffled",
                (fun () ->
                    Suggestions.begin_endless_mode (EndlessModeState.create_from_playlist_shuffled playlist Content.Library) true
                ),
                Icon = Icons.SHUFFLE
            )
            .Pos(5)
        :> Widget

    override this.Title = name
    override this.OnClose() = ()

type GroupContextMenu(name: string, charts: CachedChart seq, context: LibraryGroupContext) =
    inherit Page()

    override this.Content() =
        FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))
        |+ PageButton(
            "group.delete",
            (fun () -> GroupContextMenu.ConfirmDelete(name, charts, true)),
            Icon = Icons.TRASH
        )
            .Tooltip(Tooltip.Info("group.delete"))
        :> Widget

    override this.Title = name
    override this.OnClose() = ()

    static member ConfirmDelete(name, charts, is_submenu) =
        let group_name = sprintf "%s (%i charts)" name (Seq.length charts)

        ConfirmPage(
            [ group_name ] %> "misc.confirmdelete",
            fun () ->
                Cache.delete_many charts Content.Cache
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