namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Collections

type PlaylistContextMenu(name: string, playlist: Playlist) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton(%"collections.edit", fun () -> EditPlaylistPage(name, playlist).Show())
            .Icon(Icons.EDIT_2)
            .Pos(0)
        |+ PageButton
            .Once(
                %"playlist.play",
                fun () -> LevelSelect.start_playlist (name, playlist)
        )
            .Icon(Icons.PLAY)
            .Pos(3)
        |+ PageButton
            .Once(
                %"playlist.play_shuffled",
                fun () -> LevelSelect.start_playlist_shuffled (name, playlist)
        )
            .Icon(Icons.SHUFFLE)
            .Pos(5)
        :> Widget

    override this.Title = name
    override this.OnClose() = ()

type GroupContextMenu(name: string, charts: ChartMeta seq, context: LibraryGroupContext) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton(%"bulk_actions.delete", fun () ->
            GroupContextMenu.ConfirmDelete(charts, context, true)
        )
            .TextColor(Colors.red_accent)
            .Icon(Icons.TRASH)
            .Hotkey("delete")
            .Pos(0)
        :> Widget

    override this.Title = name
    override this.OnClose() = ()

    static member ConfirmDelete(charts: ChartMeta seq, ctx: LibraryGroupContext, is_submenu: bool) =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_delete",
            fun () ->
                match ctx with
                | LibraryGroupContext.Pack p ->
                    ChartDatabase.delete_many_from_pack charts p Content.Charts
                | _ -> ChartDatabase.delete_many charts Content.Charts
                LevelSelect.refresh_all ()

                if is_submenu then
                    Menu.Back()
        )
            .Show()

    static member Show(name: string, charts: ChartMeta seq, context: LibraryGroupContext) =
        match context with
        | LibraryGroupContext.None
        | LibraryGroupContext.Pack _ -> GroupContextMenu(name, charts, context).Show()
        | LibraryGroupContext.Likes -> ()
        | LibraryGroupContext.Folder id -> EditFolderPage(id, Content.Collections.GetFolder(id).Value).Show()
        | LibraryGroupContext.Playlist id -> PlaylistContextMenu(id, Content.Collections.GetPlaylist(id).Value).Show()
        | LibraryGroupContext.Table lvl -> ()