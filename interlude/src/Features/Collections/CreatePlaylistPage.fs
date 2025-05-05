namespace Interlude.Features.Collections

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI

type CreatePlaylistPage(starting_name: string, on_create: (string * Collection) -> unit) =
    inherit Page()

    let new_name = Setting.simple starting_name |> Setting.alphanumeric
    let icon = Setting.simple Icons.HEART

    override this.Content() =
        page_container()
            .With(
                PageTextEntry(%"collections.edit.playlist_name", new_name)
                    .Pos(0),
                PageSetting(%"collections.edit.icon", SelectDropdown(CreatePlaylistPage.Icons, icon))
                    .Pos(3),
                PageButton(%"confirm.yes", fun () ->
                    match Content.Collections.CreatePlaylist(new_name.Value, icon.Value) with
                    | Some playlist ->
                        Menu.Back()
                        on_create (new_name.Value, Playlist playlist)
                        CollectionActions.collection_modified_ev.Trigger()
                    | None ->
                        Notifications.action_feedback (Icons.X, %"notification.collection_create_failed.title", %"notification.collection_create_failed.body")
                )
                    .Disabled(fun() -> new_name.Value = "")
                    .Pos(6)
            )
        :> Widget

    override this.Title = %"collections.create_playlist"

    static member Icons =
        [|
            Icons.STAR, Icons.STAR
            Icons.FLAG, Icons.FLAG
            Icons.PLAY, Icons.PLAY
            Icons.LIST, Icons.LIST
        |]