namespace Interlude.Features.Collections

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI

type CreateFolderPage(on_create: (string * Collection) -> unit) =
    inherit Page()

    let new_name = Setting.simple "" |> Setting.alphanumeric
    let icon = Setting.simple Icons.HEART

    override this.Content() =
        page_container()
            .With(
                PageTextEntry(%"collections.edit.folder_name", new_name)
                    .Pos(0),
                PageSetting(%"collections.edit.icon", SelectDropdown(CreateFolderPage.Icons, icon))
                    .Pos(3),
                PageButton(
                    %"confirm.yes",
                    (fun () ->
                        match Content.Collections.CreateFolder(new_name.Value, icon.Value) with
                        | Some folder ->
                            Menu.Back()
                            on_create (new_name.Value, Folder folder)
                            CollectionActions.collection_modified_ev.Trigger()
                        | None ->
                            Notifications.action_feedback (Icons.X, %"notification.collection_create_failed.title", %"notification.collection_create_failed.body")
                    )
                )
                    .Disabled(fun () -> new_name.Value = "")
                    .Pos(6)
            )

    override this.Title = %"collections.create_folder"

    static member Icons =
        [|
            Icons.HEART, Icons.HEART
            Icons.STAR, Icons.STAR
            Icons.BOOKMARK, Icons.BOOKMARK
            Icons.FOLDER, Icons.FOLDER
        |]