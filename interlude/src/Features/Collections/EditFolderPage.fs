namespace Interlude.Features.Collections

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI

type EditFolderPage(name: string, folder: Folder) =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphanumeric

    member this.SaveChanges() =
        if new_name.Value <> name && new_name.Value.Length > 1 then
            if Content.Collections.RenameCollection(name, new_name.Value) then
                CollectionActions.collection_modified_ev.Trigger()
            else
                Notifications.action_feedback (Icons.X, %"notification.collection_rename_failed.title", %"notification.collection_rename_failed.body")
                Logging.Debug "Rename failed, maybe that name already exists?"

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageTextEntry(%"collections.edit.folder_name", new_name)
                    .Pos(0),
                PageSetting(%"collections.edit.icon", SelectDropdown(CreateFolderPage.Icons, folder.Icon))
                    .Pos(2),
                PageButton(%"collections.edit.delete", fun () ->
                    ConfirmPage(
                        [ name ] %> "misc.confirmdelete",
                        fun () ->
                            // todo: these operations should live in Actions.fs and collection_modified_ev should be private
                            if Content.Collections.Delete name then
                                CollectionActions.collection_modified_ev.Trigger()
                                Menu.Back()
                    )
                        .Show()
                )
                    .TextColor(Colors.red_accent)
                    .Icon(Icons.TRASH)
                    .Pos(5)
            )

    override this.Title = name