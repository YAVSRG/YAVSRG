namespace Interlude.Features.Collections

open Prelude
open Prelude.Data.Library
open Prelude.Data.Library.Collections
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay

module CollectionActions =

    let internal collection_modified_ev = Event<unit>()
    let collection_modified = collection_modified_ev.Publish

    let private likes_modified_ev = Event<unit>()
    let likes_modified = likes_modified_ev.Publish

    let is_liked (cc: ChartMeta) =
        Content.Library.Collections.IsLiked cc.Hash

    let like_chart (cc: ChartMeta) =
        if not (is_liked cc) then
            Content.Library.Collections.Like cc.Hash
            likes_modified_ev.Trigger()
            Notifications.action_feedback (Icons.HEART, [ cc.Title ] %> "collections.liked", "")

    let unlike_chart (cc: ChartMeta) =
        if is_liked cc then
            Content.Library.Collections.Unlike cc.Hash
            likes_modified_ev.Trigger()
            Notifications.action_feedback (Icons.FOLDER_MINUS, [ cc.Title ] %> "collections.unliked", "")

    let add_to (name: string, collection: Collection, cc: ChartMeta) =
        if
            match collection with
            | Folder c -> c.Add cc
            | Playlist p -> p.Add(cc, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
        then
            collection_modified_ev.Trigger()

            Notifications.action_feedback (Icons.FOLDER_PLUS, [ cc.Title; name ] %> "collections.added", "")
            true
        else
            false

    let remove_from (name: string, collection: Collection, cc: ChartMeta, context: LibraryContext) =
        if
            match collection with
            | Folder c -> c.Remove cc
            | Playlist p ->
                match context with
                | LibraryContext.Playlist(i, in_name, _) when name = in_name -> p.RemoveAt i
                | _ -> p.RemoveSingle cc
        then
            collection_modified_ev.Trigger()

            Notifications.action_feedback (Icons.FOLDER_MINUS, [ cc.Title; name ] %> "collections.removed", "")

            if Some cc = SelectedChart.CACHE_DATA then
                SelectedChart.LIBRARY_CTX <- LibraryContext.None

            true
        else
            false

    let reorder_up (context: LibraryContext) : bool =
        match context with
        | LibraryContext.Playlist(index, id, data) ->
            if Content.Collections.GetPlaylist(id).Value.MoveChartUp index then
                if SelectedChart.LIBRARY_CTX.Matches context then
                    SelectedChart.LIBRARY_CTX <- LibraryContext.Playlist(index - 1, id, data)

                collection_modified_ev.Trigger()
                true
            else
                false
        | _ -> false

    let reorder_down (context: LibraryContext) : bool =
        match context with
        | LibraryContext.Playlist(index, id, data) ->
            if Content.Collections.GetPlaylist(id).Value.MoveChartDown index then
                if SelectedChart.LIBRARY_CTX.Matches context then
                    SelectedChart.LIBRARY_CTX <- LibraryContext.Playlist(index + 1, id, data)

                collection_modified_ev.Trigger()
                true
            else
                false
        | _ -> false
