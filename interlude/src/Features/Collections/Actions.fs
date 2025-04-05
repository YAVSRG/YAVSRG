namespace Interlude.Features.Collections

open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay

module CollectionActions =

    let internal collection_modified_ev = Event<unit>()
    let collection_modified = collection_modified_ev.Publish

    let private likes_modified_ev = Event<unit>()
    let likes_modified = likes_modified_ev.Publish

    let is_liked (chart_meta: ChartMeta) : bool =
        Content.Library.Collections.IsLiked chart_meta.Hash

    let toggle_liked (chart_meta: ChartMeta) : unit =
        if is_liked chart_meta then
            Content.Library.Collections.Unlike chart_meta.Hash
            likes_modified_ev.Trigger()
            Notifications.action_feedback (Icons.FOLDER_MINUS, [ chart_meta.Title ] %> "collections.unliked", "")
        else
            Content.Library.Collections.Like chart_meta.Hash
            likes_modified_ev.Trigger()
            Notifications.action_feedback (Icons.HEART, [ chart_meta.Title ] %> "collections.liked", "")

    let add_to (name: string, collection: Collection, chart_meta: ChartMeta) : bool =
        if
            match collection with
            | Folder c -> c.Add chart_meta
            | Playlist p -> p.Add(chart_meta, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
        then
            collection_modified_ev.Trigger()

            Notifications.action_feedback (Icons.FOLDER_PLUS, [ chart_meta.Title; name ] %> "collections.added", "")
            true
        else
            false

    let remove_from (name: string, collection: Collection, chart_meta: ChartMeta, context: LibraryContext) : bool =
        if
            match collection with
            | Folder c -> c.Remove chart_meta
            | Playlist p ->
                match context with
                | LibraryContext.Playlist(i, in_name, _) when name = in_name -> p.RemoveAt i
                | _ -> p.RemoveSingle chart_meta
        then
            collection_modified_ev.Trigger()

            Notifications.action_feedback (Icons.FOLDER_MINUS, [ chart_meta.Title; name ] %> "collections.removed", "")

            if Some chart_meta = SelectedChart.CACHE_DATA then
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