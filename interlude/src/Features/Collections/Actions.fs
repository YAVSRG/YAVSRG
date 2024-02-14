namespace Interlude.Features.Collections

open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Collections
open Interlude.Utils
open Interlude.UI
open Interlude.Features.Gameplay

module CollectionActions =

    let collection_modified_ev = Event<unit>()
    let collection_modified = collection_modified_ev.Publish

    let add_to (name: string, collection: Collection, cc: CachedChart) =
        if
            match collection with
            | Folder c -> c.Add cc
            | Playlist p -> p.Add(cc, rate.Value, selected_mods.Value)
        then
            collection_modified_ev.Trigger ()

            Notifications.action_feedback (Icons.FOLDER_PLUS, [ cc.Title; name ] %> "collections.added", "")
            true
        else
            false

    let remove_from (name: string, collection: Collection, cc: CachedChart, context: LibraryContext) =
        if
            match collection with
            | Folder c -> c.Remove cc
            | Playlist p ->
                match context with
                | LibraryContext.Playlist(i, in_name, _) when name = in_name -> p.RemoveAt i
                | _ -> p.RemoveSingle cc
        then
            collection_modified_ev.Trigger ()

            Notifications.action_feedback (Icons.FOLDER_MINUS, [ cc.Title; name ] %> "collections.removed", "")

            if Some cc = Chart.CACHE_DATA then
                Chart.LIBRARY_CTX <- LibraryContext.None

            true
        else
            false

    let reorder_up (context: LibraryContext) : bool =
        match context with
        | LibraryContext.Playlist(index, id, data) ->
            if collections.GetPlaylist(id).Value.MoveChartUp index then
                if Chart.LIBRARY_CTX = context then
                    Chart.LIBRARY_CTX <- LibraryContext.Playlist(index - 1, id, data)
                    
                collection_modified_ev.Trigger ()
                true
            else
                false
        | _ -> false

    let reorder_down (context: LibraryContext) : bool =
        match context with
        | LibraryContext.Playlist(index, id, data) ->
            if collections.GetPlaylist(id).Value.MoveChartDown index then
                if Chart.LIBRARY_CTX = context then
                    Chart.LIBRARY_CTX <- LibraryContext.Playlist(index + 1, id, data)
                    
                collection_modified_ev.Trigger ()
                true
            else
                false
        | _ -> false