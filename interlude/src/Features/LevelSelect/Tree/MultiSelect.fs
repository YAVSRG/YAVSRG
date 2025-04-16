namespace Interlude.Features.LevelSelect

open System.Collections.Generic
open Prelude.Data.Library

[<Struct>]
[<RequireQualifiedAccess>]
type private MultiSelectContext =
    | Playlist of playlist_id: string
    | Folder of folder_id: string
    | Likes
    | Normal

[<RequireQualifiedAccess>]
type AmountSelected =
    | All
    | Some
    | None

type MultiSelection =
    private {
        Context: MultiSelectContext
        mutable GroupSelectedCache: Map<string * LibraryGroupContext, AmountSelected>
        mutable Selected: HashSet<ChartMeta * LibraryContext>
    }

    static member Create(items: (ChartMeta * LibraryContext) seq) : MultiSelection =
        let ctx =
            match snd (Seq.head items) with
            | LibraryContext.Likes -> MultiSelectContext.Likes
            | LibraryContext.Folder f -> MultiSelectContext.Folder f
            | LibraryContext.Playlist (_, p, _) -> MultiSelectContext.Playlist p
            | LibraryContext.Table _
            | LibraryContext.Pack _
            | LibraryContext.None -> MultiSelectContext.Normal
        {
            Context = ctx
            GroupSelectedCache = Map.empty
            Selected = HashSet items
        }

    member this.Select(items: (ChartMeta * LibraryContext) seq) : unit =
        for item in items do
            let ctx = snd item
            if
                match this.Context, ctx with

                | MultiSelectContext.Normal, LibraryContext.None
                | MultiSelectContext.Normal, LibraryContext.Pack _
                | MultiSelectContext.Normal, LibraryContext.Table _ -> true

                | MultiSelectContext.Likes, LibraryContext.Likes -> true

                | MultiSelectContext.Folder f, LibraryContext.Folder f2 when f = f2 -> true

                | MultiSelectContext.Playlist p, LibraryContext.Playlist (_, p2, _) when p = p2 -> true

                | _ -> false
            then
                this.Selected.Add item |> ignore
        this.GroupSelectedCache <- Map.empty

    member this.Deselect(items: (ChartMeta * LibraryContext) seq) : unit =
        Seq.iter (this.Selected.Remove >> ignore) items
        this.GroupSelectedCache <- Map.empty

    member this.IsEmpty : bool = this.Selected.Count = 0

    member this.Contains(chart_meta: ChartMeta, ctx: LibraryContext) : bool = this.Selected.Contains (chart_meta, ctx)

    member this.GroupAmountSelected(group_name: string, group_ctx: LibraryGroupContext, charts: (ChartMeta * LibraryContext) seq) : AmountSelected =
        match this.GroupSelectedCache.TryFind (group_name, group_ctx) with
        | Some already_calculated -> already_calculated
        | None ->
            let mutable some = false
            let mutable all = true
            for c in charts do
                if this.Contains c then
                    some <- true
                else all <- false
            let result =
                if all then AmountSelected.All
                elif some then AmountSelected.Some
                else AmountSelected.None
            this.GroupSelectedCache <- this.GroupSelectedCache.Add ((group_name, group_ctx), result)
            result

    member this.ShowActions() =
        match this.Context with
        | MultiSelectContext.Normal -> BatchContextMenu(this.Selected).Show()
        | MultiSelectContext.Folder f -> BatchFolderContextMenu(f, this.Selected).Show()
        | MultiSelectContext.Playlist p -> BatchPlaylistContextMenu(p, this.Selected).Show()
        | MultiSelectContext.Likes -> BatchLikesContextMenu(this.Selected).Show()

    member this.ConfirmDelete() =
        match this.Context with
        | MultiSelectContext.Normal -> BulkActions.confirm_bulk_delete this.Selected
        | _ -> this.ShowActions() // todo: handle other cases like removing from collections with delete key?