namespace Prelude.Data.Library

open System.Collections.Generic
open Percyqaz.Data
open Prelude.Backbeat
open Prelude.Data.Library.Collections

[<RequireQualifiedAccess; Json.AutoCodec>]
type LibraryView =
    | All
    | Collections
    | Table

module LibraryView =

    type LexSortedGroups = Dictionary<int * string, Group>

    let get_groups
        (filter_by: Filter)
        (group_by: GroupMethod)
        (sort_by: SortMethod)
        (ctx: LibraryViewContext)
        : LexSortedGroups =

        let found_groups = new Dictionary<int * string, GroupWithSorting>()

        for cc in Filter.apply_seq (filter_by, ctx) ctx.Library.Charts.Cache.Values do
            let group_key = group_by (cc, ctx)

            if found_groups.ContainsKey group_key |> not then
                found_groups.Add(
                    group_key,
                    {
                        Charts = ResizeArray<ChartMeta * LibraryContext * SortingTag>()
                        Context = LibraryGroupContext.None
                    }
                )

            found_groups.[group_key].Charts.Add(cc, LibraryContext.None, sort_by (cc, ctx))

        let groups = new Dictionary<int * string, Group>()

        for g in found_groups.Keys |> Seq.toArray do
            groups.[g] <- found_groups.[g].ToGroup

        groups

    let get_collection_groups (filter_by: Filter) (sort_by: SortMethod) (ctx: LibraryViewContext) : LexSortedGroups =

        let groups = new Dictionary<int * string, Group>()

        for name in ctx.Library.Collections.Folders.Keys do
            let collection = ctx.Library.Collections.Folders.[name]

            collection.Charts
            |> Seq.choose (fun entry ->
                match ChartDatabase.get_meta entry.Hash ctx.Library.Charts with
                | Some cc -> Some(cc, LibraryContext.Folder name)
                | None -> None
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> Seq.sortBy (fun (cc, _) -> sort_by (cc, ctx))
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        (0, name),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Folder name
                        }
                    )

        for name in ctx.Library.Collections.Playlists.Keys do
            let playlist = ctx.Library.Collections.Playlists.[name]

            playlist.Charts
            |> Seq.indexed
            |> Seq.choose (fun (i, (entry, info)) ->
                match ChartDatabase.get_meta entry.Hash ctx.Library.Charts with
                | Some cc -> Some(cc, LibraryContext.Playlist(i, name, info))
                | None -> None
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        (0, name),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Playlist name
                        }
                    )

        groups

    let get_table_groups
        (filter_by: Filter)
        (sort_by: SortMethod)
        (table: Table)
        (ctx: LibraryViewContext)
        : LexSortedGroups =
        let groups = new Dictionary<int * string, Group>()

        for level, charts in table.Charts |> Seq.groupBy (fun x -> x.Level) do
            charts
            |> Seq.choose (fun (c: TableChart) ->
                match ChartDatabase.get_meta c.Hash ctx.Library.Charts with
                | Some cc -> Some(cc, LibraryContext.Table level)
                | None -> None
            )
            |> Filter.apply_ctx_seq (filter_by, ctx)
            |> Seq.sortBy (fun (cc, _) -> sort_by (cc, ctx))
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        (level,
                         table.Info.LevelDisplayNames.TryFind level
                         |> Option.defaultValue (level.ToString())),
                        {
                            Charts = x
                            Context = LibraryGroupContext.Table level
                        }
                    )

        groups

    let get_empty_view () = new Dictionary<int * string, Group>()
