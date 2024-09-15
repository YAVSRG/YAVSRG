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

    type SortedGroups = (string * Group) seq

    let get_collection_groups (filter_by: Filter) (reverse_groups: bool) (sort_by: SortMethod) (reverse_sorting: bool) (ctx: LibraryViewContext) : SortedGroups =

        let groups = new Dictionary<string, Group>()

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
            |> if reverse_sorting then Seq.rev else id
            |> Array.ofSeq
            |> fun x ->
                if x.Length > 0 then
                    groups.Add(
                        name,
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
                        name,
                        {
                            Charts = x
                            Context = LibraryGroupContext.Playlist name
                        }
                    )

        groups 
        |> Seq.sortBy (fun kvp -> kvp.Key.ToLowerInvariant())
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (|KeyValue|)

    let get_table_groups
        (filter_by: Filter)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (table: Table)
        (ctx: LibraryViewContext)
        : SortedGroups =
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
            |> if reverse_sorting then Seq.rev else id
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
        |> Seq.sortBy (_.Key >> (fun (index, group_name) -> (index, group_name.ToLowerInvariant())))
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (fun kvp -> snd kvp.Key, kvp.Value)
    
    let get_groups
        (filter_by: Filter)
        (group_by: GroupMethod)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let found_groups = new Dictionary<int * string, GroupWithSorting>()

        for cc in Filter.apply_seq (filter_by, ctx) ctx.Library.Charts.Cache.Values do
            for group_key in group_by.Func (cc, ctx) do
                if found_groups.ContainsKey group_key |> not then
                    found_groups.Add(
                        group_key,
                        {
                            Charts = ResizeArray<ChartMeta * LibraryContext * SortingTag>()
                            Context = if group_by.IsPacks then LibraryGroupContext.Pack (snd group_key) else LibraryGroupContext.None
                        }
                    )

                found_groups.[group_key].Charts.Add(
                    cc, 
                    (if group_by.IsPacks then LibraryContext.Pack (snd group_key) else LibraryContext.None), 
                    sort_by (cc, ctx)
                )

        let collections = get_collection_groups filter_by reverse_groups sort_by reverse_sorting ctx

        let groups =
            found_groups
            |> Seq.sortBy (_.Key >> (fun (index, group_name) -> (index, group_name.ToLowerInvariant())))
            |> if reverse_groups then Seq.rev else id
            |> Seq.map (fun kvp -> snd kvp.Key, kvp.Value.ToGroup reverse_sorting)

        Seq.concat [collections; groups]

    let get_empty_view () : SortedGroups = Seq.empty