namespace Prelude.Data.Library

open System.Collections.Generic
open Prelude.Backbeat
open Prelude.Data.Library.Collections

module LibraryView =

    type SortedGroups = (string * Group) seq

    let private get_collection_groups (filter_by: Filter) (reverse_groups: bool) (sort_by: SortMethod) (reverse_sorting: bool) (ctx: LibraryViewContext) : SortedGroups =

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

    let private get_table_groups
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

    let private get_normal_groups
        (filter_by: Filter)
        (group_by: GroupFunc)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (ctx: LibraryViewContext)
        : SortedGroups =

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

        found_groups
        |> Seq.sortBy (_.Key >> (fun (index, group_name) -> (index, group_name.ToLowerInvariant())))
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (fun kvp -> snd kvp.Key, kvp.Value.ToGroup reverse_sorting)
    
    let private get_packs
        (filter_by: Filter)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let found_groups = new Dictionary<string, GroupWithSorting>()

        for cc in Filter.apply_seq (filter_by, ctx) ctx.Library.Charts.Cache.Values do
            for pack in cc.Packs do

                if found_groups.ContainsKey pack |> not then
                    found_groups.Add(
                        pack,
                        {
                            Charts = ResizeArray<ChartMeta * LibraryContext * SortingTag>()
                            Context = LibraryGroupContext.Pack pack
                        }
                    )

                found_groups.[pack].Charts.Add(cc, LibraryContext.Pack pack, sort_by (cc, ctx))

        found_groups
        |> Seq.sortBy (fun kvp -> kvp.Key.ToLowerInvariant())
        |> if reverse_groups then Seq.rev else id
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value.ToGroup reverse_sorting)
    
    let get_groups
        (filter_by: Filter)
        (group_by: GroupMethod)
        (reverse_groups: bool)
        (sort_by: SortMethod)
        (reverse_sorting: bool)
        (always_show_collections: bool)
        (table: Table option)
        (ctx: LibraryViewContext)
        : SortedGroups =

        let groups =
            match group_by with
            | Normal func ->
                get_normal_groups filter_by func reverse_groups sort_by reverse_sorting ctx
            | Packs -> get_packs filter_by reverse_groups sort_by reverse_sorting ctx
            | Collections -> get_collection_groups filter_by reverse_groups sort_by reverse_sorting ctx
            | Levels -> 
                match table with
                | Some t -> get_table_groups filter_by reverse_groups sort_by reverse_sorting t ctx
                | None -> Seq.empty

        match group_by with
        | Normal _
        | Packs when always_show_collections ->
            let collections = get_collection_groups filter_by reverse_groups sort_by reverse_sorting ctx
            Seq.concat [collections; groups]
        | _ -> groups
