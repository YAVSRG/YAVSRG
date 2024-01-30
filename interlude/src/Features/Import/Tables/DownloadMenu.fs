namespace Interlude.Features.Import

open Percyqaz.Flux.UI
open Prelude.Data.Charts
open Prelude.Data.Charts.Tables
open Interlude.Web.Shared.Requests
open Interlude.UI.Menu

type private DownloadMenuFragment(height: float32) =
    inherit StaticContainer(NodeType.Leaf)

    interface FlowContainerV2.Item with
        member this.Size = height
        member this.OnSizeChanged with set _ = ()

type private SectionHeader(info: TableSectionInfo) =
    inherit DownloadMenuFragment(80.0f)

    override this.Init (parent: Widget) =
        this
        |+ Text(info.Name, Position = Position.TrimBottom(30.0f))
        |* Text(info.Description, Position = Position.SliceBottom(30.0f))
        base.Init parent

type private LevelHeader(level_name: string) =
    inherit DownloadMenuFragment(40.0f)

    override this.Init (parent: Widget) =
        this
        |* Text(level_name)
        base.Init parent

type private Chart(song: Prelude.Backbeat.Archive.Song) =
    inherit DownloadMenuFragment(25.0f)

    override this.Init (parent: Widget) =
        this
        |* Text(song.FormattedTitle)
        base.Init parent

type TableDownloadMenu(table: Table, charts: Tables.Charts.ChartInfo array) =
    inherit Page()

    let charts_by_level = charts |> Array.groupBy (fun x -> x.Level)
    let sections = 
        table.Info.Sections
        |> List.sortBy (fun s -> s.LevelStart)
        |> List.map (fun section -> 
            section,
            charts_by_level
            |> Array.filter (fun (level, _) -> level >= section.LevelStart && level <= section.LevelEnd)
            |> Array.sortBy fst
        )
    
    override this.Init (parent: Widget) =
        let container = FlowContainerV2.Vertical<DownloadMenuFragment>()

        for section_info, section_levels in sections do
            container.Add (SectionHeader section_info)

            for level, level_charts in section_levels do
                container.Add (LevelHeader (table.Info.LevelName level))

                for chart in level_charts do
                    container.Add (Chart chart.Song)

        this.Content(ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 100.0f)))
        base.Init parent

    override this.Title = table.Info.Name
    override this.OnClose() = ()

    //member this.GetMissingCharts() =
    //    status <- InstallingCharts

    //    let on_download_chart () =
    //        missing <- missing - 1

    //        if missing = 0 then
    //            status <- UpToDate
    //            this.RefreshInfo()

    //    let missing_charts =
    //        seq {
    //            for level in existing.Levels do
    //                for chart in level.Charts do
    //                    match Cache.by_key (sprintf "%s/%s" table.Name chart.Hash) Library.cache with
    //                    | Some _ -> ()
    //                    | None -> yield chart.Id, chart.Hash
    //        }

    //    for id, hash in missing_charts do
    //        Charts.Identify.get (
    //            hash,
    //            function
    //            | Some(d: Charts.Identify.Response) ->
    //                match d.Info with
    //                | Some info ->
    //                    Cache.cdn_download_service.Request(
    //                        (table.Name, hash, (info.Chart, info.Song), Library.cache),
    //                        fun _ -> sync on_download_chart
    //                    )
    //                | None ->
    //                    Logging.Info(sprintf "Chart not found: %s(%s)" id hash)
    //                    sync on_download_chart
    //            | _ ->
    //                Logging.Info(sprintf "Chart not found/server error: %s(%s)" id hash)
    //                sync on_download_chart
    //        )