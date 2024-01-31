namespace Interlude.Features.Import

open System.Collections.Generic
open Percyqaz.Flux.UI
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Tables
open Interlude.Web.Shared.Requests
open Interlude.UI
open Interlude.UI.Menu

module private TableDownloader =

    [<RequireQualifiedAccess>]
    type ChartStatus =
        | Missing
        | Queued
        | Downloading
        | Downloaded
        | DownloadFailed
    
    [<RequireQualifiedAccess>]
    type GroupStatus =
        | Downloading
        | SomeMissing
        | AllMissing
        | Downloaded

    type DownloaderState(table: Table, charts: Tables.Charts.ChartInfo array, on_selected_changed) =

        let levels_by_chart = 
            charts
            |> Seq.map (fun c -> c.Hash, c.Level)
            |> Map.ofSeq
        let charts_by_level = 
            charts
            |> Array.groupBy (fun x -> x.Level)
            |> Array.sortBy fst
            |> Map.ofArray
        let sections = 
            table.Info.Sections
            |> List.sortBy (fun s -> s.LevelStart)
            |> List.map (fun section -> 
                section,
                charts_by_level
                |> Map.filter (fun level _ -> level >= section.LevelStart && level <= section.LevelEnd)
            )
        let sections_by_level = 
            sections
            |> Seq.map (fun (info, levels) -> levels |> Map.toSeq |> Seq.map (fun (l, _) -> l, info.Name))
            |> Seq.concat
            |> Map.ofSeq

        let statuses = Dictionary<string, ChartStatus>()
        let level_status = Dictionary<int, GroupStatus>()
        let section_status = Dictionary<string, GroupStatus>()

        let mutable open_level = -1
        let mutable open_section = ""

        do
            for chart in charts do
                statuses.[chart.Hash] <- 
                    if (Cache.by_key (sprintf "%s/%s" table.Info.Name chart.Hash) Library.cache).IsNone then 
                        ChartStatus.Missing
                    else ChartStatus.Downloaded

            for level in charts_by_level.Keys do
                let counts =
                    charts_by_level.[level]
                    |> Seq.map (fun c -> statuses.[c.Hash])
                    |> Seq.countBy id
                    |> fun s -> seq { 
                        yield ChartStatus.Missing, 0
                        yield ChartStatus.Queued, 0
                        yield ChartStatus.Downloading, 0
                        yield ChartStatus.Downloaded, 0
                        yield ChartStatus.DownloadFailed, 0
                        yield! s
                    }
                    |> Map.ofSeq
                level_status.[level] <-
                    if counts.[ChartStatus.Downloaded] > 0 && (counts.[ChartStatus.DownloadFailed] > 0 || counts.[ChartStatus.Missing] > 0) then
                        GroupStatus.SomeMissing
                    elif counts.[ChartStatus.Downloaded] = 0 then
                        GroupStatus.AllMissing
                    else 
                        GroupStatus.Downloaded

            for section, levels in sections do
                let counts =
                    levels
                    |> Map.keys
                    |> Seq.map (fun level -> level_status.[level])
                    |> Seq.countBy id
                    |> fun s -> seq { 
                        yield GroupStatus.SomeMissing, 0
                        yield GroupStatus.AllMissing, 0
                        yield GroupStatus.Downloading, 0
                        yield GroupStatus.Downloaded, 0
                        yield! s
                    }
                    |> Map.ofSeq
                section_status.[section.Name] <-
                    if counts.[GroupStatus.SomeMissing] > 0 then
                        GroupStatus.SomeMissing
                    elif counts.[GroupStatus.Downloaded] > 0 && counts.[GroupStatus.AllMissing] > 0 then
                        GroupStatus.SomeMissing
                    elif counts.[GroupStatus.Downloaded] = 0 then
                        GroupStatus.AllMissing
                    else
                        GroupStatus.Downloaded

        member this.SectionStatus (section: string) = section_status.[section]

        member this.LevelStatus (level: int) = level_status.[level]

        member this.Status (chart: string) = statuses.[chart]

        member this.SetStatus (chart: string, status: ChartStatus) =
            statuses.[chart] <- status

            let level = levels_by_chart.[chart]
            level_status.[level] <-
                let chart_statuses =
                    charts_by_level.[level]
                    |> Seq.map (fun c -> statuses.[c.Hash])
                    |> Seq.countBy id
                    |> Map.ofSeq
                if chart_statuses.[ChartStatus.Downloading] > 0 || chart_statuses.[ChartStatus.Queued] > 0 then
                    GroupStatus.Downloading
                elif chart_statuses.[ChartStatus.Downloaded] > 0 && (chart_statuses.[ChartStatus.DownloadFailed] > 0 || chart_statuses.[ChartStatus.Missing] > 0) then
                    GroupStatus.SomeMissing
                elif chart_statuses.[ChartStatus.Downloaded] = 0 then
                    GroupStatus.AllMissing
                else
                    GroupStatus.Downloaded

            let section = sections_by_level.[level]
            section_status.[section] <-
                let level_statuses =
                    sections |> List.find (fun (x, _) -> x.Name = section)
                    |> snd
                    |> Map.keys
                    |> Seq.map (fun level -> level_status.[level])
                    |> Seq.countBy id
                    |> Map.ofSeq
                if level_statuses.[GroupStatus.Downloading] > 0 then
                    GroupStatus.Downloading
                elif level_statuses.[GroupStatus.SomeMissing] > 0 then
                    GroupStatus.SomeMissing
                elif level_statuses.[GroupStatus.Downloaded] > 0 && level_statuses.[GroupStatus.AllMissing] > 0 then
                    GroupStatus.SomeMissing
                elif level_statuses.[GroupStatus.Downloaded] = 0 then
                    GroupStatus.AllMissing
                else
                    GroupStatus.Downloaded
                    
        member this.OpenLevel 
            with get() = open_level
            and set v = open_level <- v; on_selected_changed()
        member this.OpenSection 
            with get() = open_section
            and set v = open_section <- v; on_selected_changed()

open TableDownloader

[<AbstractClass>]
type private DownloadMenuFragment(height: float32) as this =
    inherit StaticContainer(NodeType.Button (fun () -> this.OnClick()))

    abstract member Visible : bool
    abstract member OnClick : unit -> unit

    interface FlowContainerV2.Item with
        member this.Size = height
        member this.OnSizeChanged with set _ = ()

type private SectionHeader(info: TableSectionInfo, state: DownloaderState) =
    inherit DownloadMenuFragment(120.0f)

    override this.Visible = true
    override this.OnClick() = 
        if state.OpenSection = info.Name then 
            state.OpenSection <- "" 
        else state.OpenSection <- info.Name

    override this.Init (parent: Widget) =
        this
        |+ Text(info.Name, Align = Alignment.LEFT, Position = Position.TrimBottom(50.0f))
        |* Text(info.Description, Align = Alignment.LEFT, Position = Position.SliceBottom(50.0f))
        base.Init parent

type private LevelHeader(section_name: string, level: int, level_name: string, state: DownloaderState) =
    inherit DownloadMenuFragment(50.0f)

    override this.Visible = state.OpenSection = section_name
    override this.OnClick() = 
        if state.OpenLevel = level then state.OpenLevel <- -1
        else state.OpenLevel <- level

    override this.Init (parent: Widget) =
        this
        |* Text(level_name, Align = Alignment.LEFT)
        base.Init parent

type private Chart(chart: Tables.Charts.ChartInfo, state: DownloaderState) =
    inherit DownloadMenuFragment(40.0f)

    override this.Visible = state.OpenLevel = chart.Level
    override this.OnClick() = ()

    override this.Init (parent: Widget) =
        this
        |+ Text(chart.Song.FormattedTitle, Align = Alignment.LEFT)
        |* Text(
            fun () -> 
                match state.Status chart.Hash with
                | ChartStatus.Missing -> Icons.X
                | ChartStatus.Queued -> Icons.REFRESH_CW
                | ChartStatus.Downloading -> Icons.DOWNLOAD
                | ChartStatus.Downloaded -> Icons.CHECK
                | ChartStatus.DownloadFailed -> Icons.X
            ,
            Color = 
                fun () ->
                    match state.Status chart.Hash with
                    | ChartStatus.Missing -> Colors.text_greyout
                    | ChartStatus.Queued -> Colors.text_subheading
                    | ChartStatus.Downloading -> Colors.text_yellow_2
                    | ChartStatus.Downloaded -> Colors.text_green_2
                    | ChartStatus.DownloadFailed -> Colors.text_red_2
            ,
            Align = Alignment.RIGHT)
        base.Init parent

type TableDownloadMenu(table: Table, charts: Tables.Charts.ChartInfo array) =
    inherit Page()

    let container = FlowContainerV2.Vertical<DownloadMenuFragment>()

    let state = DownloaderState(table, charts, fun () -> container.Filter <- _.Visible)

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

        container.Filter <- _.Visible

        for section_info, section_levels in sections do
            container.Add (SectionHeader (section_info, state))

            for level, level_charts in section_levels do
                container.Add (LevelHeader (section_info.Name, level, table.Info.LevelName level, state))

                for chart in level_charts do
                    container.Add (Chart (chart, state))

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