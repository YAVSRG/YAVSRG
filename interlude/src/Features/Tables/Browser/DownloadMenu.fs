namespace Interlude.Features.Tables.Browser

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Backbeat
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Interlude.Web.Shared.Requests
open Interlude.Content
open Interlude.UI

#nowarn "40"

module TableDownloader =

    [<RequireQualifiedAccess>]
    type ChartStatus =
        | Missing
        | Queued
        | Downloading
        | Downloaded
        | DownloadFailed

    [<RequireQualifiedAccess>]
    type GroupStatus =
        | AllMissing
        | SomeMissing
        | Downloading
        | Downloaded

    type DownloaderState
        (
            table: Table,
            charts: Tables.Charts.ChartInfo array,
            download_service: Async.Queue<DownloaderState * string * Tables.Charts.ChartInfo, unit>
        ) =

        let levels_by_chart = charts |> Seq.map (fun c -> c.Hash, c.Level) |> Map.ofSeq

        let charts_by_level =
            charts |> Array.groupBy (fun x -> x.Level) |> Array.sortBy fst |> Map.ofArray

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
                    match ChartDatabase.get_meta chart.Hash Content.Charts with
                    | Some chart_meta when chart_meta.Packs.Contains table.Info.Name -> ChartStatus.Downloaded
                    | _ -> ChartStatus.Missing

            for level in charts_by_level.Keys do
                let counts =
                    charts_by_level.[level]
                    |> Seq.map (fun c -> statuses.[c.Hash])
                    |> Seq.countBy id
                    |> fun s ->
                        seq {
                            yield ChartStatus.Missing, 0
                            yield ChartStatus.Queued, 0
                            yield ChartStatus.Downloading, 0
                            yield ChartStatus.Downloaded, 0
                            yield ChartStatus.DownloadFailed, 0
                            yield! s
                        }
                    |> Map.ofSeq

                level_status.[level] <-
                    if
                        counts.[ChartStatus.Downloaded] > 0
                        && (counts.[ChartStatus.DownloadFailed] > 0 || counts.[ChartStatus.Missing] > 0)
                    then
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
                    |> fun s ->
                        seq {
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

        member this.Charts = charts

        member this.SectionStatus(section: string) = section_status.[section]

        member this.LevelStatus(level: int) = level_status.[level]

        member this.Status(chart: string) = statuses.[chart]

        member this.SetStatus(chart: string, status: ChartStatus) =
            statuses.[chart] <- status

            let level = levels_by_chart.[chart]

            level_status.[level] <-
                let chart_statuses =
                    charts_by_level.[level]
                    |> Seq.map (fun c -> statuses.[c.Hash])
                    |> Seq.countBy id
                    |> fun s ->
                        seq {
                            yield ChartStatus.Missing, 0
                            yield ChartStatus.Queued, 0
                            yield ChartStatus.Downloading, 0
                            yield ChartStatus.Downloaded, 0
                            yield ChartStatus.DownloadFailed, 0
                            yield! s
                        }
                    |> Map.ofSeq

                if
                    chart_statuses.[ChartStatus.Downloading] > 0
                    || chart_statuses.[ChartStatus.Queued] > 0
                then
                    GroupStatus.Downloading
                elif
                    chart_statuses.[ChartStatus.Downloaded] > 0
                    && (chart_statuses.[ChartStatus.DownloadFailed] > 0
                        || chart_statuses.[ChartStatus.Missing] > 0)
                then
                    GroupStatus.SomeMissing
                elif chart_statuses.[ChartStatus.Downloaded] = 0 then
                    GroupStatus.AllMissing
                else
                    GroupStatus.Downloaded

            let section = sections_by_level.[level]

            section_status.[section] <-
                let level_statuses =
                    sections
                    |> List.find (fun (x, _) -> x.Name = section)
                    |> snd
                    |> Map.keys
                    |> Seq.map (fun level -> level_status.[level])
                    |> Seq.countBy id
                    |> fun s ->
                        seq {
                            yield GroupStatus.SomeMissing, 0
                            yield GroupStatus.AllMissing, 0
                            yield GroupStatus.Downloading, 0
                            yield GroupStatus.Downloaded, 0
                            yield! s
                        }
                    |> Map.ofSeq

                if level_statuses.[GroupStatus.Downloading] > 0 then
                    GroupStatus.Downloading
                elif level_statuses.[GroupStatus.SomeMissing] > 0 then
                    GroupStatus.SomeMissing
                elif
                    level_statuses.[GroupStatus.Downloaded] > 0
                    && level_statuses.[GroupStatus.AllMissing] > 0
                then
                    GroupStatus.SomeMissing
                elif level_statuses.[GroupStatus.Downloaded] = 0 then
                    GroupStatus.AllMissing
                else
                    GroupStatus.Downloaded

        member val OnSelectedChanged = ignore with get, set

        member this.OpenLevel
            with get () = open_level
            and set v =
                open_level <- v
                this.OnSelectedChanged()

        member this.OpenSection
            with get () = open_section
            and set v =
                open_level <- -1
                open_section <- v
                this.OnSelectedChanged()

        member this.QueueSection(section: string) =
            for chart in
                sections
                |> List.find (fun (info, _) -> info.Name = section)
                |> snd
                |> Map.values
                |> Seq.concat do
                match this.Status chart.Hash with
                | ChartStatus.DownloadFailed
                | ChartStatus.Missing ->
                    this.SetStatus(chart.Hash, ChartStatus.Queued)
                    download_service.Request((this, table.Info.Name, chart), ignore)
                | _ -> ()

        member this.QueueLevel(level: int) =
            for chart in charts_by_level.[level] do
                match this.Status chart.Hash with
                | ChartStatus.DownloadFailed
                | ChartStatus.Missing ->
                    this.SetStatus(chart.Hash, ChartStatus.Queued)
                    download_service.Request((this, table.Info.Name, chart), ignore)
                | _ -> ()

    let download_service =
        { new Async.Queue<DownloaderState * string * Tables.Charts.ChartInfo, unit>() with
            override _.Handle((state: DownloaderState, table_name: string, chart: Tables.Charts.ChartInfo)) =
                async {
                    GameThread.defer (fun () -> state.SetStatus(chart.Hash, ChartStatus.Downloading))

                    match ChartDatabase.get_meta chart.Hash Content.Charts with
                    | Some chart_meta ->
                        ChartDatabase.change_packs chart_meta (chart_meta.Packs.Add table_name) Content.Charts
                        GameThread.defer (fun () -> state.SetStatus(chart.Hash, ChartStatus.Downloaded))
                    | None ->
                        match! OnlineImports.cdn_install (table_name, chart.Hash, chart.Chart, chart.Song, Content.Charts) with
                        | Ok() -> GameThread.defer (fun () -> state.SetStatus(chart.Hash, ChartStatus.Downloaded))
                        | Error reason ->
                            Logging.Error "Error downloading '%s' from CDN: %s" chart.Hash reason
                            GameThread.defer (fun () -> state.SetStatus(chart.Hash, ChartStatus.DownloadFailed))

                    return ()
                }
        }

    let mutable existing_states: Map<string, DownloaderState> = Map.empty

open TableDownloader

[<AbstractClass>]
type private DownloadMenuFragment(nt: NodeType, height: float32) =
    inherit Container(nt)

    abstract member Visible: bool

    interface IHeight with
        member this.Height = height

type private Chart(chart: Tables.Charts.ChartInfo, state: DownloaderState) =
    inherit DownloadMenuFragment(NodeType.None, 40.0f)

    override this.Visible = state.OpenLevel = chart.Level

    override this.Init(parent: Widget) =
        this
        |+ Frame(Fill = K Colors.shadow_2.O2, Border = K Colors.shadow_2.O2)
        |+ Text(chart.Song.FormattedTitle)
            .Align(Alignment.LEFT)
            .Position(Position.Shrink(5.0f, 0.0f))
        |* Text(fun () ->
            match state.Status chart.Hash with
            | ChartStatus.Missing -> Icons.X
            | ChartStatus.Queued -> Icons.REFRESH_CW
            | ChartStatus.Downloading -> Icons.DOWNLOAD
            | ChartStatus.Downloaded -> Icons.CHECK
            | ChartStatus.DownloadFailed -> Icons.X
        )
            .Color(fun () ->
                match state.Status chart.Hash with
                | ChartStatus.Missing -> Colors.text_greyout
                | ChartStatus.Queued -> Colors.text_subheading
                | ChartStatus.Downloading -> Colors.text_yellow_2
                | ChartStatus.Downloaded -> Colors.text_green_2
                | ChartStatus.DownloadFailed -> Colors.text_red_2
            )
            .Position(Position.Shrink(5.0f, 0.0f))
            .Align(Alignment.RIGHT)

        base.Init parent

type private LevelHeader(section: TableSectionInfo, level: int, level_name: string, state: DownloaderState) as this =
    inherit
        DownloadMenuFragment(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.OnClick()
            ),
            50.0f
        )

    override this.Visible = state.OpenSection = section.Name

    member this.OnClick() =
        if state.OpenLevel = level then
            state.OpenLevel <- -1
        else
            state.OpenLevel <- level

    member this.Button =
        Button(
            (fun () ->
                match state.LevelStatus level with
                | GroupStatus.AllMissing -> Icons.DOWNLOAD + " Download"
                | GroupStatus.SomeMissing -> Icons.DOWNLOAD + " Update"
                | GroupStatus.Downloading -> ""
                | GroupStatus.Downloaded -> ""
            ),
            (fun () -> state.QueueLevel level)
        )
            .Position(Position.ShrinkR(160.0f).SliceR(200.0f).Shrink(20.0f, 5.0f))

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Init(parent: Widget) =
        this
        |+ Frame(Fill = (K <| Color.FromArgb(section.Color).O1), Border = (K <| Color.FromArgb(section.Color).O1))
        |+ Text(level_name)
            .Align(Alignment.LEFT)
            .Color(fun () -> if this.Focused then Colors.text_yellow_2 else Colors.text)
            .Position(Position.Shrink(5.0f, 0.0f))
        |+ Text(fun () ->
            match state.LevelStatus level with
            | GroupStatus.AllMissing -> Icons.X
            | GroupStatus.SomeMissing -> Icons.X
            | GroupStatus.Downloading -> Icons.REFRESH_CW
            | GroupStatus.Downloaded -> Icons.CHECK
        )
            .Color(fun () ->
                match state.LevelStatus level with
                | GroupStatus.AllMissing -> Colors.text_greyout
                | GroupStatus.SomeMissing -> Colors.text_yellow_2
                | GroupStatus.Downloading -> Colors.text_yellow_2
                | GroupStatus.Downloaded -> Colors.text_green_2
            )
            .Position(Position.Shrink(85.0f, 0.0f))
            .Align(Alignment.RIGHT)
        |+ Text(
            (fun () ->
                if state.OpenLevel = level then
                    Icons.CHEVRON_UP
                else
                    Icons.CHEVRON_DOWN
            ))
            .Color(Colors.text)
            .Align(Alignment.RIGHT)
            .Position(Position.Shrink(5.0f, 0.0f))
        |+ MouseListener().Button(this)
        |* this.Button

        base.Init parent

type private SectionHeader(info: TableSectionInfo, state: DownloaderState) as this =
    inherit
        DownloadMenuFragment(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.OnClick()
            ),
            120.0f
        )

    override this.Visible = true

    member this.OnClick() =
        if state.OpenSection = info.Name then
            state.OpenSection <- ""
        else
            state.OpenSection <- info.Name

    member this.Button =
        Button(
            fun () ->
                match state.SectionStatus info.Name with
                | GroupStatus.AllMissing -> Icons.DOWNLOAD + " Download"
                | GroupStatus.SomeMissing -> Icons.DOWNLOAD + " Update"
                | GroupStatus.Downloading -> ""
                | GroupStatus.Downloaded -> ""
            , (fun () -> state.QueueSection info.Name)
        )
            .Position(Position.ShrinkR(100.0f).SliceR(200.0f).Shrink(20.0f, 20.0f))

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Init(parent: Widget) =
        this
        |+ Frame(Fill = (K <| Color.FromArgb(info.Color).O2), Border = (K <| Color.FromArgb(info.Color).O3))
        |+ Text(info.Name)
            .Color(fun () -> if this.Focused then Colors.text_yellow_2 else Colors.text)
            .Align(Alignment.LEFT)
            .Position(Position.Shrink(5.0f).ShrinkB(50.0f))
        |+ Text(info.Description)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Position(Position.Shrink(5.0f).SliceB(50.0f))
        |+ Text(
            (fun () ->
                if state.OpenSection = info.Name then
                    Icons.CHEVRON_UP
                else
                    Icons.CHEVRON_DOWN
            ))
            .Color(Colors.text)
            .Align(Alignment.RIGHT)
            .Position(Position.Shrink(25.0f, 20.0f))
        |+ MouseListener().Button(this)
        |* this.Button

        base.Init parent

type private TableDownloadMenu(table: Table, state: DownloaderState) =
    inherit Page()

    let container = DynamicFlowContainer.Vertical<DownloadMenuFragment>(Spacing = 10.0f)

    let charts_by_level = state.Charts |> Array.groupBy (fun x -> x.Level)

    let sections =
        table.Info.Sections
        |> List.sortBy (fun s -> s.LevelStart)
        |> List.map (fun section ->
            section,
            charts_by_level
            |> Array.filter (fun (level, _) -> level >= section.LevelStart && level <= section.LevelEnd)
            |> Array.sortBy fst
        )

    override this.Content() =
        container.Filter <- _.Visible
        state.OnSelectedChanged <- fun () -> container.Filter <- _.Visible

        for section_info, section_levels in sections do
            container.Add(SectionHeader(section_info, state))

            for level, level_charts in section_levels do
                container.Add(LevelHeader(section_info, level, table.Info.LevelName level, state))

                for chart in level_charts do
                    container.Add(Chart(chart, state))

        ScrollContainer(container)
            .Margin(Style.PADDING * 2.0f)
            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))

    override this.Title = table.Info.Name
    override this.OnClose() = ()

    static member LoadOrOpen(table: Table) =
        match existing_states.TryFind table.Id with
        | Some state -> TableDownloadMenu(table, state).Show()
        | None ->
            Tables.Charts.get (
                table.Id,
                function
                | Some charts ->
                    let state = DownloaderState(table, charts.Charts, download_service)

                    GameThread.defer (fun () ->
                        match existing_states.TryFind table.Id with
                        | Some state -> () // do nothing if they spam clicked the table button
                        | None ->
                            existing_states <- Map.add table.Id state existing_states
                            TableDownloadMenu(table, state).Show()
                    )
                | None -> Logging.Error("Error getting charts for table")
            // error toast
            )

    static member OpenAfterInstall(table: Table, charts: Tables.Charts.Response) =
        let state = DownloaderState(table, charts.Charts, download_service)
        existing_states <- Map.add table.Id state existing_states
        TableDownloadMenu(table, state).Show()