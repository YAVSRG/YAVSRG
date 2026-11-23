namespace Interlude.Features.Tables.Browser

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude.Backbeat
open Prelude.Data.Imports
open Interlude.Web.Shared.Requests
open Interlude.Content

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

        let levels_by_chart = charts |> Seq.map(fun c -> c.Hash, c.Level) |> Map.ofSeq

        let charts_by_level =
            charts |> Array.groupBy(fun x -> x.Level) |> Array.sortBy fst |> Map.ofArray

        let sections =
            table.Info.Sections
            |> List.sortBy(fun s -> s.LevelStart)
            |> List.map(fun section ->
                section,
                charts_by_level |> Map.filter(fun level _ -> level >= section.LevelStart && level <= section.LevelEnd)
            )

        let sections_by_level =
            sections
            |> Seq.map(fun (info, levels) -> levels |> Map.toSeq |> Seq.map(fun (l, _) -> l, info.Name))
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
                    match Content.Charts.GetChartMeta(chart.Hash) with
                    | Some chart_meta when chart_meta.Packs.Contains(table.Info.Name) -> ChartStatus.Downloaded
                    | _ -> ChartStatus.Missing

            for level in charts_by_level.Keys do
                let counts =
                    charts_by_level.[level]
                    |> Seq.map(fun c -> statuses.[c.Hash])
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
                    |> Seq.map(fun level -> level_status.[level])
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
                    if counts.[GroupStatus.SomeMissing] > 0 then GroupStatus.SomeMissing
                    elif counts.[GroupStatus.Downloaded] > 0 && counts.[GroupStatus.AllMissing] > 0 then GroupStatus.SomeMissing
                    elif counts.[GroupStatus.Downloaded] = 0 then GroupStatus.AllMissing
                    else GroupStatus.Downloaded

        member this.Charts = charts

        member this.SectionStatus(section: string) : GroupStatus = section_status.[section]

        member this.LevelStatus(level: int) : GroupStatus = level_status.[level]

        member this.Status(chart: string) : ChartStatus = statuses.[chart]

        member this.SetStatus(chart: string, status: ChartStatus) : unit =
            statuses.[chart] <- status

            let level = levels_by_chart.[chart]

            level_status.[level] <-
                let chart_statuses =
                    charts_by_level.[level]
                    |> Seq.map(fun c -> statuses.[c.Hash])
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

                if chart_statuses.[ChartStatus.Downloading] > 0 || chart_statuses.[ChartStatus.Queued] > 0 then
                    GroupStatus.Downloading
                elif
                    chart_statuses.[ChartStatus.Downloaded] > 0
                    && (chart_statuses.[ChartStatus.DownloadFailed] > 0 || chart_statuses.[ChartStatus.Missing] > 0)
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
                    |> List.find(fun (x, _) -> x.Name = section)
                    |> snd
                    |> Map.keys
                    |> Seq.map(fun level -> level_status.[level])
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
                elif level_statuses.[GroupStatus.Downloaded] > 0 && level_statuses.[GroupStatus.AllMissing] > 0 then
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

        member this.QueueSection(section: string) : unit =
            for chart in sections |> List.find(fun (info, _) -> info.Name = section) |> snd |> Map.values |> Seq.concat do
                match this.Status(chart.Hash) with
                | ChartStatus.DownloadFailed
                | ChartStatus.Missing ->
                    this.SetStatus(chart.Hash, ChartStatus.Queued)
                    download_service.Request((this, table.Info.Name, chart), ignore)
                | _ -> ()

        member this.QueueLevel(level: int) : unit =
            for chart in charts_by_level.[level] do
                match this.Status(chart.Hash) with
                | ChartStatus.DownloadFailed
                | ChartStatus.Missing ->
                    this.SetStatus(chart.Hash, ChartStatus.Queued)
                    download_service.Request((this, table.Info.Name, chart), ignore)
                | _ -> ()

    let download_service =
        { new Async.Queue<DownloaderState * string * Tables.Charts.ChartInfo, unit>() with
            override _.Handle((state: DownloaderState, table_name: string, chart: Tables.Charts.ChartInfo)) =
                async {
                    GameThread.defer(fun () -> state.SetStatus(chart.Hash, ChartStatus.Downloading))

                    match Content.Charts.GetChartMeta(chart.Hash) with
                    | Some chart_meta ->
                        Content.Charts.AddToPack(chart_meta, table_name)
                        GameThread.defer(fun () -> state.SetStatus(chart.Hash, ChartStatus.Downloaded))
                    | None ->
                        match!
                            OnlineImports.cdn_install(table_name, chart.Hash, chart.Chart, chart.Song, Content.Charts)
                        with
                        | Ok() -> GameThread.defer(fun () -> state.SetStatus(chart.Hash, ChartStatus.Downloaded))
                        | Error reason ->
                            Logging.Error "Error downloading '%s' from CDN: %s" chart.Hash reason
                            GameThread.defer(fun () -> state.SetStatus(chart.Hash, ChartStatus.DownloadFailed))

                    return ()
                }
        }

    let mutable existing_states: Map<string, DownloaderState> = Map.empty
