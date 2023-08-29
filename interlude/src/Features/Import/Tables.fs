namespace Interlude.Features.Import

open System.IO
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Tables
open Prelude.Backbeat.Archive
open Interlude.Web.Shared
open Interlude.UI

type private TableStatus =
    | NotInstalled
    | UpdateAvailable
    | MissingCharts
    | InstallingCharts
    | UpToDate

[<Json.AutoCodec(false)>]
type ChartIdentity =
    {
        Found: bool
        Chart: Chart option
        Song: Song option
        Mirrors: string list
    }
    static member Default = { Found = false; Mirrors = []; Chart = None; Song = None }

type TableCard(id: string, desc: string, table: Table) as this =
    inherit Frame(NodeType.Button (fun () -> Style.click.Play(); this.Install()),
        Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.pink_accent else Colors.grey_2.O3))
            
    let mutable status = NotInstalled
    let mutable existing = table
    let mutable levels = 0
    let mutable charts = 0
    let mutable missing = 0

    do
        for t in Table.list() do
            if t.File = id then
                existing <- t.Table
                if t.Table.Version < table.Version then status <- UpdateAvailable else status <- UpToDate

        this
        |+ Text(table.Name,
            Align = Alignment.CENTER,
            Position = Position.SliceTop(80.0f).Margin(20.0f, Style.PADDING))
        |+ Text(desc,
            Align = Alignment.CENTER,
            Position = Position.TrimTop(65.0f).SliceTop(60.0f).Margin(20.0f, Style.PADDING))
        |+ Text((fun () -> sprintf "%iK, %i levels, %i charts" table.Keymode levels charts),
            Align = Alignment.CENTER,
            Position = Position.TrimTop(130.0f).SliceTop(60.0f).Margin(20.0f, Style.PADDING))
        |* Clickable.Focus this

        this.RefreshInfo()

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    member this.RefreshInfo() =
        levels <- 0
        charts <- 0
        missing <- 0
        for level in existing.Levels do
            levels <- levels + 1
            for chart in level.Charts do
                charts <- charts + 1
                match Cache.by_key (sprintf "%s/%s" table.Name chart.Hash) Library.cache with
                | Some _ -> ()
                | None -> missing <- missing + 1
        if missing > 0 && status = UpToDate then status <- MissingCharts

    member this.GetMissingCharts() =
        status <- InstallingCharts

        let on_download_chart() =
            missing <- missing - 1
            if missing = 0 then 
                status <- UpToDate
                this.RefreshInfo()

        let missing_charts =
            seq {
                for level in existing.Levels do
                    for chart in level.Charts do
                        match Cache.by_key (sprintf "%s/%s" table.Name chart.Hash) Library.cache with
                        | Some _ -> ()
                        | None -> yield chart.Id, chart.Hash
            }
        for id, hash in missing_charts do
            API.Client.get("charts/identify?chart=" + hash,
                function
                | Some (d: Requests.Charts.Identify.Response) ->
                    match d.Info with
                    | Some info -> Cache.cdn_download_service.Request((table.Name, hash, (info.Chart, info.Song), Library.cache), fun _ -> sync on_download_chart)
                    | None -> 
                        Logging.Info(sprintf "Chart not found: %s(%s)" id hash)
                        sync on_download_chart
                | _ -> 
                    Logging.Info(sprintf "Chart not found/server error: %s(%s)" id hash)
                    sync on_download_chart
            )

    member this.Install() =
        match status with
        | NotInstalled 
        | UpdateAvailable ->
            Table.install(id, table)
            existing <- table
            status <- UpToDate
            this.RefreshInfo()
        | InstallingCharts -> ()
        | MissingCharts -> this.GetMissingCharts()
        | UpToDate -> ()

    override this.Draw() =
        base.Draw()
        let button_bounds = this.Bounds.SliceBottom(70.0f).Shrink(20.0f, 10.0f)
        Draw.rect button_bounds Colors.shadow_2.O2
        Text.drawFillB(
            Style.font, 
            (
                match status with
                | NotInstalled -> Icons.download + " Install"
                | UpdateAvailable -> Icons.download + " Update available"
                | MissingCharts -> sprintf "%s Download missing charts (%i)" Icons.download missing
                | InstallingCharts -> sprintf "%s Installing missing charts (%i)" Icons.download missing
                | UpToDate -> Icons.check + " Up to date!"
            ),
            button_bounds.Shrink Style.PADDING,
            (
                match status with
                | InstallingCharts -> Colors.text_cyan_2
                | NotInstalled -> if this.Focused then Colors.text_yellow_2 else Colors.text
                | MissingCharts -> if this.Focused then Colors.text_yellow_2 else Colors.text_green_2
                | UpdateAvailable -> Colors.text_yellow_2
                | UpToDate -> Colors.text_green
            ),
            Alignment.CENTER)

    member this.Name = table.Name
        
module Tables =

    type TableList() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))
    
        let flow = FlowContainer.Vertical<TableCard>(260.0f, Spacing = 15.0f)
        let scroll = ScrollContainer.Flow(flow, Margin = Style.PADDING)
    
        override this.Init(parent) =
            WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Backbeat/main/tables/index.json",
                fun data ->
                match data with
                | Some (d: TableIndex) -> 
                    for (entry: TableIndexEntry) in d.Tables do
                        WebServices.download_json("https://raw.githubusercontent.com/YAVSRG/Backbeat/main/tables/" + entry.File, 
                            fun table ->
                            match table with
                            | Some (t: Table) ->
                                sync (
                                    fun () -> flow.Add(TableCard(entry.File, entry.Description, t))
                                )
                            | None -> Logging.Error(sprintf "Error getting table %s" entry.File)
                        )
                | None -> Logging.Error("Error getting table index")
            )
            this
            |* scroll
            base.Init parent

        override this.Focusable = flow.Focusable 
    
        member this.Items = flow

    let tab = 
        TableList()