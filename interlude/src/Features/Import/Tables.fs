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
open Interlude.UI

type private TableStatus =
    | NotInstalled
    | UpdateAvailable
    | UpToDate
    | InstallingCharts

[<Json.AutoCodec(false)>]
type ChartIdentity =
    {
        Mirrors: string list
    }
    static member Default = { Mirrors = [] }

type TableCard(id: string, table: Table) as this =
    inherit Frame(NodeType.Button (fun () -> this.Install()),
        Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.pink_accent else Colors.grey_2.O3))
            
    let mutable status = NotInstalled
    let mutable existing = table
    let mutable levels = 0
    let mutable charts = 0
    let mutable missing = 0

    let mutable unavailable_charts = Set.empty

    do
        for t in Table.list() do
            if t.File = id then
                existing <- t.Table
                if t.Table.Version < table.Version then status <- UpdateAvailable else status <- UpToDate

        this
        |+ Text(table.Name,
            Align = Alignment.LEFT,
            Position = Position.SliceTop(60.0f).Margin(10.0f, Style.PADDING))
        |+ Text((fun () -> sprintf "%i levels, %i charts (%i missing)" levels charts missing),
            Align = Alignment.LEFT,
            Position = Position.TrimTop(60.0f).SliceTop(50.0f).Margin(10.0f, Style.PADDING))
        |* Clickable.Focus this

        this.RefreshInfo()

    member this.RefreshInfo() =
        levels <- 0
        charts <- 0
        missing <- 0
        for level in existing.Levels do
            levels <- levels + 1
            for chart in level.Charts do
                charts <- charts + 1
                match Cache.by_hash chart.Hash Library.cache with
                | Some _ -> ()
                | None -> missing <- missing + 1

    member this.GetMissingCharts() =
        status <- InstallingCharts
        match
            seq {
                for level in existing.Levels do
                    for chart in level.Charts do
                        match Cache.by_hash chart.Hash Library.cache with
                        | Some _ -> ()
                        | None -> if not (unavailable_charts.Contains chart.Hash) then yield chart.Hash
            }
            |> Seq.tryHead
        with
        | Some missing ->
            WebServices.download_json("https://api.yavsrg.net/charts/identify?id=" + missing,
                function
                | Some (d: ChartIdentity) when not d.Mirrors.IsEmpty ->
                    let mirror = d.Mirrors.Head
                    Logging.Info("Starting download: " + mirror)
                    let target = Path.Combine(getDataPath "Downloads", System.Guid.NewGuid().ToString() + ".zip")
                    WebServices.download_file.Request((mirror, target, ignore),
                        fun completed ->
                            if completed then Library.Imports.auto_convert.Request((target, true),
                                fun b ->
                                    if b then charts_updated_ev.Trigger()
                                    File.Delete target
                                    sync(this.RefreshInfo)
                                    sync(this.GetMissingCharts)
                            )
                            else 
                                Logging.Info(sprintf "Download failed for %s" missing)
                                sync(this.GetMissingCharts) 
                            unavailable_charts <- unavailable_charts.Add missing
                        )
                | _ -> 
                    Logging.Info(sprintf "Failed to identify chart %s" missing)
                    unavailable_charts <- unavailable_charts.Add missing
                    sync(this.GetMissingCharts)
            )
        | None -> 
            Logging.Info("All charts for this table are installed!") 
            status <- UpToDate

    member this.Install() =
        match status with
        | InstallingCharts -> ()
        | UpToDate -> this.GetMissingCharts()
        | _ -> 
            Table.install(id, table)
            status <- UpToDate
            existing <- table

    override this.Draw() =
        base.Draw()
        Draw.rect (this.Bounds.SliceTop(40.0f).SliceRight(300.0f).Shrink(20.0f, 0.0f)) Colors.shadow_2.O2
        Text.drawFillB(
            Style.font, 
            (
                match status with
                | InstallingCharts -> Icons.download + " Installing charts"
                | NotInstalled -> Icons.download + " Install"
                | UpdateAvailable -> Icons.download + " Update available"
                | UpToDate -> Icons.check + " Installed"
            ),
            this.Bounds.SliceTop(40.0f).SliceRight(300.0f).Shrink(25.0f, Style.PADDING),
            (
                match status with
                | InstallingCharts -> Colors.text_cyan_2
                | NotInstalled -> if this.Focused then Colors.text_yellow_2 else Colors.text
                | UpdateAvailable -> Colors.text_yellow_2
                | UpToDate -> Colors.text_green
            ),
            Alignment.CENTER)

    member this.Name = table.Name
        
module Tables =

    type TableList() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))
    
        let flow = FlowContainer.Vertical<TableCard>(200.0f, Spacing = 15.0f)
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
                                    fun () -> flow.Add(TableCard(entry.File, t))
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