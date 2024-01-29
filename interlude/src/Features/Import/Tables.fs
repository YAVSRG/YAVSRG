namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Data
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Tables
open Interlude.Web.Shared.Requests
open Interlude.UI

type TableCard(online_table: Tables.List.Table) as this =
    inherit
        Frame(
            NodeType.Button(fun () ->
                Style.click.Play()
                //this.Install()
            ),
            Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
            Border =
                (fun () ->
                    if this.Focused then
                        Colors.pink_accent
                    else
                        Colors.grey_2.O3
                )
        )

    let mutable existing = None

    override this.Init (parent: Widget) =
        this
        |+ Text(online_table.Info.Name, Align = Alignment.CENTER, Position = Position.SliceTop(80.0f).Margin(20.0f, Style.PADDING))
        |+ Text(
            online_table.Info.Description,
            Align = Alignment.CENTER,
            Position = Position.TrimTop(65.0f).SliceTop(60.0f).Margin(20.0f, Style.PADDING)
        )
        |* Clickable.Focus this

        existing <- Interlude.Content.Tables.by_id online_table.Id

        base.Init parent

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

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

    //member this.Install() =
    //    match status with
    //    | NotInstalled
    //    | UpdateAvailable ->
    //        Table.install (id, table)
    //        Interlude.Options.options.Table.Value <- Some table.Name
    //        existing <- table
    //        status <- UpToDate
    //        this.RefreshInfo()
    //    | InstallingCharts -> ()
    //    | MissingCharts -> this.GetMissingCharts()
    //    | UpToDate -> ()

module Tables =

    type TableList() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))

        let flow = FlowContainer.Vertical<TableCard>(260.0f, Spacing = 15.0f)
        let scroll = ScrollContainer.Flow(flow, Margin = Style.PADDING)

        override this.Init(parent) =
            Tables.List.get (
                function
                | Some tables ->
                    for table in tables.Tables do
                        sync (fun () -> flow.Add(TableCard(table)))
                | None -> Logging.Error("Error getting online tables list")
            )
            this |* scroll
            base.Init parent

        override this.Focusable = flow.Focusable

        member this.Items = flow

    let tab = TableList()
