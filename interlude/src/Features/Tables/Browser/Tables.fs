namespace Interlude.Features.Tables.Browser

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Backbeat
open Interlude.Web.Shared.Requests
open Interlude.Content
open Interlude.UI

[<RequireQualifiedAccess>]
type private TableStatus =
    | NotInstalled
    | OutOfDate
    | Installing
    | Installed

type TableCard(online_table: Tables.List.Table) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                this.Install()
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
    let mutable status = TableStatus.NotInstalled

    override this.Init(parent: Widget) =
        this
        |+ Text(online_table.Info.Name)
            .Align(Alignment.CENTER)
            .Position(Position.SliceT(80.0f).Shrink(20.0f, Style.PADDING))
        |+ Text(online_table.Info.Description)
            .Align(Alignment.CENTER)
            .Position(Position.ShrinkT(65.0f).SliceT(60.0f).Shrink(20.0f, Style.PADDING))
        |+ Text(fun () ->
            match status with
            | TableStatus.NotInstalled -> "Click to install"
            | TableStatus.OutOfDate -> "Click to update"
            | TableStatus.Installing -> "Installing ..."
            | TableStatus.Installed -> "Click to view"
        )
            .Align(Alignment.CENTER)
            .Position(Position.SliceB(60.0f).Shrink(20.0f, Style.PADDING))
        |+ LoadingIndicator.Border(fun () -> status = TableStatus.Installing)
        |* MouseListener().Button(this)

        existing <- Tables.by_id online_table.Id

        status <-
            match existing with
            | Some table when table.LastUpdated < online_table.LastUpdated -> TableStatus.OutOfDate
            | Some _ -> TableStatus.Installed
            | None -> TableStatus.NotInstalled

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    member this.Install() =
        match status with
        | TableStatus.NotInstalled
        | TableStatus.OutOfDate as current_status ->
            status <- TableStatus.Installing

            Tables.Charts.get (
                online_table.Id,
                function
                | Some charts ->
                    GameThread.defer (fun () ->
                        let table =
                            {
                                Id = online_table.Id
                                Info = online_table.Info
                                LastUpdated = online_table.LastUpdated
                                Charts =
                                    charts.Charts
                                    |> Array.map (fun c -> { Hash = c.Hash; Level = c.Level })
                                    |> List.ofArray
                            }

                        Tables.install_or_update table
                        Tables.selected_id.Value <- Some table.Id
                        status <- TableStatus.Installed
                        existing <- Some table
                        TableDownloadMenu.OpenAfterInstall(table, charts)
                    )
                | None ->
                    Logging.Error("Error getting charts for table")
                    // error toast
                    GameThread.defer (fun () -> status <- current_status)
            )
        | TableStatus.Installing -> ()
        | TableStatus.Installed ->
            Tables.selected_id.Value <- Some existing.Value.Id
            TableDownloadMenu.LoadOrOpen(existing.Value)

type TableBrowserPage() =
    inherit Page()

    let flow = FlowContainer.Vertical<TableCard>(200.0f).Spacing(Style.PADDING * 3.0f)
    let scroll =
        ScrollContainer(flow, Margin = Style.PADDING)
            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))

    override this.Content() =
        Tables.List.get (
                function
                | Some tables ->
                    for table in tables.Tables do
                        GameThread.defer (fun () -> flow.Add(TableCard(table)))
                | None -> Logging.Error("Error getting online tables list")
            )

        NavigationContainer.Column()
        |+ Dummy(NodeType.Leaf)
        |+ scroll
        :> Widget

    override this.Title = sprintf "%s %s" Icons.SIDEBAR (%"tables.browser")