namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Data
open Prelude.Data.Charts.Tables
open Interlude.UI

type private TableStatus =
    | NotInstalled
    | UpdateAvailable
    | UpToDate

type TableCard(id: string, table: Table) as this =
    inherit Frame(NodeType.Button (fun () -> this.Install()),
        Fill = (fun () -> if this.Focused then Colors.pink.O2 else Colors.shadow_2.O2),
        Border = (fun () -> if this.Focused then Colors.pink_accent else Colors.grey_2.O3))
            
    let mutable status = NotInstalled

    do
        for t in Table.list() do
            if t.File = id then
                if t.Table.Version < table.Version then status <- UpdateAvailable else status <- UpToDate

        this
        |+ Text(table.Name,
            Align = Alignment.LEFT,
            Position = Position.SliceTop(50.0f).Margin(10.0f, Style.padding))
        |* Clickable.Focus this

    member this.Install() =
        match status with
        | UpToDate -> ()
        | _ -> Table.install(id, table); status <- UpToDate

    override this.Draw() =
        base.Draw()
        Draw.rect (this.Bounds.SliceTop(40.0f).SliceRight(300.0f).Shrink(20.0f, 0.0f)) Colors.shadow_2.O2
        Text.drawFillB(
            Style.baseFont, 
            (
                match status with
                | NotInstalled -> Icons.download + " Install"
                | UpdateAvailable -> Icons.download + " Update available"
                | UpToDate -> Icons.check + " Installed"
            ),
            this.Bounds.SliceTop(40.0f).SliceRight(300.0f).Shrink(25.0f, Style.padding),
            (
                match status with
                | NotInstalled -> if this.Focused then Colors.text_yellow_2 else Colors.text
                | UpdateAvailable -> Colors.text_yellow_2
                | UpToDate -> Colors.text_green
            ),
            Alignment.CENTER)

    member this.Name = table.Name
        
module Tables =

    type RulesetSearch() as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))
    
        let flow = FlowContainer.Vertical<TableCard>(200.0f, Spacing = 15.0f)
        let scroll = ScrollContainer.Flow(flow, Margin = Style.padding)
    
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
    
        member this.Items = flow

    let tab = 
        RulesetSearch()