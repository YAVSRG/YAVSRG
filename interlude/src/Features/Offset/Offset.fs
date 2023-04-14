namespace Interlude.Features.Offset

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

type private TileButton(body: Callout, onclick: unit -> unit, is_selected: unit -> bool) =
    inherit StaticContainer(NodeType.Button (onclick))

    let body_height = snd <| Callout.measure body

    member val Disabled = false with get, set
    member val Margin = (0.0f, 20.0f) with get, set
    member this.Height = body_height + snd this.Margin * 2.0f

    override this.Init(parent) =
        this |* Clickable.Focus(this)
        base.Init(parent)

    override this.Draw() =
        let color, dark = 
            if this.Disabled then Colors.shadow_1, false
            elif is_selected() then Colors.yellow_accent, true
            elif this.Focused then Colors.pink_accent, false
            else Colors.shadow_1, false
        Draw.rect this.Bounds (Color.FromArgb(180, color))
        Draw.rect (this.Bounds.Expand(0.0f, 5.0f).SliceBottom(5.0f)) color
        Callout.draw (this.Bounds.Left + fst this.Margin, this.Bounds.Top + snd this.Margin, body_height, Colors.text, body)

type OffsetPage(chart: Chart) as this =
    inherit Page()

    let mutable tab = 0

    do 
        let global_offset_tile = 
            TileButton(
                Callout.Small.Body("Compensate for hardware delay\nUse this if all songs are offsync").Title("Hardware sync").Icon(Icons.connected),
                (fun () -> tab <- 1),
                fun () -> tab = 1
            )
            
        let local_offset_tile = 
            TileButton(
                Callout.Small.Body("Synchronise this chart\nUse this if the chart's timing is off").Title("Chart sync").Icon(Icons.reset),
                (fun () -> tab <- 2),
                fun () -> tab = 2
            )
        
        let visual_offset_tile = 
            TileButton(
                Callout.Small.Body("Adjust your scroll speed or hitposition\nUse this if your timing is off").Title("Visual sync").Icon(Icons.preview),
                (fun () -> VisualSyncPart1().Show()),
                fun () -> tab = 3
            )

        let height = max (max global_offset_tile.Height local_offset_tile.Height) visual_offset_tile.Height

        this.Content(
            StaticContainer(NodeType.Leaf)
            |+ (
                GridContainer(1, 3, Spacing = (100.0f, 0.0f), Position = Position.Row(60.0f, height).Margin(150.0f, 0.0f))
                |+ global_offset_tile
                |+ local_offset_tile
                |+ visual_offset_tile
            )
            |+ Conditional((fun () -> tab = 1), GlobalSync(chart, fun s -> tab <- 0), Position = Position.TrimTop(60.0f + height))
            |+ Conditional((fun () -> tab = 2), AudioSync(chart, fun () -> tab <- 0), Position = Position.TrimTop(60.0f + height))
        )

    override this.Title = L"offset.name"
    override this.OnClose() = ()