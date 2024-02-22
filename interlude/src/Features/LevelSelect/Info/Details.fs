namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts.Tools.Patterns
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features
open Interlude.Features.Gameplay

[<RequireQualifiedAccess>]
type Display =
    | Local
    | Online
    | Patterns

type Patterns(display: Setting<Display>) =
    inherit StaticContainer(NodeType.None)

    let mutable patterns : Summary.PatternBreakdown list = []

    override this.Init(parent: Widget) =
        base.Init parent

        this
        |* StylishButton(
            (fun () -> display.Set Display.Local),
            K <| Localisation.localise "levelselect.info.details.name",
            !%Palette.MAIN_100,
            Hotkey = "scoreboard_storage",
            TiltLeft = false,
            TiltRight = false,
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 0.0f
                    Right = 1.0f %- 0.0f
                    Bottom = 0.0f %+ 50.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.info.mode", "scoreboard_storage"))

    override this.Draw() =
        base.Draw()

        let mutable b =
            this.Bounds.SliceTop(25.0f).Shrink(10.0f, 0.0f).Translate(0.0f, 50.0f)
        
        for entry in patterns do
            Text.fill_b (
                Style.font,
                (if entry.Mixed then sprintf "Mixed %O (about %i BPM)" entry.Pattern entry.BPM else sprintf "%i BPM %O" entry.BPM entry.Pattern),
                b,
                Colors.text,
                Alignment.CENTER
            )
            Text.fill_b (
                Style.font,
                sprintf "%.0f %.2f %.2f %.2f %.2f %.2f" (entry.Amount / 1000.0f<ms>) entry.Density10 entry.Density25 entry.Density50 entry.Density75 entry.Density90,
                b.Translate(0.0f, 25.0f),
                Colors.text_subheading,
                Alignment.CENTER
            )

            b <- b.Translate(0.0f, 50.0f)

    member this.OnChartUpdated(info: Chart.LoadedChartInfo) =
        patterns <- info.Patterns.Patterns