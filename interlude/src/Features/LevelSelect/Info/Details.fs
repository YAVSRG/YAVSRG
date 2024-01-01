namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features

[<RequireQualifiedAccess>]
type Display =
    | Local
    | Online
    | Details

type Details(display: Setting<Display>) =
    inherit StaticContainer(NodeType.None)

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
            this.Bounds.SliceTop(40.0f).Shrink(10.0f, 0.0f).Translate(0.0f, 50.0f)

        match Gameplay.Chart.PATTERNS with
        | None -> ()
        | Some p ->
            for entry in p.Patterns do
                Text.fill_b (
                    Style.font,
                    sprintf "%i BPM %O" entry.BPM entry.Pattern,
                    b,
                    Colors.text_subheading,
                    Alignment.CENTER
                )

                b <- b.Translate(0.0f, 45.0f)
