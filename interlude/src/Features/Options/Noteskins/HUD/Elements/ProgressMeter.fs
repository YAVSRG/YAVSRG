namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type ProgressMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.ProgressMeterPosition

    let color = Setting.simple noteskin_options.ProgressMeterColor
    let background_color = Setting.simple noteskin_options.ProgressMeterBackgroundColor
    let label = Setting.simple user_options.ProgressMeterLabel

    let preview =
        { new ConfigPreviewNew(pos.Value) with
            override this.DrawComponent(bounds) =
                let x, y = bounds.Center
                let r = (min bounds.Width bounds.Height) * 0.5f
                let angle = System.MathF.PI / 15.0f

                let outer i =
                    let angle = float32 i * angle
                    let struct (a, b) = System.MathF.SinCos(angle)
                    (x + r * a, y - r * b)

                let inner i =
                    let angle = float32 i * angle
                    let struct (a, b) = System.MathF.SinCos(angle)
                    (x + (r - 2f) * a, y - (r - 2f) * b)

                for i = 0 to 29 do
                    Draw.untextured_quad
                        (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                        (Quad.color background_color.Value)

                    Draw.untextured_quad
                        (Quad.createv (inner i) (outer i) (outer (i + 1)) (inner (i + 1)))
                        (Quad.color Colors.white.O2)

                for i = 0 to 17 do
                    Draw.untextured_quad (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1))) (Quad.color color.Value)

                let text =
                    match label.Value with
                    | ProgressMeterLabel.Countdown -> "7:27"
                    | ProgressMeterLabel.Percentage -> "60%"
                    | _ -> ""

                Text.fill_b (
                    Style.font,
                    text,
                    bounds.BorderBottom(40.0f),
                    Colors.text_subheading,
                    Alignment.CENTER
                )
        }

    do
        this.Content(
            page_container()
            |+ PageSetting("hud.progressmeter.label", Selector<ProgressMeterLabel>.FromEnum(label))
                .Pos(0)
            |+ ([
                PageSetting("hud.progressmeter.color", ColorPicker(color, true))
                    .Pos(2, 3) :> Widget
                PageSetting("hud.progressmeter.backgroundcolor", ColorPicker(background_color, true))
                    .Pos(5, 3)
                ] |> or_require_noteskin)
            |>> Container
            |+ preview
        )

    override this.Title = %"hud.progressmeter.name"

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                ProgressMeterLabel = label.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                ProgressMeterColor = color.Value
                ProgressMeterBackgroundColor = background_color.Value
            }

        on_close ()