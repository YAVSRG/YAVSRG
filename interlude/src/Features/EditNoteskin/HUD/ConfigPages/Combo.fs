namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options

type ComboPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.ComboPosition

    let lamp_colors = Setting.simple user_options.ComboLampColors

    let pop_amount =
        Setting.simple noteskin_options.ComboPop |> Setting.bound 0.0f 20.0f

    let growth_amount =
        Setting.simple noteskin_options.ComboGrowth |> Setting.bound 0.0f 0.05f

    let preview =
        { new ConfigPreview(0.35f, pos) with
            override this.DrawComponent(bounds) =
                Text.fill (Style.font, "727", bounds, Color.White, Alignment.CENTER)
        }

    do
        this.Content(
            page_container()
            |+ PageSetting("hud.combo.lampcolors", Selector<_>.FromBool lamp_colors)
                .Pos(0)
                .Tooltip(Tooltip.Info("hud.combo.lampcolors"))
            |+ ([
                PageSetting("hud.combo.pop", Slider(pop_amount, Step = 1f))
                    .Pos(2)
                    .Tooltip(Tooltip.Info("hud.combo.pop")) :> Widget
                PageSetting("hud.combo.growth", Slider(growth_amount))
                    .Pos(4)
                    .Tooltip(Tooltip.Info("hud.combo.growth"))
            ] |> or_require_noteskin)
            |>> Container
            |+ preview
        )

    override this.Title = %"hud.combo.name"
    override this.OnDestroy() = preview.Destroy()

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                ComboLampColors = lamp_colors.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                ComboPop = pop_amount.Value
                ComboGrowth = growth_amount.Value
            }

        on_close ()