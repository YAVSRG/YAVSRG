namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Options
open Interlude.Features.Play.HUD

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

    let use_font = Setting.simple noteskin_options.ComboUseFont
    let font_spacing = Setting.simple noteskin_options.ComboFontSpacing |> Setting.bound -1.0f 1.0f

    let texture = Content.Texture "combo-font"
    let preview =
        { new ConfigPreviewNew(pos.Value) with
            override this.DrawComponent(bounds) =
                if use_font.Value then
                    Combo.draw_combo_centered(texture, bounds, Color.White, 727, font_spacing.Value)
                else
                    Text.fill (Style.font, "727", bounds, Color.White, Alignment.CENTER)
        }

    do
        this.Content(
            page_container()
            |+ PageSetting("hud.combo.lampcolors", Checkbox lamp_colors)
                .Tooltip(Tooltip.Info("hud.combo.lampcolors"))
                .Pos(0)
            |+ ([
                PageSetting("hud.combo.pop", Slider(pop_amount, Step = 1f))
                    .Tooltip(Tooltip.Info("hud.combo.pop"))
                    .Pos(2)
                PageSetting("hud.combo.growth", Slider(growth_amount))
                    .Tooltip(Tooltip.Info("hud.combo.growth"))
                    .Pos(4)
                PageSetting("hud.generic.use_font", Checkbox use_font)
                    .Tooltip(Tooltip.Info("hud.generic.use_font"))
                    .Pos(7)
                Conditional(use_font.Get,
                    PageSetting("hud.generic.font_spacing", Slider.Percent(font_spacing))
                        .Tooltip(Tooltip.Info("hud.generic.font_spacing"))
                        .Pos(9)
                )
            ] |> or_require_noteskin)
            |>> Container
            |+ preview
        )

    override this.Title = %"hud.combo.name"

    override this.OnClose() =
        options.HUD.Set
            { options.HUD.Value with
                ComboLampColors = lamp_colors.Value
            }

        Noteskins.save_hud_config
            { Content.NoteskinConfig.HUD with
                ComboPop = pop_amount.Value
                ComboGrowth = growth_amount.Value
                ComboUseFont = use_font.Value
                ComboFontSpacing = font_spacing.Value
            }

        on_close ()