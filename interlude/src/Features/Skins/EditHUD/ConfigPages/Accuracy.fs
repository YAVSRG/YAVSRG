namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.Noteskins
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Features.Play.HUD

type AccuracyPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let grade_colors = Setting.simple config.AccuracyGradeColors
    let show_name = Setting.simple config.AccuracyShowName

    let use_font = Setting.simple config.AccuracyUseFont
    let font_spacing = Setting.simple config.AccuracyFontSpacing |> Setting.bound -1.0f 1.0f
    let font_dot_spacing = Setting.simple config.AccuracyDotExtraSpacing |> Setting.bound -1.0f 1.0f
    let font_percent_spacing = Setting.simple config.AccuracyPercentExtraSpacing |> Setting.bound -1.0f 1.0f

    let texture = Content.Texture "accuracy-font"
    let preview =
        { new ConfigPreviewNew(config.AccuracyPosition) with
            override this.DrawComponent(bounds) =
                if use_font.Value then
                    Accuracy.draw_accuracy_centered(
                        texture,
                        bounds.TrimBottom(bounds.Height * 0.4f),
                        Color.White,
                        0.9672,
                        font_spacing.Value,
                        font_dot_spacing.Value,
                        font_percent_spacing.Value
                    )
                else
                    Text.fill (Style.font, "96.72%", bounds.TrimBottom(bounds.Height * 0.3f), Color.White, 0.5f)

                if show_name.Value then
                    Text.fill (Style.font, "SC J4", bounds.SliceBottom(bounds.Height * 0.4f), Color.White, 0.5f)
        }

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.accuracy.gradecolors", Checkbox grade_colors)
            .Help(Help.Info("hud.accuracy.gradecolors"))
            .Pos(0)
        |+ PageSetting(%"hud.accuracy.showname", Checkbox show_name)
            .Help(Help.Info("hud.accuracy.showname"))
            .Pos(2)
        |+ PageSetting(%"hud.generic.use_font", Checkbox use_font)
            .Help(Help.Info("hud.generic.use_font"))
            .Pos(7)
        |+ PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
            .Help(Help.Info("hud.generic.font_spacing"))
            .Pos(9)
            .Conditional(use_font.Get)
        |+ PageSetting(%"hud.generic.dot_spacing", Slider.Percent(font_dot_spacing))
            .Help(Help.Info("hud.generic.dot_spacing"))
            .Pos(11)
            .Conditional(use_font.Get)
        |+ PageSetting(%"hud.generic.percent_spacing", Slider.Percent(font_percent_spacing))
            .Help(Help.Info("hud.generic.percent_spacing"))
            .Pos(13)
            .Conditional(use_font.Get)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.accuracy"

    override this.OnClose() =

        Skins.save_hud_config
            { Content.HUD with
                AccuracyGradeColors = grade_colors.Value
                AccuracyShowName = show_name.Value

                AccuracyUseFont = use_font.Value
                AccuracyFontSpacing = font_spacing.Value
                AccuracyDotExtraSpacing = font_dot_spacing.Value
                AccuracyPercentExtraSpacing = font_percent_spacing.Value
            }

        on_close ()