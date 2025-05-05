namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Play.HUD

type AccuracyPage() =
    inherit Page()

    let config = Content.HUD

    let grade_colors = Setting.simple config.AccuracyGradeColors
    let show_name = Setting.simple config.AccuracyShowName

    let use_font = Setting.simple config.AccuracyUseFont
    let font_spacing = config.AccuracyFontSpacing |> Setting.bounded (-1.0f, 1.0f)
    let font_dot_spacing = config.AccuracyDotExtraSpacing |> Setting.bounded (-1.0f, 1.0f)
    let font_percent_spacing = config.AccuracyPercentExtraSpacing |> Setting.bounded (-1.0f, 1.0f)

    let alignment = config.AccuracyPosition.TextAlignment

    let texture = Content.Texture "accuracy-font"
    let preview =
        { new ElementPreview(config.AccuracyPosition) with
            override this.DrawComponent(bounds) =
                if use_font.Value then
                    Accuracy.draw_accuracy_aligned(
                        texture,
                        bounds.ShrinkB(bounds.Height * 0.4f),
                        Color.White,
                        Rulesets.current.FormatAccuracy 0.967234,
                        font_spacing.Value,
                        font_dot_spacing.Value,
                        font_percent_spacing.Value,
                        alignment
                    )
                else
                    Text.fill (Style.font, Rulesets.current.FormatAccuracy 0.967234, bounds.ShrinkB(bounds.Height * 0.3f), Color.White, alignment)

                if show_name.Value then
                    Text.fill (Style.font, Rulesets.current.Name, bounds.SliceB(bounds.Height * 0.4f), Color.White, alignment)
        }

    member this.SaveChanges() =

        Skins.save_hud_config
            { Content.HUD with
                AccuracyGradeColors = grade_colors.Value
                AccuracyShowName = show_name.Value

                AccuracyUseFont = use_font.Value
                AccuracyFontSpacing = font_spacing.Value
                AccuracyDotExtraSpacing = font_dot_spacing.Value
                AccuracyPercentExtraSpacing = font_percent_spacing.Value
            }

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.accuracy.gradecolors", Checkbox grade_colors)
                    .Help(Help.Info("hud.accuracy.gradecolors"))
                    .Pos(0),
                PageSetting(%"hud.accuracy.showname", Checkbox show_name)
                    .Help(Help.Info("hud.accuracy.showname"))
                    .Pos(2),
                PageSetting(%"hud.generic.use_font", Checkbox use_font)
                    .Help(Help.Info("hud.generic.use_font"))
                    .Pos(7),
                PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
                    .Help(Help.Info("hud.generic.font_spacing"))
                    .Pos(9)
                    .Conditional(use_font.Get),
                PageSetting(%"hud.generic.dot_spacing", Slider.Percent(font_dot_spacing))
                    .Help(Help.Info("hud.generic.dot_spacing"))
                    .Pos(11)
                    .Conditional(use_font.Get),
                PageSetting(%"hud.generic.percent_spacing", Slider.Percent(font_percent_spacing))
                    .Help(Help.Info("hud.generic.percent_spacing"))
                    .Pos(13)
                    .Conditional(use_font.Get)
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"hud.accuracy"