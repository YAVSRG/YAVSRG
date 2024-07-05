namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skinning.HudLayouts
open Interlude.Content
open Interlude.UI.Menu
open Interlude.Features.Play.HUD

type ProgressMeterPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let pos = Setting.simple config.ProgressMeterPosition

    let color = Setting.simple config.ProgressMeterColor
    let background_color = Setting.simple config.ProgressMeterBackgroundColor
    let label = Setting.simple config.ProgressMeterLabel

    let label_size = Setting.percentf config.ProgressMeterLabelSize

    let use_font = Setting.simple config.ProgressMeterUseFont
    let font_spacing = Setting.simple config.ProgressMeterFontSpacing |> Setting.bound -1.0f 1.0f
    let font_colon_spacing = Setting.simple config.ProgressMeterColonExtraSpacing |> Setting.bound -1.0f 1.0f
    let font_percent_spacing = Setting.simple config.ProgressMeterPercentExtraSpacing |> Setting.bound -1.0f 1.0f

    let font_texture = Content.Texture "progress-meter-font"

    let preview =
        { new ConfigPreviewNew(pos.Value) with
            override this.DrawComponent(bounds) =
                ProgressMeter.draw_pie(bounds.SliceTop(bounds.Width), color.Value, background_color.Value, 0.6f)

                if use_font.Value then

                    match label.Value  with
                        | ProgressMeterLabel.Countdown ->
                            let time_left = 447000.0f<ms>
                            ProgressMeter.draw_countdown_centered (
                                font_texture,
                                bounds.SliceBottom(bounds.Width * label_size.Value), 
                                Color.White,
                                time_left,
                                font_spacing.Value,
                                font_colon_spacing.Value
                            )
                        | ProgressMeterLabel.Percentage ->
                            ProgressMeter.draw_percent_progress_centered (
                                font_texture,
                                bounds.SliceBottom(bounds.Width * label_size.Value), 
                                Color.White,
                                0.6f,
                                font_spacing.Value,
                                font_percent_spacing.Value
                            )
                        | _ -> ()

                else

                    let text =
                        match label.Value with
                        | ProgressMeterLabel.Countdown -> "7:27"
                        | ProgressMeterLabel.Percentage -> "60%"
                        | _ -> ""

                    Text.fill_b (
                        Style.font,
                        text,
                        bounds.SliceBottom(bounds.Width * label_size.Value),
                        Colors.text_subheading,
                        Alignment.CENTER
                    )
        }

    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.progressmeter.label", 
            SelectDropdown(
                [|
                    ProgressMeterLabel.None, %"hud.progressmeter.label.none"
                    ProgressMeterLabel.Countdown, %"hud.progressmeter.label.countdown"
                    ProgressMeterLabel.Percentage, %"hud.progressmeter.label.percentage"
                |],
                label
            )
        )
            .Pos(0)
        |+ PageSetting(%"hud.progressmeter.label_size", Slider.Percent(label_size))
            .Tooltip(Tooltip.Info("hud.progressmeter.label_size"))
            .Pos(2)
        |+ PageSetting(%"hud.progressmeter.color", ColorPicker(color, true))
            .Pos(4, 3)
        |+ PageSetting(%"hud.progressmeter.backgroundcolor", ColorPicker(background_color, true))
            .Pos(7, 3)
        |+ PageSetting(%"hud.generic.use_font", Checkbox use_font)
            .Tooltip(Tooltip.Info("hud.generic.use_font"))
            .Pos(10)
        |+ PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
            .Tooltip(Tooltip.Info("hud.generic.font_spacing"))
            .Pos(12)
            .Conditional(use_font.Get)
        |+ PageSetting(%"hud.generic.colon_spacing", Slider.Percent(font_colon_spacing))
            .Tooltip(Tooltip.Info("hud.generic.colon_spacing"))
            .Pos(14)
            .Conditional(use_font.Get)
        |+ PageSetting(%"hud.generic.percent_spacing", Slider.Percent(font_percent_spacing))
            .Tooltip(Tooltip.Info("hud.generic.percent_spacing"))
            .Pos(16)
            .Conditional(use_font.Get)
        |>> Container
        |+ preview
        :> Widget

    override this.Title = %"hud.progressmeter"

    override this.OnClose() =
        Skins.save_hud_config 
            { Content.HUD with
                ProgressMeterLabel = label.Value
                ProgressMeterColor = color.Value
                ProgressMeterBackgroundColor = background_color.Value
                ProgressMeterLabelSize = label_size.Value

                ProgressMeterUseFont = use_font.Value
                ProgressMeterFontSpacing = font_spacing.Value
                ProgressMeterColonExtraSpacing = font_colon_spacing.Value
                ProgressMeterPercentExtraSpacing = font_percent_spacing.Value
            }

        on_close ()