namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI
open Interlude.Features.Play.HUD

type ProgressPiePage() =
    inherit Page()

    let config = Content.HUD

    let pos = Setting.simple config.ProgressMeterPosition

    let color = Setting.simple config.ProgressMeterColor
    let background_color = Setting.simple config.ProgressMeterBackgroundColor
    let label = Setting.simple config.ProgressMeterLabel

    let label_size = Setting.percentf config.ProgressMeterLabelSize

    let use_font = Setting.simple config.ProgressMeterUseFont
    let font_spacing = config.ProgressMeterFontSpacing |> Setting.bounded (-1.0f, 1.0f)
    let font_colon_spacing = config.ProgressMeterColonExtraSpacing |> Setting.bounded (-1.0f, 1.0f)
    let font_percent_spacing = config.ProgressMeterPercentExtraSpacing |> Setting.bounded (-1.0f, 1.0f)

    let font_texture = Content.Texture "progress-meter-font"

    let preview =
        { new ElementPreview(pos.Value) with
            override this.DrawComponent(bounds) =
                ProgressMeter.draw_pie(bounds.SliceT(bounds.Width), color.Value, background_color.Value, 0.6f)

                if use_font.Value then

                    match label.Value  with
                        | ProgressPieLabel.Countdown ->
                            let time_left = 447000.0f<ms / rate>
                            ProgressMeter.draw_countdown_centered (
                                font_texture,
                                bounds.SliceB(bounds.Width * label_size.Value),
                                Color.White,
                                time_left,
                                font_spacing.Value,
                                font_colon_spacing.Value
                            )
                        | ProgressPieLabel.Percentage ->
                            ProgressMeter.draw_percent_progress_centered (
                                font_texture,
                                bounds.SliceB(bounds.Width * label_size.Value),
                                Color.White,
                                0.6f,
                                font_spacing.Value,
                                font_percent_spacing.Value
                            )
                        | _ -> ()

                else

                    let text =
                        match label.Value with
                        | ProgressPieLabel.Countdown -> "7:27"
                        | ProgressPieLabel.Percentage -> "60%"
                        | _ -> ""

                    Text.fill_b (
                        Style.font,
                        text,
                        bounds.SliceB(bounds.Width * label_size.Value),
                        Colors.text_subheading,
                        Alignment.CENTER
                    )
        }

    member this.SaveChanges() =
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

    override this.Content() =
        this.OnClose(this.SaveChanges)

        page_container()
            .With(
                PageSetting(%"hud.progress_pie.label",
                    SelectDropdown(
                        [|
                            ProgressPieLabel.None, %"hud.progress_pie.label.none"
                            ProgressPieLabel.Countdown, %"hud.progress_pie.label.countdown"
                            ProgressPieLabel.Percentage, %"hud.progress_pie.label.percentage"
                        |],
                        label
                    )
                )
                    .Pos(0),
                PageSetting(%"hud.progress_pie.label_size", Slider.Percent(label_size))
                    .Help(Help.Info("hud.progress_pie.label_size"))
                    .Pos(2),
                PageSetting(%"hud.progress_pie.color", ColorPicker(%"hud.progress_pie.color", color, true))
                    .Pos(4),
                PageSetting(%"hud.progress_pie.backgroundcolor", ColorPicker(%"hud.progress_pie.backgroundcolor", background_color, true))
                    .Pos(6),
                PageSetting(%"hud.generic.use_font", Checkbox use_font)
                    .Help(Help.Info("hud.generic.use_font"))
                    .Pos(9)
            )
            .WithConditional(
                use_font.Get,

                PageSetting(%"hud.generic.font_spacing", Slider.Percent(font_spacing))
                    .Help(Help.Info("hud.generic.font_spacing"))
                    .Pos(11),
                PageSetting(%"hud.generic.colon_spacing", Slider.Percent(font_colon_spacing))
                    .Help(Help.Info("hud.generic.colon_spacing"))
                    .Pos(13),
                PageSetting(%"hud.generic.percent_spacing", Slider.Percent(font_percent_spacing))
                    .Help(Help.Info("hud.generic.percent_spacing"))
                    .Pos(15)
            )
        |> Container.Create
        |+ preview
        :> Widget

    override this.Title = %"hud.progress_pie"