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

type ProgressMeterPage(on_close: unit -> unit) as this =
    inherit Page()

    let user_options = options.HUD.Value
    let noteskin_options = Content.NoteskinConfig.HUD

    let pos = Setting.simple noteskin_options.ProgressMeterPosition

    let color = Setting.simple noteskin_options.ProgressMeterColor
    let background_color = Setting.simple noteskin_options.ProgressMeterBackgroundColor
    let label = Setting.simple user_options.ProgressMeterLabel

    let label_size = Setting.percentf noteskin_options.ProgressMeterLabelSize

    let use_font = Setting.simple noteskin_options.ProgressMeterUseFont
    let font_spacing = Setting.simple noteskin_options.ProgressMeterFontSpacing |> Setting.bound -1.0f 1.0f
    let font_colon_spacing = Setting.simple noteskin_options.ProgressMeterColonExtraSpacing |> Setting.bound -1.0f 1.0f
    let font_percent_spacing = Setting.simple noteskin_options.ProgressMeterPercentExtraSpacing |> Setting.bound -1.0f 1.0f

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

    do
        this.Content(
            page_container()
            |+ PageSetting("hud.progressmeter.label", SelectDropdown.FromEnum(label))
                .Pos(0)
            |+ ([
                PageSetting("hud.progressmeter.label_size", Slider.Percent(label_size))
                    .Pos(2)
                    .Tooltip(Tooltip.Info("hud.progressmeter.label_size")) :> Widget
                PageSetting("hud.progressmeter.color", ColorPicker(color, true))
                    .Pos(4, 3)
                PageSetting("hud.progressmeter.backgroundcolor", ColorPicker(background_color, true))
                    .Pos(7, 3)
                PageSetting("hud.generic.use_font", Checkbox use_font)
                    .Pos(10)
                    .Tooltip(Tooltip.Info("hud.generic.use_font")) :> Widget
                Conditional(use_font.Get,
                    PageSetting("hud.generic.font_spacing", Slider.Percent(font_spacing))
                        .Pos(12)
                        .Tooltip(Tooltip.Info("hud.generic.font_spacing"))
                )
                Conditional(use_font.Get,
                    PageSetting("hud.generic.colon_spacing", Slider.Percent(font_colon_spacing))
                        .Pos(14)
                        .Tooltip(Tooltip.Info("hud.generic.colon_spacing"))
                )
                Conditional(use_font.Get,
                    PageSetting("hud.generic.percent_spacing", Slider.Percent(font_percent_spacing))
                        .Pos(16)
                        .Tooltip(Tooltip.Info("hud.generic.percent_spacing"))
                )
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
                ProgressMeterLabelSize = label_size.Value

                ProgressMeterUseFont = use_font.Value
                ProgressMeterFontSpacing = font_spacing.Value
                ProgressMeterColonExtraSpacing = font_colon_spacing.Value
                ProgressMeterPercentExtraSpacing = font_percent_spacing.Value
            }

        on_close ()