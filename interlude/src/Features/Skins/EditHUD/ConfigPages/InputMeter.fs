namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Content
open Interlude.UI

type InputMeterPage(on_close: unit -> unit) =
    inherit Page()

    let config = Content.HUD

    let scroll_speed = Setting.bounded (0.5f<rate / ms>, 3.0f<rate / ms>) config.InputMeterScrollSpeed
    let key_fade_time = Setting.bounded (0.0f<ms / rate>, 1000.0f<ms / rate>) config.InputMeterKeyFadeTime
    let key_color = Setting.simple config.InputMeterKeyColor
    let column_padding = Setting.bounded (0.0f, 0.8f) config.InputMeterColumnPadding

    let show_inputs = Setting.simple config.InputMeterShowInputs
    let input_color = Setting.simple config.InputMeterInputColor
    let input_fade_distance = Setting.bounded (0.0f, 1000.0f) config.InputMeterInputFadeDistance
    let scroll_downwards = Setting.simple config.InputMeterScrollDownwards
    
    override this.Content() =
        page_container()
        |+ PageSetting(%"hud.inputmeter.scroll_speed", Slider.Percent(Setting.uom scroll_speed))
            .Pos(0)
        |+ PageSetting(%"hud.inputmeter.key_fade_time", Slider(Setting.uom key_fade_time, Step = 5f))
                .Pos(2)
        |+ PageSetting(%"hud.inputmeter.key_color", ColorPicker(key_color, true))
            .Pos(4, 3)
        |+ PageSetting(%"hud.inputmeter.column_padding", Slider.Percent column_padding)
            .Pos(7)
        |+ PageSetting(%"hud.inputmeter.scroll_downwards", Checkbox scroll_downwards)
            .Pos(9)
        |+ PageSetting(%"hud.inputmeter.show_inputs", Checkbox show_inputs)
            .Pos(11)
        |+ PageSetting(%"hud.inputmeter.input_color", ColorPicker(input_color, true))
            .Pos(13, 3)
            .Conditional(show_inputs.Get)
        |+ PageSetting(%"hud.inputmeter.input_fade_distance", Slider(input_fade_distance, Step = 5f))
            .Pos(16)
            .Conditional(show_inputs.Get)
        
        :> Widget

    override this.Title = %"hud.inputmeter"

    override this.OnClose() =
        Skins.save_hud_config 
            { Content.HUD with
                InputMeterScrollSpeed = scroll_speed.Value
                InputMeterKeyFadeTime = key_fade_time.Value
                InputMeterKeyColor = key_color.Value
                InputMeterColumnPadding = column_padding.Value
                InputMeterShowInputs = show_inputs.Value
                InputMeterInputColor = input_color.Value
                InputMeterInputFadeDistance = input_fade_distance.Value
                InputMeterScrollDownwards = scroll_downwards.Value
            }
        on_close ()
