namespace Interlude.Features.OptionsMenu.SystemSettings

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

type AudioPage() =
    inherit Page()

    static member AudioVolume() : PageSetting =
        PageSetting(
            %"system.audiovolume",
            Slider.Percent(
                options.AudioVolume
                |> Setting.trigger (fun v -> Audio.change_volume (v, v))
                |> Setting.f32
            )
        )

    static member AudioDevice() : PageSetting =
        PageSetting(
            %"system.audiodevice",
            SelectDropdown(Audio.list_devices () |> Array.map (fun d -> d.Index, d.ToString()), Setting.trigger Audio.change_device config.AudioDevice)
        )

    static member RatesChangePitchDown() : PageSetting =
        PageSetting(
            %"system.audio_pitch_rates_down",
            Checkbox(
                options.AudioPitchRatesDown
                |> Setting.trigger (fun v -> Song.set_pitch_rate_down_enabled v)
            )
        )
            .Help(Help.Info("system.audio_pitch_rates_down"))

    static member RatesChangePitchUp() : PageSetting =
        PageSetting(
            %"system.audio_pitch_rates_up",
            Checkbox(
                options.AudioPitchRatesUp
                |> Setting.trigger (fun v -> Song.set_pitch_rate_up_enabled v)
            )
        )
            .Help(Help.Info("system.audio_pitch_rates_up"))

    static member MenusMuffleSong() : PageSetting =
        PageSetting(%"system.menus_muffle_song",
            Checkbox (
                options.MenusMuffleSong
                |> Setting.trigger (fun b -> if b then Song.set_low_pass 1.0f else Song.set_low_pass 0.0f)
            )
        )
            .Help(Help.Info("system.menus_muffle_song"))

    static member AudioOffset() : PageSetting =
        PageSetting(
            %"system.audiooffset",
            { new Slider(Setting.uom options.AudioOffset, Step = 1f) with
                override this.OnDeselected(by_mouse: bool) =
                    base.OnDeselected by_mouse
                    Song.set_global_offset options.AudioOffset.Value
            }
        )
            .Help(Help.Info("system.audiooffset"))

    static member AutomaticOffset() : PageSetting =
        PageSetting(%"system.automatic_offset", Checkbox options.AutoCalibrateOffset)
            .Help(Help.Info("system.automatic_offset"))

    override this.Content() =
        page_container()
            .With(
                AudioPage.AudioVolume().Pos(0),
                AudioPage.AudioDevice().Pos(2),
                AudioPage.RatesChangePitchDown().Pos(5),
                AudioPage.RatesChangePitchUp().Pos(7),
                AudioPage.MenusMuffleSong().Pos(9),
                AudioPage.AudioOffset().Pos(12),
                AudioPage.AutomaticOffset().Pos(14)
            )

    override this.Title = Icons.SPEAKER + " " + %"system.audio"