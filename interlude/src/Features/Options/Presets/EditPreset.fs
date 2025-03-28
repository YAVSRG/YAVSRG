namespace Interlude.Features.OptionsMenu.Presets

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Skins
open Interlude.Features.Gameplay

type private PresetKeymodeCheckbox(preset_id: int, keymode: int) as this =
    inherit Container(NodeType.Container(fun () -> Some this.Button))

    let old_value =
        match options.KeymodePreferredPresets.[keymode - 3] with
        | Some i when i = preset_id -> None
        | x -> x

    let button =
        Button(
            sprintf "%iK" keymode,
            fun () ->
                if options.KeymodePreferredPresets.[keymode - 3] = Some preset_id then
                    options.KeymodePreferredPresets.[keymode - 3] <- old_value
                else
                    options.KeymodePreferredPresets.[keymode - 3] <- Some preset_id
        )

    member private this.Button : Widget = button

    override this.Init(parent: Widget) : unit =
        this.Add(button)
        base.Init(parent)

    override this.Draw() =
        if this.Focused then
            Render.rect (this.Bounds.Shrink(5.0f)) Colors.yellow_accent.O1

        base.Draw()

        if options.KeymodePreferredPresets.[keymode - 3] = Some preset_id then
            Render.rect (this.Bounds.SliceB(5.0f)) Colors.yellow_accent

type private EditPresetPage(preset_id: int, setting: Setting<Preset option>) =
    inherit Page()

    let mutable delete = false

    let preset = setting.Value.Value
    let name = Setting.simple preset.Name
    let mode = Setting.simple preset.Mode

    override this.Content() =
        let keymode_preference =
            FlowContainer.LeftToRight<PresetKeymodeCheckbox>(100.0f, Spacing = 10.0f)

        for keymode = 3 to 10 do
            keymode_preference.Add(PresetKeymodeCheckbox(preset_id, keymode))

        page_container()
        |+ PageTextEntry(%"gameplay.preset.name", name).Pos(0)
        |+ PageSetting(
            %"gameplay.preset.mode",
            SelectDropdown<PresetMode>(
                [|
                    PresetMode.Unlocked, %"gameplay.preset.mode.unlocked"
                    PresetMode.Locked, %"gameplay.preset.mode.locked"
                    PresetMode.Autosave, %"gameplay.preset.mode.autosave"
                |],
                mode
            )
        )
            .Help(Help.Info("gameplay.preset.mode"))
            .Pos(2)
        |+ PageSetting(%"gameplay.preset.keymode_preference", keymode_preference)
            .Help(Help.Info("gameplay.preset.keymode_preference"))
            .Pos(4, 2, PageWidth.Custom (PAGE_LABEL_WIDTH + (keymode_preference :> IWidth).Width))
        |+ PageButton(
            %"gameplay.preset.delete",
            (fun () ->
                delete <- true
                Menu.Back()
            ),
            Disabled = fun () -> mode.Value = PresetMode.Locked
        )
            .Pos(7)
        // todo: localise
        |+ Text("Current preset options:")
            .Color(Colors.text)
            .Align(Alignment.LEFT)
            .Pos(10, 1)
        |+ Text(sprintf "Scroll speed: %.2f" preset.ScrollSpeed)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Pos(11, 1)
        |+ Text(sprintf "Upscroll: %s" (if preset.Upscroll then "ON" else "OFF"))
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Pos(12, 1)
        |+ Text(sprintf "Hit position: %.0f" preset.HitPosition)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Pos(13, 1)
        |+ Text(sprintf "Visual offset: %.0f" preset.VisualOffset)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Pos(14, 1)
        |+ Text(sprintf "Noteskin: %s" preset.Noteskin)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Pos(15, 1)
        |+ Text(sprintf "HUD: %s" preset.HUD)
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Pos(16, 1)
        |+ Text(sprintf "Lane cover: %s" (if preset.LaneCover.Enabled then "ON + Settings" else "OFF"))
            .Color(Colors.text_subheading)
            .Align(Alignment.LEFT)
            .Pos(17, 1)
        :> Widget

    override this.Title = preset.Name

    override this.OnClose() =
        if delete then
            setting.Set None
        else
            setting.Set(
                Some
                    { preset with
                        Name = name.Value
                        Mode = mode.Value
                    }
            )