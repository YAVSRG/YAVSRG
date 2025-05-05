namespace Interlude.Features.OptionsMenu.Presets

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI

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

    member this.SaveChanges() =
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

    override this.Content() =
        this.OnClose(this.SaveChanges)

        let keymode_preference =
            FlowContainer.LeftToRight<PresetKeymodeCheckbox>(100.0f, Spacing = 10.0f)

        for keymode = 3 to 10 do
            keymode_preference.Add(PresetKeymodeCheckbox(preset_id, keymode))

        page_container()
            .With(
                PageTextEntry(%"gameplay.preset.name", name).Pos(0),
                PageSetting(
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
                    .Pos(2),
                PageSetting(%"gameplay.preset.keymode_preference", keymode_preference)
                    .Help(Help.Info("gameplay.preset.keymode_preference"))
                    .Pos(4, 2, PageWidth.Custom (PAGE_LABEL_WIDTH + (keymode_preference :> IWidth).Width)),
                PageButton(
                    %"gameplay.preset.delete",
                    (fun () ->
                        delete <- true
                        Menu.Back()
                    ),
                    Disabled = fun () -> mode.Value = PresetMode.Locked
                )
                    .Pos(7),

                Text(%"gameplay.preset.current_options")
                    .Color(Colors.text)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(10),
                Text(sprintf "%s: %.2f" %"gameplay.scrollspeed" preset.ScrollSpeed)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(11),
                Text(sprintf "%s: %s" %"gameplay.upscroll" (if preset.Upscroll then "ON" else "OFF"))
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(12),
                Text(sprintf "%s: %.0f" %"gameplay.hitposition" preset.HitPosition)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(13),
                Text(sprintf "%s: %.0f" %"system.visualoffset" preset.VisualOffset)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(14),
                Text(sprintf "%s %s" %"skins.current_noteskin" preset.Noteskin)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(15),
                Text(sprintf "%s %s" %"skins.current_hud" preset.HUD)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(16),
                Text(sprintf "%s: %s" %"gameplay.lanecover" (if preset.LaneCover.Enabled then "ON + Settings" else "OFF"))
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .TextPosSmall(17)
            )

    override this.Title = preset.Name