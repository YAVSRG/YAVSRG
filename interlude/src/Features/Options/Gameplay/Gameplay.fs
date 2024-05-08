namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude
open Interlude.Options
open Interlude.UI.Menu
open Interlude.Features
open Interlude.Features.Pacemaker
open Interlude.Features.EditNoteskin
open Interlude.Features.OptionsMenu

type GameplayPage() as this =
    inherit Page()

    let keymode: Setting<Keymode> = Setting.simple <| Gameplay.Chart.keymode ()

    let binds = GameplayKeybinder(keymode)
    let preview = NoteskinPreview(NoteskinPreview.RIGHT_HAND_SIDE 0.35f)

    do
        page_container()
        |+ PageSetting("gameplay.scrollspeed", Slider.Percent(options.ScrollSpeed))
            .Tooltip(Tooltip.Info("gameplay.scrollspeed"))
            .Pos(0)
        |+ Text(
            (fun () ->
                [
                    (options.ScrollSpeed.Value * 31.0f / 2.38f).ToString("F1")
                    (options.ScrollSpeed.Value * 33.9f / 2.38f).ToString("F1")
                    "C" + (60000.0f * options.ScrollSpeed.Value / Interlude.Content.Content.NoteskinConfig.ColumnWidth).ToString("F0")
                ]
                %> "gameplay.scrollspeed.info"
            ),
            Align = Alignment.CENTER,
            Position = pretty_pos(2, 1, PageWidth.Normal).TrimLeft(PRETTYTEXTWIDTH)
        )
        |+ PageSetting("gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
            .Tooltip(Tooltip.Info("gameplay.hitposition"))
            .Pos(3)
        |+ PageSetting("gameplay.upscroll", Checkbox options.Upscroll)
            .Tooltip(Tooltip.Info("gameplay.upscroll"))
            .Pos(5)
        |+ PageSetting("gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
            .Tooltip(Tooltip.Info("gameplay.backgrounddim"))
            .Pos(7)
        |+ PageSetting(
            "system.audiooffset",
            { new Slider(options.AudioOffset, Step = 1f) with
                override this.OnDeselected(by_mouse: bool) =
                    base.OnDeselected by_mouse
                    Song.set_global_offset (options.AudioOffset.Value * 1.0f<ms>)
            }
        )
            .Tooltip(Tooltip.Info("system.audiooffset"))
            .Pos(9)
        |+ PageSetting("system.visualoffset", Slider(options.VisualOffset, Step = 1f))
            .Tooltip(Tooltip.Info("system.visualoffset"))
            .Pos(11)
        |+ PageButton("gameplay.lanecover", (fun () -> Menu.ShowPage LanecoverPage))
            .Tooltip(Tooltip.Info("gameplay.lanecover"))
            .Pos(14)
        |+ PageButton("gameplay.pacemaker", (fun () -> Menu.ShowPage PacemakerOptionsPage))
            .Tooltip(Tooltip.Info("gameplay.pacemaker").Body(%"gameplay.pacemaker.hint"))
            .Pos(16)
        |+ PageSetting(
            "generic.keymode",
            Selector.FromEnum(keymode |> Setting.trigger (ignore >> binds.OnKeymodeChanged))
        )
            .Pos(19)
        |+ PageSetting("gameplay.keybinds", binds)
            .Tooltip(Tooltip.Info("gameplay.keybinds"))
            .Pos(21, 2, PageWidth.Full)
        |>> Container
        |+ preview
        |+ Presets.preset_buttons 1 options.Preset1 preview.Refresh
        |+ Presets.preset_buttons 2 options.Preset2 preview.Refresh
        |+ Presets.preset_buttons 3 options.Preset3 preview.Refresh
        |> this.Content

    override this.Title = %"gameplay.name"
    override this.OnDestroy() = preview.Destroy()
    override this.OnClose() = ()

module Gameplay =

    let search_settings (tokens: string array) : SearchResult seq =
        seq {
            if token_match tokens [|%"gameplay.scrollspeed.name"|] then
                yield PageSetting("gameplay.scrollspeed", Slider.Percent(options.ScrollSpeed))
                    .Tooltip(Tooltip.Info("gameplay.scrollspeed"))
                , 2, 2, PageWidth.Normal
                yield Text(
                    (fun () ->
                        [
                            (options.ScrollSpeed.Value * 31.0f / 2.38f).ToString("F1")
                            (options.ScrollSpeed.Value * 33.9f / 2.38f).ToString("F1")
                            "C" + (60000.0f * options.ScrollSpeed.Value / Interlude.Content.Content.NoteskinConfig.ColumnWidth).ToString("F0")
                        ]
                        %> "gameplay.scrollspeed.info"
                    ),
                    Align = Alignment.CENTER
                ), 1, 1, PageWidth.Normal
            if token_match tokens [|%"gameplay.hitposition.name"|] then
                yield PageSetting("gameplay.hitposition", Slider(options.HitPosition, Step = 1f))
                    .Tooltip(Tooltip.Info("gameplay.hitposition"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.upscroll.name"|] then
                yield PageSetting("gameplay.upscroll", Checkbox options.Upscroll)
                    .Tooltip(Tooltip.Info("gameplay.upscroll"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.backgrounddim.name"|] then
                yield PageSetting("gameplay.backgrounddim", Slider.Percent(options.BackgroundDim))
                    .Tooltip(Tooltip.Info("gameplay.backgrounddim"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.lanecover.name"|] then
                yield PageButton("gameplay.lanecover", (fun () -> Menu.ShowPage LanecoverPage))
                    .Tooltip(Tooltip.Info("gameplay.lanecover"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"gameplay.pacemaker.name"|] then
                yield PageButton("gameplay.pacemaker", (fun () -> Menu.ShowPage PacemakerOptionsPage))
                    .Tooltip(Tooltip.Info("gameplay.pacemaker").Body(%"gameplay.pacemaker.hint"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"system.hotkeys.name"; %"gameplay.keybinds.name"|] then
                let keymode: Setting<Keymode> = Setting.simple <| Gameplay.Chart.keymode ()

                let binds = GameplayKeybinder(keymode)
                yield PageSetting(
                    "generic.keymode",
                    Selector.FromEnum(keymode |> Setting.trigger (ignore >> binds.OnKeymodeChanged))
                )
                , 2, 2, PageWidth.Normal
                yield PageSetting("gameplay.keybinds", binds)
                    .Tooltip(Tooltip.Info("gameplay.keybinds"))
                    .Pos(21, 2, PageWidth.Full)
                , 2, 2, PageWidth.Normal
        }