namespace Interlude.Features.OptionsMenu.Presets

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Skins
open Interlude.Features.Gameplay

module Presets =

    let confirm_before_switch (preset_id: int) : bool =
        match options.SelectedPreset.Value with
        | None -> true
        | Some i when preset_id = i -> false
        | Some i ->
            match (Presets.get i).Value with
            | Some p -> p.Mode <> PresetMode.Autosave
            | None -> true

    let load_and_edit (preset_id: int, setting: Setting<Preset option>) : unit =
        match Presets.load preset_id with
        | Some _ ->
            SkinPreview.RefreshAll()
            GameThread.defer (EditPresetPage(preset_id, setting).Show)
        | None -> ()

    let load (preset_id: int, _: Setting<Preset option>) : unit =
        match Presets.load preset_id with
        | Some name ->
            SelectedChart.recolor()
            SkinPreview.RefreshAll()
            Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.preset_loaded", name)
        | None -> ()

    let confirm_if_needed (action: int * Setting<Preset option> -> unit) : int * Setting<Preset option> -> unit =
        fun (preset_id, setting) ->
            if confirm_before_switch preset_id then
                match setting.Value with
                | Some s ->
                    ConfirmPage(
                        [ s.Name ] %> "gameplay.preset.load.prompt",
                        fun () -> action (preset_id, setting)
                    )
                        .Show()
                | None -> ()
            else
                action (preset_id, setting)

type PresetControls =

    static member private EditPresetButton(preset_id: int, setting: Setting<Preset option>) : Button =
        Button(
            (fun () ->
                match setting.Value with
                | None -> sprintf "Preset %i (Empty)" preset_id
                | Some s -> Icons.EDIT_2 + " " + s.Name
            ),
            (fun () -> Presets.confirm_if_needed Presets.load_and_edit (preset_id, setting))
        )
            .Floating()
            .Disabled(fun () -> setting.Value.IsNone)

    static member private LoadPresetButton(preset_id: int, setting: Setting<Preset option>) : Button =
        Button(
            %"gameplay.preset.load",
            (fun () -> Presets.confirm_if_needed Presets.load (preset_id, setting))
        )
            .Disabled(fun () -> setting.Value.IsNone)

    static member private SavePresetButton(preset_id: int, setting: Setting<Preset option>) : Button =
        Button(
            %"gameplay.preset.save",
            (fun () ->
                match setting.Value with
                | None ->
                    let name = sprintf "Preset %i" preset_id
                    setting.Value <- Presets.create (name) |> Some
                    Notifications.action_feedback (Icons.ALERT_OCTAGON, %"notification.preset_saved", name)
                | Some existing ->
                    ConfirmPage(
                        [ existing.Name ] %> "gameplay.preset.save.prompt",
                        fun () ->
                            setting.Value <- Presets.save existing |> Some

                            Notifications.action_feedback (
                                Icons.ALERT_OCTAGON,
                                %"notification.preset_saved",
                                existing.Name
                            )
                    )
                        .Show()
            )
        )
            .Disabled(fun () ->
                match setting.Value with
                | Some s -> s.Mode <> PresetMode.Unlocked
                | None -> false
            )

    static member Create(preset_id: int, setting: Setting<Preset option>) : Widget =
        NavigationContainer.Column()
            .WrapNavigation(false)
            .With(
                NavigationContainer.Row()
                    .WrapNavigation(false)
                    .With(
                        PresetControls.LoadPresetButton(preset_id, setting)
                            .Position(Position.SliceB(40.0f).ShrinkX(Style.PADDING * 2.0f).GridX(1, 2, Style.PADDING * 4.0f)),
                        PresetControls.SavePresetButton(preset_id, setting)
                            .Position(Position.SliceB(40.0f).ShrinkX(Style.PADDING * 2.0f).GridX(2, 2, Style.PADDING * 4.0f))
                    )
                    .Conditional(fun () ->
                        options.SelectedPreset.Value <> Some preset_id
                        || match setting.Value with
                           | Some p -> p.Mode <> PresetMode.Autosave
                           | None -> true
                    ),

                PresetControls.EditPresetButton(preset_id, setting)
                    .Position(Position.SliceT(40.0f)),

                Text(sprintf "%s %s" Icons.REFRESH_CW (%"gameplay.preset.autosaving"))
                    .Color(Colors.text_green)
                    .Position(Position.SliceB(40.0f).ShrinkX(10.0f))
                    .Conditional(fun () ->
                        options.SelectedPreset.Value = Some preset_id
                        && match setting.Value with
                           | Some p -> p.Mode = PresetMode.Autosave
                           | None -> false
                    ),

                Frame()
                    .Border(Colors.cyan)
                    .Fill(Colors.cyan_shadow)
            )