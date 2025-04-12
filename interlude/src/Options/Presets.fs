namespace Interlude.Options

open Percyqaz.Common
open Prelude.Mods
open Interlude.Content

module Presets =

    let get (id: int) : Setting<Preset option> =
        [| options.Preset1; options.Preset2; options.Preset3 |].[id - 1]

    let create (name: string) : Preset =
        {
            Name = name
            Mode = PresetMode.Autosave

            VisualOffset = options.VisualOffset.Value
            ScrollSpeed = options.ScrollSpeed.Value
            HitPosition = options.HitPosition.Value
            Upscroll = options.Upscroll.Value
            LaneCover = options.LaneCover.ToPreset
            Noteskin = options.Noteskin.Value
            HUD = options.SelectedHUD.Value
        }

    let save (preset: Preset) : Preset =
        { preset with
            VisualOffset = options.VisualOffset.Value
            ScrollSpeed = options.ScrollSpeed.Value
            HitPosition = options.HitPosition.Value
            Upscroll = options.Upscroll.Value
            LaneCover = options.LaneCover.ToPreset
            Noteskin = options.Noteskin.Value
            HUD = options.SelectedHUD.Value
        }

    let load (id: int) : string option =
        match options.SelectedPreset.Value with
        | None -> ()
        | Some i ->
            let setting = get i

            match setting.Value with
            | Some preset when preset.Mode = PresetMode.Autosave -> setting.Set(Some(save preset))
            | _ -> ()

        match (get id).Value with
        | Some loaded_preset ->
            options.SelectedPreset.Value <- Some id

            options.VisualOffset.Set loaded_preset.VisualOffset
            options.ScrollSpeed.Set loaded_preset.ScrollSpeed
            options.HitPosition.Set loaded_preset.HitPosition
            options.Upscroll.Set loaded_preset.Upscroll
            options.LaneCover.LoadPreset loaded_preset.LaneCover

            if Skins.noteskin_exists loaded_preset.Noteskin then
                options.Noteskin.Set loaded_preset.Noteskin
            else
                Logging.Debug
                    "Noteskin '%s' used in this preset has been renamed or isn't available"
                    loaded_preset.Noteskin

            if Skins.hud_exists loaded_preset.HUD then
                options.SelectedHUD.Set loaded_preset.HUD
            else
                Logging.Debug
                    "HUD '%O' used in this preset has been renamed or isn't available"
                    loaded_preset.HUD

            Some loaded_preset.Name
        | None -> None

    let mutable private previous_keymode: int option = None
    let check_for_keymode_change (base_keys: int, mods: ModState) : unit =

        // todo: push down into Prelude
        let keys = 
            if mods.ContainsKey "column_swap" then
                ColumnSwap.keys mods.["column_swap"]
            else base_keys

        if previous_keymode <> Some keys then 
            
            match options.KeymodePreferredPresets.[keys - 3] with
            | Some preference -> load preference |> ignore
            | None -> ()

            previous_keymode <- Some keys