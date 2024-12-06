﻿namespace Interlude.Options

open System.Collections.Generic
open Percyqaz.Flux.Input
open Percyqaz.Flux.Input.Bind

module Hotkeys =

    let init_startup (d: Dictionary<Hotkey, Bind>) =
        Hotkeys.register "search" (mk Keys.Tab)
        Hotkeys.register "toolbar" (ctrl Keys.T)
        Hotkeys.register "tooltip" (mk Keys.Slash)
        Hotkeys.register "delete" (mk Keys.Delete)
        Hotkeys.register "screenshot" (mk Keys.F12)
        Hotkeys.register "volume" (mk Keys.LeftAlt)
        Hotkeys.register "volume_up" (alt Keys.Up)
        Hotkeys.register "volume_down" (alt Keys.Down)
        Hotkeys.register "previous" (mk Keys.Left)
        Hotkeys.register "next" (mk Keys.Right)
        Hotkeys.register "previous_group" (mk Keys.PageUp)
        Hotkeys.register "next_group" (mk Keys.PageDown)
        Hotkeys.register "start" (mk Keys.Home)
        Hotkeys.register "end" (mk Keys.End)
        Hotkeys.register "pause_music" (Bind.Mouse MouseButton.Middle)

        Hotkeys.register "import" (ctrl Keys.I)
        Hotkeys.register "options" (ctrl Keys.O)
        Hotkeys.register "wiki" (ctrl Keys.H)
        Hotkeys.register "console" (mk Keys.GraveAccent)
        Hotkeys.register "player_list" (mk Keys.F9)
        Hotkeys.register "quick_menu" (ctrl Keys.Tab)

        Hotkeys.register "level_select_options" (mk Keys.D1)
        Hotkeys.register "like" (mk Keys.RightBracket)
        Hotkeys.register "unlike" (mk Keys.LeftBracket)
        Hotkeys.register "move_down_in_playlist" (ctrl Keys.Down)
        Hotkeys.register "move_up_in_playlist" (ctrl Keys.Up)
        Hotkeys.register "sort_mode" (mk Keys.D2)
        Hotkeys.register "reverse_sort_mode" (shift Keys.D2)
        Hotkeys.register "group_mode" (mk Keys.D3)
        Hotkeys.register "reverse_group_mode" (shift Keys.D3)
        Hotkeys.register "context_menu" (mk Keys.Period)
        Hotkeys.register "practice_mode" (mk Keys.V)
        Hotkeys.register "scoreboard" (mk Keys.Z)
        Hotkeys.register "scoreboard_storage" (mk Keys.Q)
        Hotkeys.register "scoreboard_sort" (mk Keys.W)
        Hotkeys.register "scoreboard_filter" (mk Keys.E)
        Hotkeys.register "table" (mk Keys.X)
        Hotkeys.register "collections" (mk Keys.C)
        Hotkeys.register "clear_multi_select" (shift Keys.X)
        Hotkeys.register "multi_select" (shift Keys.Z)
        Hotkeys.register "group_multi_select" (ctrl_shift Keys.Z)

        Hotkeys.register "accept_offset" (mk Keys.Tab)
        Hotkeys.register "reset_offset" (mk Keys.Backspace)
        Hotkeys.register "hide_replay_overlay" (mk Keys.H)

        Hotkeys.register "uprate_big" (ctrl_shift Keys.Equal)
        Hotkeys.register "downrate_big" (ctrl_shift Keys.Minus)
        Hotkeys.register "uprate" (mk Keys.Equal)
        Hotkeys.register "downrate" (mk Keys.Minus)
        Hotkeys.register "uprate_half" (ctrl Keys.Equal)
        Hotkeys.register "downrate_half" (ctrl Keys.Minus)
        Hotkeys.register "uprate_small" (shift Keys.Equal)
        Hotkeys.register "downrate_small" (shift Keys.Minus)

        Hotkeys.register "preview" (mk Keys.A)
        Hotkeys.register "mods" (mk Keys.S)
        Hotkeys.register "ruleset_switch" (mk Keys.D)
        Hotkeys.register "random_chart" (mk Keys.R)
        Hotkeys.register "previous_random_chart" (shift Keys.R)
        Hotkeys.register "autoplay" (ctrl Keys.A)
        Hotkeys.register "reload_content" (Bind.Key(Keys.S, (true, true, true)))

        Hotkeys.register "skip" (mk Keys.Space)
        Hotkeys.register "retry" (ctrl Keys.R)
        Hotkeys.register "next_song" (ctrl_shift Keys.R)
        Hotkeys.register "offset" (ctrl Keys.O)
        Hotkeys.register "save_score" (ctrl Keys.S)

        Hotkeys.register "preset1" (ctrl Keys.F1)
        Hotkeys.register "preset2" (ctrl Keys.F2)
        Hotkeys.register "preset3" (ctrl Keys.F3)

        Hotkeys.import d