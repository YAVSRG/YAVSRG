namespace Interlude.Features.Play.Replay

open Percyqaz.Common
open Prelude

[<AutoOpen>]
module private ReplayModeSettings =

    let show_input_overlay = Setting.simple false
    let show_hit_overlay = Setting.simple false
    let show_hit_overlay_labels = Setting.simple true
    let show_difficulty_overlay = Setting.simple false
    let playfield_dim: Setting.Bounded<float32> = Setting.percentf 0.5f
    let fixed_scroll_speed = Setting.simple false

    let overlay_shown () =
        show_input_overlay.Value || show_hit_overlay.Value || show_difficulty_overlay.Value
