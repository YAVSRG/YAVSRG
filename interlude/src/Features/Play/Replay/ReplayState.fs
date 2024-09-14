namespace Interlude.Features.Play.Replay

open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Prelude.Data.User

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto of ColoredChart
    | Replay of score_info: ScoreInfo * with_colors: ColoredChart

[<AutoOpen>]
module private ReplayModeSettings =
    
    let show_input_overlay = Setting.simple false
    let show_hit_overlay = Setting.simple false
    let playfield_dim: Setting.Bounded<float32> = Setting.percentf 0.5f