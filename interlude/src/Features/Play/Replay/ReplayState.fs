namespace Interlude.Features.Play.Replay

open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Prelude.Data

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto of ColoredChart
    | Replay of score_info: ScoreInfo * with_colors: ColoredChart

type private ReplayState =
    {
        ShowInputOverlay: Setting<bool>
        ShowHitOverlay: Setting<bool>
        PlayfieldDim: Setting.Bounded<float32>
        IsAuto: bool
    }