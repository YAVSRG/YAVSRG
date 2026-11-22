namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Input

module ScoreGraphSettings =

    let only_releases = Setting.simple false
    let column_filter = Array.create 10 true
    let scale = 1.0f |> Setting.bounded(1.0f, 6.0f)
    let show_slice = Setting.simple false
    let slice_size = 0.04f |> Setting.bounded(0.01f, 1.0f)

    let COLUMN_FILTER_KEYS =
        [|
            Keys.D1
            Keys.D2
            Keys.D3
            Keys.D4
            Keys.D5
            Keys.D6
            Keys.D7
            Keys.D8
            Keys.D9
            Keys.D0
        |]
