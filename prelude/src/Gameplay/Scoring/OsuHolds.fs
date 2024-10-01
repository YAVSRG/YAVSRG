namespace Prelude.Gameplay.Scoring

open Prelude
open Prelude.Gameplay.Rulesets

module OsuHolds =

    let ln_judgement
        (windows: OsuLnWindows)
        (head_delta: GameplayTime)
        (tail_delta: GameplayTime)
        (overheld: bool)
        (dropped: bool)
        : int =
        let tail_delta_abs = abs tail_delta
        let head_delta_abs = abs head_delta

        if not dropped && not overheld then

            let mean = (tail_delta_abs + head_delta_abs) * 0.5f

            if tail_delta < -windows.Window50 then 5

            elif head_delta_abs < windows.Window320 && mean < windows.Window320 then 0 // 300g
            elif head_delta_abs < windows.Window300 && mean < windows.Window300 then 1 // 300
            elif head_delta_abs < windows.Window200 && mean < windows.Window200 then 2 // 200
            elif head_delta_abs < windows.Window100 && mean < windows.Window100 then 3 // 100
            elif head_delta_abs < windows.Window50 && mean < windows.Window50 then 4 // 50
            else 5 // miss

        elif dropped then

            if tail_delta_abs < windows.Window50 then 4 else 5

        else

            if head_delta_abs < windows.WindowOverhold200 then 2 // 200
            elif head_delta_abs < windows.WindowOverhold100 then 3 // 100
            else 4 // 50