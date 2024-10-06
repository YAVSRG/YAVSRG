namespace Prelude.Gameplay.Scoring

open Prelude
open Prelude.Gameplay.Rulesets

module OsuHolds =

    let ln_judgement
        (windows: OsuLnWindows)
        (head_delta: GameplayTime)
        (release_delta: GameplayTime)
        (overheld: bool)
        (dropped: bool)
        : int =
        let release_delta_abs = abs release_delta
        let head_delta_abs = abs head_delta

        if overheld then
            
            if head_delta_abs < windows.WindowOverhold200 then 2 // 200
            elif head_delta_abs < windows.WindowOverhold100 then 3 // 100
            else 4 // 50

        elif dropped then
            
            if release_delta < -windows.Window50 then 5 else 4

        else

            let mean = (release_delta_abs + head_delta_abs) * 0.5f

            if release_delta < -windows.Window50 then 5 // miss
            elif head_delta_abs < windows.Window320 && mean < windows.Window320 then 0 // 300g
            elif head_delta_abs < windows.Window300 && mean < windows.Window300 then 1 // 300
            elif head_delta_abs < windows.Window200 && mean < windows.Window200 then 2 // 200
            elif head_delta_abs < windows.Window100 && mean < windows.Window100 then 3 // 100
            else 4 // 50