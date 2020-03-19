module Prelude.Gameplay.Mods

open System.Collections.Generic
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score

//ideas
type ModConfig = int
(*
    Marker for status of mods.
        0 = This is a non-silly mod suitable for online upload, personal bests, leaderboards
        1 = This is a for-fun mod that may transform a chart in large ways or are otherwise not suitable for leaderboards e.g. randomiser
        2 = Scores with this mod enabled should not be saved at all e.g. when testing/developing
*)
type ModStatus = Ranked = 0 | Unranked = 1 | Unstable = 2

(*
    Mods (Modifiers) are a set of optional effects that can be enabled for a chart before playing it
    They will change something about the chart that can vary gameplay in interesting ways
    Mods will have an integer "state" passed to them when they are applied - This allows some sub-behaviours within mods

    Rules to use this correctly
        - 'check' and 'apply_score' should not modify anything about the Chart passed
        - 'apply' should not modify the header of the Chart passed
*)
type Mod
    (status : ModStatus, numStates : int, check : ModConfig -> Chart -> bool,
        apply : ModConfig -> Chart -> unit, apply_score : ModConfig -> Chart -> ScoreData -> unit) = 
    member this.Status = status
    //Looks at a chart and returns true if applying the modifier will do anything, otherwise false
    //Used to hide enabled mods that would have no effect e.g. a mod that removes hold notes on a chart with no hold notes
    member this.CheckApplicable = check
    //Applies the modifier to the content of the chart. This is where note data, timing data, sv data should be edited to create the desired effects
    member this.ApplyChart = apply
    //Applies the modifier to the hit data of the chart - This hit data maps out what notes need to be hit/what notes have been hit
    //This typically is unused - examples uses are marking all notes as hit perfectly in Autoplay or marking LN releases as not needing to be hit
    member this.ApplyHitData = apply_score