module ScoreManager

open System
open System.Collections.Generic
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score
open Prelude.Gameplay.Mods

//todo: maybe migrate this format to something better
type Score = {
    time: DateTime
    hitdata: string
    player: string
    playerUUID: string
    rate: float
    selectedMods: ModState
    layout: unit
    keycount: int
}

type ChartSaveData = {
    Path: string
    Offset: float
    Scores: List<Score>
    Lamp: Dictionary<string, Lamp>
    Accuracy: Dictionary<string, float>
    Clear: Dictionary<string, bool>
}
(*
    Gameplay pipelines that need to happen to play a chart
    Chart -> Modified chart -> Colorized chart
                            -> Replay data -> Mod replay data
                            -> Difficulty rating data
*)
let createModifiedChart (chart: Chart) (mods: ModState) = ()

type ScoreInfoProvider(score: Score, chart: Chart) =
    let hitdata = lazy (decompressScoreData score.hitdata score.keycount)
