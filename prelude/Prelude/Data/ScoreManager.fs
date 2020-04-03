module ScoreManager

open System.Collections.Generic
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score

type ChartSaveData = {
    Path: string
    Offset: float
    Scores: List<Score>
    Lamp: Dictionary<string, Lamp>
    Accuracy: Dictionary<string, float>
    Clear: Dictionary<string, bool>
}

type ScoreInfoProvider(score: Score, chart: Chart) =
    let hitdata = lazy (decompressScoreData score.hitdata score.keycount)
