module ScoreManager

open Prelude.Charts.Interlude
open Prelude.Gameplay.Score

type ChartSaveData = {
    Path: string
    Offset: float
    Scores: List<Score>
    //Lamps
    //Accuracies
    //Clears
}

type ScoreInfoProvider(score: Score, chart: Chart) =
    let hitdata = lazy (decompressScoreData score.hitdata score.keycount)
