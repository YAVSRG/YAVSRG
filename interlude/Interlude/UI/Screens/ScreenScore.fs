namespace Interlude.UI

open OpenTK
open Prelude.Data.ScoreManager
open Interlude.Utils
open Interlude.UI.Components 

type ScreenScore(scoreData: ScoreInfoProvider) as this =
    inherit Screen()

    do
        this.Add(new TextBox(K <| scoreData.Scoring.Format(), K Color.White, 0.5f))

    override this.OnEnter(prev) =
        () //add score to database, update pbs, all that