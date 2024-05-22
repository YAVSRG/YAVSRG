namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data
open Prelude.Data.Library.Caching
open Prelude.Charts.Processing
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Score

type ScoreContextMenu(score_info: ScoreInfo) =
    inherit Page()

    override this.Content() = 
        page_container()
        |+ PageButton(
            "score.delete",
            (fun () -> ScoreContextMenu.ConfirmDeleteScore(score_info, true)),
            Icon = Icons.TRASH
        )
            .Pos(0)
        |+ PageButton(
            "score.watch_replay",
            (fun () ->
                Gameplay.watch_replay (score_info, NoteColors.apply Content.NoteskinConfig.NoteColors score_info.WithMods)
                Menu.Back()
            ),
            Icon = Icons.FILM
        )
            .Pos(2)
        |+ PageButton(
            "score.challenge",
            (fun () ->
                LevelSelect.challenge_score score_info
                Menu.Back()
            ),
            Icon = Icons.FLAG,
            Enabled = Network.lobby.IsNone
        )
            .Tooltip(Tooltip.Info("score.challenge"))
            .Pos(4)
        :> Widget

    override this.Title =
        sprintf "%s | %s" (score_info.Scoring.FormatAccuracy()) (score_info.Ruleset.LampName score_info.Lamp)

    override this.OnClose() = ()

    static member ConfirmDeleteScore(score_info, is_submenu) =
        let score_name =
            sprintf "%s | %s" (score_info.Scoring.FormatAccuracy()) (score_info.Ruleset.LampName score_info.Lamp)

        ConfirmPage(
            [ score_name ] %> "misc.confirmdelete",
            fun () ->
                if ScoreDatabase.delete_score score_info.CachedChart.Hash score_info.TimePlayed Content.Scores then
                    LevelSelect.refresh_all ()
                    Notifications.action_feedback (Icons.TRASH, [ score_name ] %> "notification.deleted", "")
                else
                    Logging.Debug("Couldn't find score matching timestamp to delete")

                if is_submenu then
                    Menu.Back()
        )
            .Show()
