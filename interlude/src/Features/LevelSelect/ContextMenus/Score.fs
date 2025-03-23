namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.Noteskins
open Prelude.Data.User
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Online

type ScoreContextMenu(is_leaderboard: bool, score_info: ScoreInfo) =
    inherit Page()

    override this.Content() =
        page_container()
        |+ PageButton(
            %"score.watch_replay",
            (fun () ->
                Gameplay.watch_replay (score_info, NoteColors.apply Content.NoteskinConfig.NoteColors score_info.WithMods)
                Menu.Back()
            ),
            Icon = Icons.FILM
        )
            .Pos(0)
        |+ PageButton(
            %"score.challenge",
            (fun () ->
                LevelSelect.challenge_score score_info
                Menu.Back()
            ),
            Icon = Icons.FLAG,
            Disabled = K Network.lobby.IsSome
        )
            .Help(Help.Info("score.challenge"))
            .Pos(2)
        |+ PageButton(
            %"score.delete",
            (fun () -> ScoreContextMenu.ConfirmDeleteScore(score_info, true)),
            Icon = Icons.TRASH,
            Hotkey = %%"delete"
        )
            .Pos(4)
            .Conditional(K (not is_leaderboard))
        :> Widget

    override this.Title =
        sprintf "%s | %s" score_info.Scoring.FormattedAccuracy (score_info.Ruleset.LampName score_info.Lamp)

    override this.OnClose() = ()

    static member ConfirmDeleteScore(score_info: ScoreInfo, is_submenu: bool) =
        let score_name =
            sprintf "%s | %s" score_info.Scoring.FormattedAccuracy (score_info.Ruleset.LampName score_info.Lamp)

        ConfirmPage(
            [ score_name ] %> "misc.confirmdelete",
            fun () ->
                Gameplay.delete_score score_info
                if is_submenu then
                    Menu.Back()
        )
            .Show()