namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Online

type ScoreContextMenu(is_leaderboard: bool, score_info: ScoreInfo) =
    inherit Page()

    override this.Content() =
        page_container()
            .With(
                PageButton(%"score.watch_replay", fun () ->
                    Gameplay.watch_replay(score_info)
                    Menu.Back()
                )
                    .Icon(Icons.FILM)
                    .Pos(0),
                PageButton(%"score.challenge", fun () ->
                    LevelSelect.challenge_score(score_info)
                    Menu.Back()
                )
                    .Icon(Icons.FLAG)
                    .Disabled(Network.lobby.IsSome)
                    .Help(Help.Info("score.challenge"))
                    .Pos(2),
                PageButton(%"score.use_mods", fun () ->
                    SelectedChart.set_rate_and_mods(score_info.Rate, score_info.Mods)
                    Menu.Back()
                )
                    .Icon(Icons.ZAP)
                    .Disabled(score_info.Rate = SelectedChart.rate.Value && score_info.Mods = Map.empty)
                    .Pos(4),
                PageButton(%"score.delete", fun () ->
                    ScoreContextMenu.ConfirmDeleteScore(score_info, true)
                )
                    .TextColor(Colors.red_accent)
                    .Icon(Icons.TRASH)
                    .Hotkey("delete")
                    .Pos(6)
                    .Conditional(K (not is_leaderboard))
            )

    override this.Title = score_info.Shorthand

    static member ConfirmDeleteScore(score_info: ScoreInfo, is_submenu: bool) =

        ConfirmPage(
            [ score_info.Shorthand ] %> "misc.confirmdelete",
            fun () ->
                Gameplay.delete_score score_info
                if is_submenu then
                    Menu.Back()
        )
            .Show()