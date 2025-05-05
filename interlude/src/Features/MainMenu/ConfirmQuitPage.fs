namespace Interlude.Features.MainMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Import

type ConfirmQuitPage(on_confirm: unit -> unit) =
    inherit Page()

    override this.Content() =
        if TaskTracking.in_progress() then
            page_container()
                .With(
                    PageButton.Once(%"confirm.no", Menu.Back)
                        .Pos(7),
                    PageButton.Once(%"confirm.yes", fork on_confirm Menu.Back)
                        .Pos(5),
                    Text(%"menu.exit_prompt")
                        .Align(Alignment.LEFT)
                        .TextPos(0),
                    Text(%"menu.exit_task_warning")
                        .Color(Colors.text_red_2)
                        .Align(Alignment.LEFT)
                        .TextPos(2)
                )
        else
            page_container()
                .With(
                    PageButton.Once(%"confirm.yes", fork on_confirm Menu.Back)
                        .Pos(3),
                    PageButton.Once(%"confirm.no", Menu.Back)
                        .Pos(5),
                    Text(%"menu.exit_prompt")
                        .Align(Alignment.LEFT)
                        .TextPos(0)
                )
        :> Widget

    override this.Title = %"confirm"