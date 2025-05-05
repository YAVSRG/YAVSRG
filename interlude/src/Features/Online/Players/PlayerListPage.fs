namespace Interlude.Features.Online.Players

open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type PlayerListPage() =
    inherit Page()

    let SIDEBAR_PERCENTAGE_SPLIT = 0.35f
    let PADDING = 40.0f

    override this.Content() =
        NavigationContainer.Row()
            .With(
                PlayerListSidebar.Create()
                    .Position(Position.SlicePercentL(SIDEBAR_PERCENTAGE_SPLIT).Shrink(PADDING).ExpandR(PADDING)),
                Profile()
                    .Position(Position.ShrinkPercentL(SIDEBAR_PERCENTAGE_SPLIT).Shrink(PADDING)),

                HotkeyListener("exit", Menu.Back),
                HotkeyListener("player_list", Menu.Back)
            )

    override this.Title = %"network.players"
    override this.Header() = Dummy()