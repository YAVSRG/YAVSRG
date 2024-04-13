namespace Interlude.Features.Online.Players

open Percyqaz.Flux.UI
open Interlude.UI.Menu

type PlayerListPage() as this =
    inherit Dialog()

    let contents =
        Container(NodeType.None)
        |+ PlayerListSidebar(
            Position =
                { Position.Default with
                    Right = 0.35f %+ 40.0f
                }
                    .Margin(40.0f)
        )
        |+ Profile(
            Position =
                { Position.Default with
                    Left = 0.35f %- 0.0f
                }
                    .Margin(40.0f)
        )
        |+ HotkeyAction("exit", this.Close)
        |+ HotkeyAction("player_list", this.Close)
        |+ Volume()

    override this.Init(parent) =
        base.Init parent
        contents.Init this

    override this.Draw() = contents.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        contents.Update(elapsed_ms, moved)
