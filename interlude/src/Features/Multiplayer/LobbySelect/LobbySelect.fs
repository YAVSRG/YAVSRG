namespace Interlude.Features.Multiplayer

open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Features.Online

type LobbySelectPage() =
    inherit Page()

    let lobby_list =
        LobbyList(
            Position =
                { Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y) with
                    Right = 0.7f %- (PAGE_MARGIN_X * 0.5f)
                }
        )

    let invite_list =
        InviteList(
            Position =
                { Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y) with
                    Left = 0.7f %+ (PAGE_MARGIN_X * 0.5f)
                }
        )

    override this.Content() =

        this.DisposeOnClose(
            NetworkEvents.receive_lobby_list
                .Subscribe(fun lobbies -> lobby_list.UpdateList lobbies),
            NetworkEvents.receive_invite
                .Subscribe(fun _ -> invite_list.UpdateList()),
            NetworkEvents.join_lobby
                .Subscribe(fun lobby -> Menu.Exit(); Screen.change ScreenType.Lobby Transitions.Default |> ignore)
        )

        Container(NodeType.Leaf).With(lobby_list, invite_list)

    override this.Title = %"select_lobby"