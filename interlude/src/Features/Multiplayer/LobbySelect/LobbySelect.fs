namespace Interlude.Features.Multiplayer

open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Online

type LobbySelectPage() =
    inherit Page()

    let lobby_list = 
        LobbyList(
            Position =
                { Position.Default with
                    Right = 0.7f %+ 0.0f
                }
                    .Margin(200.0f, 100.0f)
        )

    let invite_list =
        InviteList(
            Position =
                { Position.Default with
                    Left = 0.7f %+ 0.0f
                }
                    .Margin(100.0f, 100.0f)
        )

    let subscribed_events =
        NetworkEvents.receive_lobby_list.Subscribe (fun lobbies -> lobby_list.UpdateList lobbies),
        NetworkEvents.receive_invite.Subscribe (fun _ -> invite_list.UpdateList()),
        NetworkEvents.join_lobby.Subscribe (fun lobby -> Menu.Exit(); Screen.change Screen.Type.Lobby Transitions.Flags.Default |> ignore)

    override this.Content() =
        Container(NodeType.Leaf)
        |+ lobby_list
        |+ invite_list
        :> Widget

    override this.Title = %"select_lobby"
    override this.OnClose() =
        let a, b, c = subscribed_events
        a.Dispose()
        b.Dispose()
        c.Dispose()