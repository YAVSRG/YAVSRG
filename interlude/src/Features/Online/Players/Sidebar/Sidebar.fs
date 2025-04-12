namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type PlayerListSidebar =

    static let selected_tab = Setting.simple 0

    static member Create() =
        let list_container = SwapContainer()

        let tab_options : (Widget * string) array =
            [|
                OnlineList(), %"online.players.online"
                FriendList(), %"online.players.friends"
                SearchList.Create(), %"online.players.search"
            |]

        NavigationContainer.Column()
            .With(
                TabButtons.CreatePersistent(tab_options, list_container, selected_tab)
                    .Position(Position.SliceT(TabButtons.HEIGHT).TranslateY(Style.PADDING)),
                list_container
                    .Position(Position.ShrinkT(TabButtons.HEIGHT + Style.PADDING * 2.0f).ShrinkB(40.0f))
            )