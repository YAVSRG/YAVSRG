namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Interlude.UI

module PlayerListSidebar =

    let create (position: Position) =
        let online = OnlineList()
        let friends = FriendList()
        let search = SearchList.create()

        let tabs =
            SwapContainer(online)
                .Position(Position.ShrinkT(60.0f).ShrinkB(40.0f))

        let tab_buttons =
            RadioButtons.create_tabs
                {
                    Setting = Setting.make (fun v -> tabs.Current <- v; Input.remove_listener()) tabs.get_Current
                    Options =
                        [|
                            online, %"online.players.online", K false
                            friends, %"online.players.friends", K false
                            search, %"online.players.search", K false
                        |]
                    Height = 50.0f
                }

        tab_buttons.Position <- Position.SliceT(50.0f).TranslateY(Style.PADDING)

        NavigationContainer.Column(Position = position)
        |+ tab_buttons
        |+ tabs