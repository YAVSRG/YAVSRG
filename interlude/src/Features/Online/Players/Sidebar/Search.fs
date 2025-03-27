namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

module SearchList =

    let create () : NavigationContainer.Column =

        let query = Setting.simple ""
        let searcher =
            WebRequestContainer<Players.Search.Response>(
                fun this ->
                    if query.Value.Trim().Length > 0 then
                        if Network.status = Network.Status.LoggedIn then
                            Players.Search.get (
                                query.Value,
                                fun response ->
                                    GameThread.defer
                                    <| fun () ->
                                        match response with
                                        | Some result -> this.SetData result
                                        | None -> this.ServerError()
                            )
                        else
                            this.Offline()
                    else
                        this.SetData { Matches = [||] }
                , fun _ data ->
                    if data.Matches.Length > 0 then
                        let contents = FlowContainer.Vertical<Widget>(60.0f, Spacing = Style.PADDING)

                        for player in data.Matches do
                            contents.Add(PlayerButton(player.Username, player.Color))

                        ScrollContainer(contents) :> Widget
                    else
                        EmptyState(
                            Icons.SEARCH,
                            if query.Value.Trim().Length > 0 then
                                %"online.players.search.no_results"
                            else
                                %"online.players.search.empty_search_bar"
                        )
                )
                .Position(Position.ShrinkT(60.0f))

        NavigationContainer.Column()
        |+ SearchBox(query, (fun (_: string) -> searcher.Reload()))
            .Position(Position.Shrink(5.0f).SliceT(50.0f))
        |+ searcher