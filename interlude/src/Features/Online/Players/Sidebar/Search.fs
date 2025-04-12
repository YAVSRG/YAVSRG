namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type SearchList =

    static let SEARCH_BOX_HEIGHT = 50.0f

    static member Create() : NavigationContainer.Column =

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
                        FlowContainer.Vertical<PlayerButton>(PlayerButton.HEIGHT)
                            .Spacing(Style.PADDING)
                            .With(seq{
                                for player in data.Matches do
                                    yield PlayerButton(player.Username, player.Color)
                            })
                        |> ScrollContainer
                        :> Widget
                    else
                        EmptyState(
                            Icons.SEARCH,
                            if query.Value.Trim().Length > 0 then
                                %"online.players.search.no_results"
                            else
                                %"online.players.search.empty_search_bar"
                        )
                )

        NavigationContainer.Column()
            .With(
                SearchBox(query, (fun (_: string) -> searcher.Reload()))
                    .Position(Position.Shrink(5.0f).SliceT(SEARCH_BOX_HEIGHT)),
                searcher
                    .Position(Position.ShrinkT(SEARCH_BOX_HEIGHT + Style.PADDING * 2.0f))
            )