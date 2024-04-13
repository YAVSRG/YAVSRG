namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type private SearchList() =
    inherit Container(NodeType.None)

    let query = Setting.simple ""

    override this.Init(parent) =
        let searcher =
            WebRequestContainer<Players.Search.Response>(
                fun this ->
                    if query.Value.Trim().Length > 0 then
                        if Network.status = Network.Status.LoggedIn then
                            Players.Search.get (
                                query.Value,
                                fun response ->
                                    sync
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
                        let contents = FlowContainer.Vertical<Widget>(60.0f)

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
                , Position = Position.TrimTop(60.0f)
            )

        this
        |+ SearchBox(query, searcher.Reload, Position = Position.TrimTop(5.0f).SliceTop(50.0f))
        |* searcher

        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        base.Draw()