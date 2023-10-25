namespace Interlude.Features.Online

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Prelude.Common
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Web.Shared.API
open Interlude.Web.Shared.Requests

type private WebRequestState =
    | Offline = 0
    | Loading = 1
    | ServerError = 2
    | Loaded = 3

type private OnlineList() =
    inherit StaticContainer(NodeType.None)

    let mutable status = WebRequestState.Loading

    override this.Init(parent) =

        let contents = FlowContainer.Vertical<Widget>(50.0f)

        if Network.status = Network.Status.LoggedIn then
            Players.Online.get(
                fun response -> sync <| fun () ->
                    match response with
                    | Some data ->
                        status <- WebRequestState.Loaded
                        for player in data.Players do
                            contents.Add (
                                // todo: button
                                StaticContainer(NodeType.None)
                                |+ Text(player.Username, Color = K (Color.FromArgb player.Color, Colors.shadow_2), Align = Alignment.LEFT, Position = Position.Margin(20.0f, 5.0f))
                            )
                    | None -> status <- WebRequestState.ServerError
            )
        else status <- WebRequestState.Offline

        this
        |+ Conditional((fun () -> status = WebRequestState.Loading), LoadingState())
        |+ Conditional((fun () -> status = WebRequestState.Offline), EmptyState(Icons.connected, L"misc.offline"))
        |+ Conditional((fun () -> status = WebRequestState.ServerError), EmptyState(Icons.connected, L"misc.server_error"))
        |* ScrollContainer.Flow(contents)

        base.Init parent

type private FriendList() =
    inherit StaticContainer(NodeType.None)

    let mutable status = WebRequestState.Loading

    override this.Init(parent) =

        let contents = FlowContainer.Vertical<Widget>(50.0f)

        if Network.status = Network.Status.LoggedIn then
            Friends.List.get(
                fun response -> sync <| fun () ->
                    match response with
                    | Some data ->
                        status <- WebRequestState.Loaded
                        for player in data.Friends do
                            contents.Add (
                                // todo: button
                                StaticContainer(NodeType.None)
                                |+ Text(player.Username, Color = K (Color.FromArgb player.Color, Colors.shadow_2), Align = Alignment.LEFT, Position = Position.Margin(20.0f, 5.0f))
                            )
                    | None -> status <- WebRequestState.ServerError
            )
        else status <- WebRequestState.Offline

        this
        |+ Conditional((fun () -> status = WebRequestState.Loading), LoadingState())
        |+ Conditional((fun () -> status = WebRequestState.Offline), EmptyState(Icons.connected, L"misc.offline"))
        |+ Conditional((fun () -> status = WebRequestState.ServerError), EmptyState(Icons.connected, L"misc.server_error"))
        |* ScrollContainer.Flow(contents)

        base.Init parent

type PlayerList() =
    inherit StaticContainer(NodeType.None)

    let online = OnlineList()
    let friends = FriendList()

    let swap = SwapContainer(Current = online, Position = Position.TrimTop 50.0f)

    let button(label: string, cmp) =
        Frame(NodeType.None, Border = K Color.Transparent, Fill = fun () -> if swap.Current = cmp then !*Palette.DARK_100 else Colors.black.O2)
        |+ Button(label, fun () -> swap.Current <- cmp)

    override this.Init(parent) =
        this
        |+ 
            (
                FlowContainer.LeftToRight(200.0f, Position = Position.SliceTop(50.0f))
                |+ button(L"online.players.online", online)
                |+ button(L"online.players.friends", friends)
            )
        |* swap
        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.TrimTop(50.0f)) !*Palette.DARK_100
        base.Draw()

type Profile = Dummy

type PlayersPage() as this =
    inherit Dialog()

    let contents =
        StaticContainer(NodeType.None)
        |+ PlayerList(Position = { Position.Default with Right = 0.35f %+ 0.0f }.Margin(40.0f))
        |+ Profile(Position = { Position.Default with Left = 0.35f %- 40.0f }.Margin(40.0f))
        |+ HotkeyAction("exit", this.Close)

    override this.Init(parent) =
        base.Init parent
        contents.Init this

    override this.Draw() =
        contents.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        contents.Update(elapsedTime, moved)