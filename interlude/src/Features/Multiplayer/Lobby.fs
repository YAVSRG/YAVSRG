namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Interlude.UI.Components
open Interlude.Web.Shared

type LobbyInfoCard(info: LobbyInfo) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text(info.Name, Position = Position.SliceTop 50.0f, Align = Alignment.LEFT)
        |+ Text((match info.CurrentlyPlaying with None -> "--" | Some s -> s), Color = Style.text_subheading, Position = Position.SliceBottom 30.0f, Align = Alignment.LEFT)
        |* Text(sprintf "%i players" info.Players, Color = Style.text_subheading, Position = Position.SliceTop(50.0f).Margin(10.0f), Align = Alignment.RIGHT)
        base.Init parent

    member this.Name = info.Name

type LobbyList() =
    inherit StaticContainer(NodeType.None)

    let searchtext = Setting.simple ""

    let container = FlowContainer.Vertical<LobbyInfoCard>(80.0f, Position = Position.TrimTop 80.0f)

    let refresh() =
        container.Clear()
        for l in Network.lobby_list do
            container.Add(LobbyInfoCard l)

    override this.Init(parent) =
        this
        |+ container
        |* SearchBox(searchtext, fun () -> container.Filter <- fun l -> l.Name.ToLower().Contains searchtext.Value)
        
        base.Init parent