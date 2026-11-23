namespace Interlude.Features.Online.Players

type PlayerListContext =
    {
        mutable CurrentPlayer: string option
        PlayerChanged: Event<unit>
        FriendsChanged: Event<unit>
    }

    member this.OnPlayerChanged = this.PlayerChanged.Publish
    member this.OnFriendsChanged = this.FriendsChanged.Publish

    member this.SwitchPlayer(player: string) : unit =
        this.CurrentPlayer <- Some player
        this.PlayerChanged.Trigger()

    static member Create() : PlayerListContext =
        { CurrentPlayer = None; PlayerChanged = Event<unit>(); FriendsChanged = Event<unit>() }
