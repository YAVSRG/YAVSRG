namespace Interlude.Features.Multiplayer

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.Features.Online

type InviteCard(invite: LobbyInvite) =
    inherit FrameContainer(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text(Icons.MAIL + " " + invite.InvitedBy, Position = Position.Shrink(5.0f), Align = Alignment.LEFT)
        |+ Button(
            Icons.CHECK,
            (fun () ->
                GameThread.defer (fun () -> (this.Parent :?> FlowContainer.Vertical<InviteCard>).Remove this)
                Network.join_lobby invite.LobbyId
            )).Position(Position.ShrinkR(50.0f).SliceR(50.0f))
        |* Button(
            Icons.X,
            (fun () -> GameThread.defer (fun () -> (this.Parent :?> FlowContainer.Vertical<InviteCard>).Remove this))).Position(Position.SliceR(50.0f))

        base.Init parent

type InviteList() =
    inherit Container(NodeType.None)

    let container =
        FlowContainer.Vertical<InviteCard>(50.0f, Spacing = 10.0f).Position(Position.Shrink(0.0f, 80.0f))

    member this.UpdateList() =
        container.Clear()

        for l in Network.lobby_invites do
            container.Add(InviteCard l)

        //if
        //    match Screen.currentType with
        //    | Screen.Type.LevelSelect
        //    | Screen.Type.Import
        //    | Screen.Type.MainMenu -> true
        //    | _ -> false
        //then
        //    Notifications.add([name] %> "notification.invited_to_lobby.title", NotificationType.Info)

    override this.Init(parent) =
        this |* container
        this.UpdateList()
        base.Init parent