namespace Interlude.Features.Multiplayer

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.Features.Online

type InviteCard(invite: LobbyInvite) =
    inherit FrameContainer(NodeType.None)

    let dismiss_invite (this: InviteCard) : unit =
        GameThread.defer (fun () -> (this.Parent :?> FlowContainer.Vertical<InviteCard>).Remove this |> ignore)

    let accept_invite (this: InviteCard) : unit =
        dismiss_invite(this)
        Network.join_lobby invite.LobbyId

    let BUTTON_SIZE = 50.0f

    override this.Init(parent: Widget) =
        this
            .Add(
                Text(Icons.MAIL + " " + invite.InvitedBy)
                    .Align(Alignment.LEFT)
                    .Position(Position.Shrink(5.0f)),
                Button(Icons.CHECK, fun () -> accept_invite this)
                    .Position(Position.SliceR(BUTTON_SIZE, BUTTON_SIZE)),
                Button(Icons.X, fun () -> dismiss_invite this)
                    .Position(Position.SliceR(BUTTON_SIZE))
            )

        base.Init parent

type InviteList() =
    inherit Container(NodeType.None)

    let container =
        FlowContainer.Vertical<InviteCard>(50.0f)
            .Spacing(Style.PADDING * 3.0f)
            .Position(Position.ShrinkY(80.0f))

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