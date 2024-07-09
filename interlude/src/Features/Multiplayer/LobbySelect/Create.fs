namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.Web.Shared
open Interlude.Features.Online
open Interlude.UI

type CreateLobbyPage() =
    inherit Page()

    let name = Setting.simple (Network.credentials.Username + "'s lobby") |> Setting.alphanumeric
    let submit () = Network.client.Send(Upstream.CREATE_LOBBY name.Value)

    let submit_button =
        PageButton(
            %"confirm.yes",
            (fun () ->
                submit ()
                Menu.Back()
            )
        )

    override this.Content() = 
        page_container()
        |+ PageTextEntry(
            %"create_lobby.name",
            name |> Setting.trigger (fun s -> submit_button.Enabled <- s.Length > 0)
        )
            .Pos(0)
        |+ submit_button.Pos(3)
        :> Widget

    override this.Title = %"create_lobby"
    override this.OnClose() = ()