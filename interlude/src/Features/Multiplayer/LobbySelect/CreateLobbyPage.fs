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

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"create_lobby.name", name)
            .Pos(0)
        |+ PageButton(%"confirm.yes",
            (fun () ->
                submit ()
                Menu.Back()
            ),
            Disabled = fun () -> name.Value = ""
        )
            .Pos(3)
        :> Widget

    override this.Title = %"create_lobby"