namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Prelude
open Interlude.UI.Menu
open Interlude.Features.Online

type CreateLobbyPage() as this =
    inherit Page()

    let name = Setting.simple (Network.credentials.Username + "'s Lobby") |> Setting.alphanumeric
    let submit () = Lobby.create name.Value

    let submit_button =
        PageButton(
            "confirm.yes",
            (fun () ->
                submit ()
                Menu.Back()
            )
        )

    do
        this.Content(
            page_container()
            |+ PageTextEntry(
                "create_lobby.name",
                name |> Setting.trigger (fun s -> submit_button.Enabled <- s.Length > 0)
            )
                .Pos(0)
            |+ submit_button.Pos(3)
        )

    override this.Title = %"create_lobby.name"
    override this.OnClose() = ()