namespace Interlude.Features.Online

open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type LoginPage() =
    inherit Page()

    let mutable waiting_for_browser = false

    let login () : unit =
        if not waiting_for_browser then
            waiting_for_browser <- true
            Network.begin_login ()

    let info = Callout.Small.Icon(Icons.GLOBE).Title(%"login.waiting_for_discord")

    override this.Content() =

        this.DisposeOnClose(
            NetworkEvents.successful_login
                .Subscribe(fun _ -> Menu.Back()),

            NetworkEvents.waiting_registration
                .Subscribe(fun discord_tag ->
                    waiting_for_browser <- false
                    RegisterPage(discord_tag).Show()
                ),

            NetworkEvents.login_failed
                .Subscribe(fun _ -> waiting_for_browser <- false),
            NetworkEvents.registration_failed
                .Subscribe(fun _ -> waiting_for_browser <- false)
        )

        page_container()
            .With(
                PageButton(%"login.login_with_discord", login)
                    .Pos(0),
                CalloutCard(info)
                    .Position(fun (w, h) -> Position.SliceT(400.0f, h))
                    .Conditional(fun () -> waiting_for_browser)
            )

    override this.Title = %"login"