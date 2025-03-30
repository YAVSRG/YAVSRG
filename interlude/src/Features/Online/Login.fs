namespace Interlude.Features.Online

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type RegisterPage(discord_tag) =
    inherit Page()

    let username = Setting.simple ""

    let register () : unit =
        Network.complete_registration (username.Value.Trim())

    let agree_tos = Setting.simple false

    let handler = NetworkEvents.successful_login.Subscribe(fun _ -> Menu.Back())

    let info =
        Callout.Normal
            .Icon(Icons.INFO)
            .Title("Complete registration")
            .Body("Choose your username wisely! This is what gets displayed on your profile.")
            .Body("Usernames must be 3-20 characters long, and can have at most 2 special characters")
            .Body(
                "By completing your registration you confirm that:\n - You have read, understood and agree to abide by Interlude's Terms of Service and Privacy Policy\n - You are at least 13 years old"
            )

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"register.username", username).Pos(2)
        |+ Text(
            "Creating an account linked to " + discord_tag)
            .Align(Alignment.LEFT)
            .Position(page_position(0, 2, PageWidth.Full).Shrink(0.0f, 10.0f))
        |+ CalloutCard(info).Position(fun (w, h) -> page_position(4, 9, PageWidth.Custom w).Translate(0.0f, 25.0f))
        |+ PageButton(%"register.terms_of_service", (fun () -> open_url ("https://yavsrg.net/terms_of_service")))
            .Pos(14)
        |+ PageButton(%"register.privacy_policy", (fun () -> open_url ("https://yavsrg.net/privacy_policy")))
            .Pos(16)
        |+ PageSetting(%"register.confirm_terms_of_service", Checkbox agree_tos)
            .Pos(18)
        |+ PageButton(%"register.register", register, Disabled = fun () -> not agree_tos.Value || username.Value = "")
            .Pos(21)
        :> Widget

    override this.Title = %"register"
    override this.OnClose() = handler.Dispose()

type LoginPage() =
    inherit Page()

    let mutable waiting_for_browser = false

    let login () : unit =
        if not waiting_for_browser then
            waiting_for_browser <- true
            Network.begin_login ()

    let subscribed_events =
        NetworkEvents.successful_login.Subscribe(fun _ -> Menu.Back()),
        NetworkEvents.waiting_registration.Subscribe(fun discord_tag ->
            waiting_for_browser <- false
            RegisterPage(discord_tag).Show()
        ),
        NetworkEvents.login_failed.Subscribe(fun _ -> waiting_for_browser <- false),
        NetworkEvents.registration_failed.Subscribe(fun _ -> waiting_for_browser <- false)

    let info = Callout.Small.Icon(Icons.GLOBE).Title(%"login.waiting_for_discord")

    override this.Content() =
        page_container()
        |+ PageButton(%"login.login_with_discord", login).Pos(0)
        |+ (CalloutCard(info).Position(fun (w, h) -> Position.SliceT(400.0f, h)))
            .Conditional(fun () -> waiting_for_browser)
        :> Widget

    override this.Title = %"login"

    override this.OnClose() =
        let a,b,c,d = subscribed_events
        a.Dispose()
        b.Dispose()
        c.Dispose()
        d.Dispose()