namespace Interlude.Features.Online

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu

type RegisterPage(discord_tag) as this =
    inherit Page()

    let username = Setting.simple ""
    let register() = Network.complete_registration(username.Value.Trim())
    let submit_button = PageButton("register.register", register, Enabled = false)

    let agree_tos = Setting.simple false |> Setting.trigger submit_button.set_Enabled
    
    let handler = Network.Events.successful_login.Subscribe(fun _ -> Menu.Back())
    
    let info = 
        Callout.Normal
            .Icon(Icons.info)
            .Title("Complete registration")
            .Body("Choose your username wisely! This is what gets displayed on your profile.")
            .Body("Usernames must be 3-20 characters long, and can have at most 2 special characters")
            .Body("By completing your registration you confirm that:\n You have read, understood and agree to abide by Interlude's Terms of Service and Privacy Policy\n You are 18 years old or older")

    do
        this.Content(
            column()
            |+ PageTextEntry("register.username", username).Pos(250.0f)
            |+ Text("Creating an account linked to " + discord_tag, Position = Position.Box(0.0f, 0.0f, 100.0f, 150.0f, PRETTYWIDTH, PRETTYHEIGHT), Align = Alignment.LEFT)
            |+ Callout.frame info (fun (w, h) -> Position.Row(340.0f, h + 40.0f).Margin(100.0f, 0.0f))
            |+ PageButton("register.terms_of_service", fun () -> openUrl("https://yavsrg.net/terms_of_service")).Pos(650.0f)
            |+ PageButton("register.privacy_policy", fun () -> openUrl("https://yavsrg.net/privacy_policy")).Pos(720.0f)
            |+ PageSetting("register.confirm_terms_of_service", Selector<_>.FromBool agree_tos).Pos(790.0f)
            |+ submit_button.Pos(890.0f)
        )
    
    override this.Title = L"register.name"
    override this.OnClose() = handler.Dispose()

type LoginPage() as this =
    inherit Page()

    let mutable waiting_for_browser = false

    let login() = 
        if not waiting_for_browser then
            waiting_for_browser <- true
            Network.begin_login()
    let register() =
        if not waiting_for_browser then
            waiting_for_browser <- true
            Network.begin_registration()

    let a = Network.Events.successful_login.Subscribe(fun _ -> Menu.Back())
    let b = Network.Events.waiting_registration.Subscribe(fun discord_tag -> waiting_for_browser <- false; RegisterPage(discord_tag).Show())
    let c = Network.Events.login_failed.Subscribe(fun _ -> waiting_for_browser <- false)
    let d = Network.Events.registration_failed.Subscribe(fun _ -> waiting_for_browser <- false)

    let info = Callout.Small.Icon(Icons.connected).Title(L"login.waiting_for_discord")

    do
        this.Content(
            column()
            |+ PageButton("login.login_with_discord", login)
                .Pos(200.0f)
            |+ PageButton("login.register_with_discord", register)
                .Pos(300.0f)
            |+ Conditional((fun () -> waiting_for_browser),
                    Callout.frame info (fun (w, h) -> Position.Row(400.0f, h + 40.0f).Margin(100.0f, 0.0f))
                )
        )

    override this.Title = L"login.name"
    override this.OnClose() = a.Dispose(); b.Dispose(); c.Dispose(); d.Dispose()