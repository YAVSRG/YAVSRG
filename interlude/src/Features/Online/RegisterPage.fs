namespace Interlude.Features.Online

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type RegisterPage(discord_tag: string) =
    inherit Page()

    let username = Setting.simple ""

    let register () : unit =
        Network.complete_registration (username.Value.Trim())

    let agree_tos = Setting.simple false

    let handler = NetworkEvents.successful_login.Subscribe(fun _ -> Menu.Back())

    let info =
        Callout.Normal
            .Icon(Icons.INFO)
            .Title(%"register.signup_prompt.title")
            .Body(%"register.signup_prompt.a")
            .Body(%"register.signup_prompt.b")
            .Body(%"register.signup_prompt.c")

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"register.username", username).Pos(2)
        |+ Text([discord_tag] %> "register.discord_tag")
            .Align(Alignment.LEFT)
            .TextPos(0)
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