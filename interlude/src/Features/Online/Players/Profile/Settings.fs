namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type ProfileData = Players.Profile.View.Response

type private ProfileSettingsPage(profile: ProfileData, profile_color: Setting<int32>) =
    inherit Page()

    let color_picker =
        DropdownWrapper(fun d -> Position.BorderB(min d.Height 500.0f + 2.0f * Style.PADDING).Shrink(Style.PADDING))

    let badges =
        seq {
            for b in profile.Badges do
                for c in b.Colors do
                    yield (b.Name, c)
        } |> Array.ofSeq

    let change_color_dropdown() =

        let save_color color =
            Players.Profile.Options.post (
                { Color = color },
                function
                | Some true -> GameThread.defer <| fun () -> profile_color.Set color
                | _ -> Notifications.error (%"notification.network_action_failed", "")
            )

        let dropdown =
            Dropdown
                {
                    Items = badges |> Seq.map (fun (label, color) -> color, label)
                    ColorFunc = fun c -> Color.FromArgb c, Colors.shadow_2
                    Setting = Setting.make save_color profile_color.Get
                }

        color_picker.Show dropdown
        dropdown.Focus false

    override this.Content() =
        page_container()
        |+ PageSetting(
            %"profile_settings.color",
            Button(profile.Username, change_color_dropdown)
                .Align(Alignment.LEFT)
                .TextColor(fun () -> Color.FromArgb(profile_color.Value))
            |+ color_picker
        )
            .Pos(0)
        :> Widget

    override this.Title = %"profile_settings"