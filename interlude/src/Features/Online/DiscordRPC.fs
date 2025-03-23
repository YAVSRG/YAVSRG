namespace Interlude.Features.Online

open Percyqaz.Common
open Prelude
open DiscordRPC

module DiscordRPC =

    let private client =
        new DiscordRpcClient("420320424199716864", Logger = Logging.NullLogger(), ShutdownOnly = true)

    let deinit () =
        if not client.IsDisposed then
            client.ClearPresence()
            client.Dispose()

    let init () =
        client.OnConnectionFailed.Add(fun msg ->
            Logging.Info("Discord not detected, disabling rich presence")
            deinit ()
        )
        //client.RegisterUriScheme(null, null) |> ignore
        client.Initialize() |> ignore

    let trim_long_string (string: string) : string =
        if System.Text.Encoding.UTF8.GetByteCount string <= 64 then
            string
        else
            let bytes = System.Text.Encoding.UTF8.GetBytes string
            System.Text.Encoding.UTF8.GetString(bytes, 0, 64).TrimEnd('�') + "..."

    let clear () =
        if not client.IsDisposed then
            client.ClearPresence()

    let large_image_text () : string =
        if Network.status = Network.Status.LoggedIn then Network.credentials.Username else "www.yavsrg.net"

    let in_menus (details: string) =
        if not client.IsDisposed then

            let rp =
                new RichPresence(
                    State = "In menus",
                    Details = details,
                    Assets = Assets(LargeImageKey = "icon", LargeImageText = large_image_text())
                )

            client.SetPresence(rp)

    let playing (mode: string, song: string) : unit =
        if not client.IsDisposed then

            let rp =
                new RichPresence(
                    State = mode,
                    Details = trim_long_string song,
                    Assets = Assets(LargeImageKey = "icon", LargeImageText = large_image_text())
                )

            client.SetPresence(rp)

    let playing_timed (mode: string, song: string, time_left: GameplayTime) : unit =
        if not client.IsDisposed then

            let rp =
                new RichPresence(
                    State = mode,
                    Details = trim_long_string song,
                    Assets = Assets(LargeImageKey = "icon", LargeImageText = large_image_text())
                )

            let now = System.DateTime.UtcNow
            rp.Timestamps <- Timestamps(now, now.AddMilliseconds(float time_left))
            client.SetPresence(rp)