namespace Interlude.Features.Online

open Percyqaz.Common
open DiscordRPC

module DiscordRPC =

    let private client = new DiscordRpcClient("420320424199716864", Logger = Logging.NullLogger())
    
    let deinit () =
        if not client.IsDisposed then
            client.ClearPresence()
            client.Dispose()

    let init_window () =
        client.OnConnectionFailed.Add (fun msg -> 
            Logging.Info("Discord not detected, disabling rich presence")
            deinit())
        //client.RegisterUriScheme(null, null) |> ignore
        client.Initialize() |> ignore

    let in_menus (details: string) =
        if client.IsDisposed then () else

        let rp =
            new RichPresence(State = "In menus", Details = details)

        client.SetPresence(rp)

    let playing (mode: string, song: string) =
        if client.IsDisposed then () else

        let rp =
            new RichPresence(
                State = mode,
                Details =
                    (if song.Length > 48 then
                         song.Substring(0, 44) + " ..."
                     else
                         song)
            )

        client.SetPresence(rp)

    let playing_timed (mode: string, song: string, time_left: Time) =
        if client.IsDisposed then () else

        let rp =
            new RichPresence(
                State = mode,
                Details =
                    (if song.Length > 48 then
                         song.Substring(0, 44) + " ..."
                     else
                         song)
            )

        let now = System.DateTime.UtcNow
        rp.Timestamps <- Timestamps(now, now.AddMilliseconds(float time_left))
        client.SetPresence(rp)
