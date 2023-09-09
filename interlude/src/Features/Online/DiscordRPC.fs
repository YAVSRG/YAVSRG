namespace Interlude.Features.Online

open Percyqaz.Common
open DiscordRPC

module DiscordRPC =

    let private client = new DiscordRpcClient("420320424199716864", Logger = Logging.ConsoleLogger())

    let init() =

        client.OnReady.Add (fun msg -> Logging.Info("Connected to discord rich presence"))
        //client.RegisterUriScheme(null, null) |> ignore
        client.Initialize() |> ignore

    let in_menus(details: string) =
        let rp = new RichPresence(
            State = "In menus",
            Details = details,
            Assets = new Assets(SmallImageKey = "logo"))
        client.SetPresence(rp)

    let playing(mode: string, song: string) =
        let rp = new RichPresence(
            State = mode,
            Details = (if song.Length > 48 then song.Substring(0, 44) + " ..." else song),
            Assets = new Assets(SmallImageKey = "logo"))
        client.SetPresence(rp)

    let playing_timed(mode: string, song: string, time_left: Time) =
        let rp = new RichPresence(
            State = mode,
            Details = (if song.Length > 48 then song.Substring(0, 44) + " ..." else song),
            Assets = new Assets(SmallImageKey = "logo"))
        let now = System.DateTime.UtcNow
        rp.Timestamps <- Timestamps(now, now.AddMilliseconds(float time_left))
        client.SetPresence(rp)

    let shutdown() =
        client.ClearPresence()
        client.Dispose()