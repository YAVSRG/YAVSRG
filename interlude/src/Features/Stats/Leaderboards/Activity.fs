namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

type private ActivityLeaderboard() =
    inherit Container(NodeType.None)

    let by_playtime = Setting.simple false

    let load (container: WebRequestContainer<_>) =
        if Network.status = Network.Status.LoggedIn then
            Stats.Leaderboard.MonthlyXP.get (
                by_playtime.Value,
                fun response ->
                    GameThread.defer
                    <| fun () ->
                        match response with
                        | Some result -> container.SetData result
                        | None -> container.ServerError()
            )
        else
            container.Offline()

    let rerender (container: WebRequestContainer<_>) (data: Stats.Leaderboard.MonthlyXP.Response) =

        let flow = FlowContainer.Vertical<Widget>(60.0f)
        for d in data.Leaderboard do
            Container(NodeType.None)
            |+ Text(d.Username, Color = K (Color.FromArgb(d.Color), Colors.shadow_2), Align = Alignment.LEFT)
            |+ Text(format_long_time d.Playtime, Align = Alignment.RIGHT)
            |+ Text(current_level d.XP |> sprintf "%i", Align = Alignment.CENTER)
            |> flow.Add

        ScrollContainer(flow)
        :> Widget

    let container = WebRequestContainer<Stats.Leaderboard.MonthlyXP.Response>(load, rerender)

    let by_playtime = by_playtime |> Setting.trigger (fun _ -> container.Reload())

    member this.SortByPlaytime = by_playtime

    override this.Init(parent) =
        this
        |* container
        base.Init parent

    override this.Draw() =
        Render.rect this.Bounds !*Palette.DARK_100
        base.Draw()