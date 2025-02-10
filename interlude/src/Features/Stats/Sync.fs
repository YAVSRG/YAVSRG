namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Prelude.Data.User.Stats
open Interlude.Web.Shared.Requests
open Interlude.Features.Online
open Interlude.Features.Gameplay

module StatsSync =

    let upload_online_stats () =
        if Network.status = Network.LoggedIn then
            match StatsSyncUpstream.Create () with
            | None -> ()
            | Some upstream_data ->
                Stats.Sync.post (
                    (upstream_data),
                    (function
                        | Some true -> ()
                        | Some false -> Logging.Error "Error submitting stats (Server indicated error)"
                        | None -> Logging.Error "Error submitting stats"
                    )
                )

    let sync_online_stats () =
        Stats.Fetch.get
            (function
                | Some data ->
                    GameThread.defer (fun () ->
                        if data.Accept then
                            Logging.Debug "Syncing stats with online server ...";
                            upload_online_stats ()
                    )
                | None -> Logging.Error "Error fetching online stats"
            )

    let init () =
        Gameplay.score_saved.Add (fun _ -> upload_online_stats())
        NetworkEvents.successful_login.Add (fun _ -> sync_online_stats())