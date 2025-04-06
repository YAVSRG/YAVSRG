namespace Interlude.Web.Server.API.Stats

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module Fetch =

    open Stats.Fetch

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, _ = authorize headers

            let stats = Stats.get_or_default user_id
            let data: Response =
                {
                    NetworkId = user_id
                    PlayTime = stats.Playtime
                    PracticeTime = stats.PracticeTime
                    GameTime = stats.GameTime
                    NotesHit = stats.NotesHit
                    XP = stats.XP
                    KeymodePlaytime = Map.ofSeq [4, stats._4KPlaytime; 7, stats._7KPlaytime; 10, stats._10KPlaytime]
                }

            response.ReplyJson(data)
        }