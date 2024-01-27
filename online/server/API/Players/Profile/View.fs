namespace Interlude.Web.Server.API.Players.Profile

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module View =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, user = authorize headers

            let target_user_id, target_user =
                if query_params.ContainsKey("user") then
                    match User.by_username query_params.["user"].[0] with
                    | Some(userId, user) -> userId, user
                    | None -> raise NotFoundException
                else
                    user_id, user

            let recent_scores = Score.get_user_recent target_user_id

            let scores: Players.Profile.View.RecentScore array =
                recent_scores
                |> Array.map (fun s ->
                    let rs = Backbeat.rulesets.[Score.PRIMARY_RULESET]

                    match Backbeat.Charts.by_hash s.ChartId with
                    | Some(chart, song) ->
                        {
                            Artist = song.FormattedArtists
                            Title = song.Title
                            Difficulty = chart.DifficultyName
                            Score = s.Accuracy
                            Lamp = rs.LampName s.Lamp
                            Mods =
                                if s.Mods.IsEmpty then
                                    sprintf "%.2fx" s.Rate
                                else
                                    sprintf "%.2fx*" s.Rate
                            Timestamp = s.TimePlayed
                        }
                    | None ->
                        {
                            Artist = "???"
                            Title = "???"
                            Difficulty = "???"
                            Score = s.Accuracy
                            Lamp = rs.LampName s.Lamp
                            Mods =
                                if s.Mods.IsEmpty then
                                    sprintf "%.2fx" s.Rate
                                else
                                    sprintf "%.2fx*" s.Rate
                            Timestamp = s.TimePlayed
                        }
                )

            let relation = Friends.relation (user_id, target_user_id)

            response.ReplyJson(
                {
                    Username = target_user.Username
                    Color = target_user.Color
                    Badges =
                        target_user.Badges
                        |> Seq.map (fun b ->
                            {
                                Players.Profile.View.Badge.Name = b
                                Players.Profile.View.Badge.Colors = Badge.badge_color b
                            }
                        )
                        |> Array.ofSeq
                    RecentScores = scores
                    DateSignedUp = target_user.DateSignedUp
                    IsFriend = relation = FriendRelation.Friend || relation = FriendRelation.MutualFriend
                    IsMutualFriend = relation = FriendRelation.MutualFriend
                }
                : Players.Profile.View.Response
            )
        }
