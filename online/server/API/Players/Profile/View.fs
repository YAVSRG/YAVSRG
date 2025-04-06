namespace Interlude.Web.Server.API.Players.Profile

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services

module View =

    open Players.Profile.View

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

            let scores: RecentScore array =
                recent_scores
                |> Array.map (fun s ->
                    match Backbeat.Charts.by_hash s.ChartId with
                    | Some(chart, song) ->
                        {
                            Artist = song.FormattedArtists
                            Title = song.Title
                            Difficulty = chart.DifficultyName
                            Score = s.Accuracy
                            Lamp = s.Lamp
                            Rate = s.Rate
                            Mods = s.Mods
                            Timestamp = s.TimePlayed
                        }
                    | None ->
                        {
                            Artist = "???"
                            Title = "???"
                            Difficulty = "???"
                            Score = s.Accuracy
                            Lamp = s.Lamp
                            Rate = s.Rate
                            Mods = s.Mods
                            Timestamp = s.TimePlayed
                        }
                )

            let relation = Friends.relation (user_id, target_user_id)

            let stats = Stats.get_or_default target_user_id

            response.ReplyJson(
                {
                    Username = target_user.Username
                    Color = target_user.Color
                    Badges =
                        target_user.Badges
                        |> Seq.map (fun b ->
                            {
                                Badge.Name = b
                                Badge.Colors = Badge.badge_color b
                            }
                        )
                        |> Array.ofSeq
                    RecentScores = scores
                    DateSignedUp = target_user.DateSignedUp
                    IsFriend = relation = FriendRelation.Friend || relation = FriendRelation.MutualFriend
                    IsMutualFriend = relation = FriendRelation.MutualFriend
                    Stats =
                        {
                            LastUpdated = stats.LastSync
                            XP = stats.XP
                            TotalPlaytime = stats.Playtime
                            Keymodes = Map.ofSeq [ 4, (stats._4KPlaytime, stats._4K.Average); 7, (stats._7KPlaytime, stats._7K.Average); 10, (stats._10KPlaytime, stats._10K.Average) ]
                        }
                }
                : Response
            )
        }