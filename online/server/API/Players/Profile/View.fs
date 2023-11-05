namespace Interlude.Web.Server.API.Players.Profile

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module View =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let userId, user = authorize headers

            let target_userId, target_user =
                if query_params.ContainsKey("user") then
                    match User.by_username query_params.["user"].[0] with
                    | Some(userId, user) -> userId, user
                    | None -> raise NotFoundException
                else
                    userId, user

            let recent_scores = Score.get_recent target_userId

            let scores: Players.Profile.View.RecentScore array =
                recent_scores
                |> Array.map (fun s ->
                    let rs = Backbeat.rulesets.[s.RulesetId]

                    match Backbeat.Charts.by_hash s.ChartId with
                    | Some(chart, song) ->
                        {
                            Artist = song.FormattedArtists
                            Title = song.Title
                            Difficulty = chart.DifficultyName
                            Score = s.Score
                            Lamp = rs.LampName s.Lamp
                            Mods =
                                if s.Mods.IsEmpty then
                                    sprintf "%.2fx" s.Rate
                                else
                                    sprintf "%.2fx*" s.Rate
                            Timestamp = s.Timestamp
                        }
                    | None ->
                        {
                            Artist = "???"
                            Title = "???"
                            Difficulty = "???"
                            Score = s.Score
                            Lamp = rs.LampName s.Lamp
                            Mods =
                                if s.Mods.IsEmpty then
                                    sprintf "%.2fx" s.Rate
                                else
                                    sprintf "%.2fx*" s.Rate
                            Timestamp = s.Timestamp
                        }
                )

            let is_friend =
                target_user.Username <> user.Username
                && Friends.has_friend (userId, target_userId)

            let is_mutual_friend = is_friend && Friends.has_friend (target_userId, userId)

            response.ReplyJson(
                {
                    Username = target_user.Username
                    Color = target_user.Color |> Option.defaultValue Badge.DEFAULT_COLOR
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
                    IsFriend = is_friend
                    IsMutualFriend = is_mutual_friend
                }
                : Players.Profile.View.Response
            )
        }
