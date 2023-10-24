namespace Interlude.Web.Server.API.Players

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Profile =

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            let userId, user = authorize headers

            let userId, user =
                if query_params.ContainsKey("user") then
                    match User.by_username query_params.["user"].[0] with
                    | Some (userId, user) -> userId, user
                    | None -> raise NotFoundException
                else userId, user

            let recent_scores = Score.get_recent userId

            let scores : Players.Profile.RecentScore array =
                recent_scores 
                |> Array.map (fun s ->
                    let rs = Charts.rulesets.[s.RulesetId]
                    match Charts.by_hash s.ChartId with
                    | Some (chart, song) ->
                        {
                            Artist = song.FormattedArtists
                            Title = song.Title
                            Difficulty = chart.DifficultyName
                            Score = s.Score
                            Lamp = rs.LampName s.Lamp
                            Mods = if s.Mods.IsEmpty then sprintf "%.2fx" s.Rate else sprintf "%.2fx*" s.Rate
                            Timestamp = s.Timestamp
                        }
                    | None -> 
                        {
                            Artist = "???"
                            Title = "???"
                            Difficulty = "???"
                            Score = s.Score
                            Lamp = rs.LampName s.Lamp
                            Mods = if s.Mods.IsEmpty then sprintf "%.2fx" s.Rate else sprintf "%.2fx*" s.Rate
                            Timestamp = s.Timestamp
                        }
                )
                    
            response.ReplyJson(
                {
                    Username = user.Username
                    Color = user.Color |> Option.defaultValue Badge.DEFAULT_COLOR
                    Badges = user.Badges |> Seq.map (fun b -> { Players.Profile.Badge.Name = b; Players.Profile.Badge.Colors = Badge.badge_color b }) |> Array.ofSeq
                    RecentScores = scores
                    DateSignedUp = user.DateSignedUp
                } : Players.Profile.Response)
        }