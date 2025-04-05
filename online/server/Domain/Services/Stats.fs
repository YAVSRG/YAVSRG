namespace Interlude.Web.Server.Domain.Services

open Percyqaz.Common
open Prelude.Data.User.Stats
open Interlude.Web.Server.Domain.Core

module Stats =

    let sync (user_id: int64) (incoming: StatsSyncUpstream): Result<unit, string> =

        let now = Timestamp.now()
        let month = timestamp_to_leaderboard_month now
        let all_time_stats = Stats.get_or_default user_id
        let monthly_stats = MonthlyStats.get_or_default month user_id

        if incoming.Month <> month then
            Error "Stats month does not match server month"
        else

        if incoming.NetworkId <> user_id then
            Error <| sprintf "Stats data is for user id #%i" incoming.NetworkId
        else

        let time_since_sync = float (now - all_time_stats.LastSync)
        if incoming.GameTime - all_time_stats.GameTime > time_since_sync + 120000.0 then
            Error
                (
                    sprintf "Increase in gametime is notably greater than real life time elapsed since last sync: +%s vs +%s"
                        (format_long_time(incoming.GameTime - all_time_stats.GameTime))
                        (format_long_time(time_since_sync))
                )
        //elif incoming.Playtime - all_time_stats.Playtime > incoming.GameTime - all_time_stats.GameTime + 120000.0 then
        //    Error
        //        (
        //            sprintf "Increase in playtime is notably greater than increase in gametime: +%s vs +%s"
        //                (format_long_time(incoming.Playtime - all_time_stats.Playtime))
        //                (format_long_time(incoming.GameTime - all_time_stats.GameTime))
        //        )
        else

        let new_all_time_stats: Stats =
            {
                LastSync = now
                Playtime = safe_stat_max all_time_stats.Playtime incoming.Playtime
                PracticeTime = safe_stat_max all_time_stats.PracticeTime incoming.PracticeTime
                GameTime = safe_stat_max all_time_stats.GameTime incoming.GameTime
                NotesHit = max all_time_stats.NotesHit incoming.NotesHit
                XP = max all_time_stats.XP incoming.XP
                _4KPlaytime = safe_stat_max all_time_stats._4KPlaytime (incoming.KeymodePlaytime.TryFind 4 |> Option.defaultValue 0.0)
                _4K =
                    {
                        Combined = max all_time_stats._4K.Combined incoming.KeymodeSkillsAllTime.[1].Combined
                        Jacks = max all_time_stats._4K.Jacks incoming.KeymodeSkillsAllTime.[1].Jacks
                        Chordstream = max all_time_stats._4K.Chordstream incoming.KeymodeSkillsAllTime.[1].Chordstream
                        Stream = max all_time_stats._4K.Stream incoming.KeymodeSkillsAllTime.[1].Stream
                    }
                _7KPlaytime = safe_stat_max all_time_stats._7KPlaytime (incoming.KeymodePlaytime.TryFind 7 |> Option.defaultValue 0.0)
                _7K =
                    {
                        Combined = max all_time_stats._7K.Combined incoming.KeymodeSkillsAllTime.[4].Combined
                        Jacks = max all_time_stats._7K.Jacks incoming.KeymodeSkillsAllTime.[4].Jacks
                        Chordstream = max all_time_stats._7K.Chordstream incoming.KeymodeSkillsAllTime.[4].Chordstream
                        Stream = max all_time_stats._7K.Stream incoming.KeymodeSkillsAllTime.[4].Stream
                    }
                _10KPlaytime = safe_stat_max all_time_stats._10KPlaytime (incoming.KeymodePlaytime.TryFind 10 |> Option.defaultValue 0.0)
                _10K =
                    {
                        Combined = max all_time_stats._10K.Combined incoming.KeymodeSkillsAllTime.[7].Combined
                        Jacks = max all_time_stats._10K.Jacks incoming.KeymodeSkillsAllTime.[7].Jacks
                        Chordstream = max all_time_stats._10K.Chordstream incoming.KeymodeSkillsAllTime.[7].Chordstream
                        Stream = max all_time_stats._10K.Stream incoming.KeymodeSkillsAllTime.[7].Stream
                    }
            }
        Stats.save user_id new_all_time_stats

        let new_monthly_stats: MonthlyStats =
            {
                LastSync = now
                Playtime = safe_stat_max monthly_stats.Playtime incoming.PlaytimeThisMonth
                XP = max monthly_stats.XP incoming.XPThisMonth
                _4KPlaytime = safe_stat_max monthly_stats._4KPlaytime (incoming.KeymodePlaytimeThisMonth.TryFind 4 |> Option.defaultValue 0.0)
                _4K =
                    {
                        Combined = max monthly_stats._4K.Combined incoming.KeymodeSkillsRecent.[1].Combined
                        Jacks = max monthly_stats._4K.Jacks incoming.KeymodeSkillsRecent.[1].Jacks
                        Chordstream = max monthly_stats._4K.Chordstream incoming.KeymodeSkillsRecent.[1].Chordstream
                        Stream = max monthly_stats._4K.Stream incoming.KeymodeSkillsRecent.[1].Stream
                    }
                _7KPlaytime = safe_stat_max monthly_stats._7KPlaytime (incoming.KeymodePlaytimeThisMonth.TryFind 7 |> Option.defaultValue 0.0)
                _7K =
                    {
                        Combined = max monthly_stats._7K.Combined incoming.KeymodeSkillsRecent.[4].Combined
                        Jacks = max monthly_stats._7K.Jacks incoming.KeymodeSkillsRecent.[4].Jacks
                        Chordstream = max monthly_stats._7K.Chordstream incoming.KeymodeSkillsRecent.[4].Chordstream
                        Stream = max monthly_stats._7K.Stream incoming.KeymodeSkillsRecent.[4].Stream
                    }
                _10KPlaytime = safe_stat_max monthly_stats._10KPlaytime (incoming.KeymodePlaytimeThisMonth.TryFind 10 |> Option.defaultValue 0.0)
                _10K =
                    {
                        Combined = max monthly_stats._10K.Combined incoming.KeymodeSkillsRecent.[7].Combined
                        Jacks = max monthly_stats._10K.Jacks incoming.KeymodeSkillsRecent.[7].Jacks
                        Chordstream = max monthly_stats._10K.Chordstream incoming.KeymodeSkillsRecent.[7].Chordstream
                        Stream = max monthly_stats._10K.Stream incoming.KeymodeSkillsRecent.[7].Stream
                    }
            }
        MonthlyStats.save month user_id new_monthly_stats

        Ok()