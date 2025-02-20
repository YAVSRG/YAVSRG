namespace Interlude.Web.Server.Domain.Core

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Calculator
open Interlude.Web.Server

type XPLeaderboardModel = { UserId: int64; XP: int64; Playtime: float }

type XPRankModel = { Rank: int64; XP: int64; Playtime: float }

type KeymodeLeaderboardModel =
    {
        UserId: int64
        Playtime: float
        Combined: float32
        Jacks: float32
        Chordstream: float32
        Stream: float32
    }

type KeymodeRankModel =
    {
        Rank: int64
        Playtime: float
        Combined: float32
        Jacks: float32
        Chordstream: float32
        Stream: float32
    }

type Stats =
    {
        LastSync: int64
        Playtime: float
        PracticeTime: float
        GameTime: float
        NotesHit: int
        XP: int64

        _4KPlaytime: float
        _4K: KeymodeTinyBreakdown
        _7KPlaytime: float
        _7K: KeymodeTinyBreakdown
        _10KPlaytime: float
        _10K: KeymodeTinyBreakdown
    }
    static member Default =
        {
            LastSync = 0L
            Playtime = 0.0
            PracticeTime = 0.0
            GameTime = 0.0
            NotesHit = 0
            XP = 0L
            _4KPlaytime = 0.0
            _4K = KeymodeTinyBreakdown.Default
            _7KPlaytime = 0.0
            _7K = KeymodeTinyBreakdown.Default
            _10KPlaytime = 0.0
            _10K = KeymodeTinyBreakdown.Default
        }

module Stats =

    let private DEFAULT = Stats.Default

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE stats (
                UserId INTEGER PRIMARY KEY NOT NULL,
                LastSync INTEGER NOT NULL,
                Playtime FLOAT NOT NULL,
                PracticeTime FLOAT NOT NULL,
                GameTime FLOAT NOT NULL,
                NotesHit INTEGER NOT NULL,
                XP FLOAT NOT NULL,

                _4KPlaytime FLOAT NOT NULL,
                _4KCombined FLOAT NOT NULL,
                _4KJacks FLOAT NOT NULL,
                _4KChordstream FLOAT NOT NULL,
                _4KStream FLOAT NOT NULL,

                _7KPlaytime FLOAT NOT NULL,
                _7KCombined FLOAT NOT NULL,
                _7KJacks FLOAT NOT NULL,
                _7KChordstream FLOAT NOT NULL,
                _7KStream FLOAT NOT NULL,

                _10KPlaytime FLOAT NOT NULL,
                _10KCombined FLOAT NOT NULL,
                _10KJacks FLOAT NOT NULL,
                _10KChordstream FLOAT NOT NULL,
                _10KStream FLOAT NOT NULL,
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE
            );
            """
        }

    let private GET: Query<int64, Stats> =
        {
            SQL =
                """
            SELECT
                LastSync, Playtime, PracticeTime, GameTime, NotesHit, XP,
                _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream,
                _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream,
                _10KPlaytime, _10KCombined, _10KJacks, _10KChordstream, _10KStream
            FROM stats
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = fun r ->
                {
                    LastSync = r.Int64
                    Playtime = r.Float64
                    PracticeTime = r.Float64
                    GameTime = r.Float64
                    NotesHit = r.Int32
                    XP = r.Int64
                    _4KPlaytime = r.Float64
                    _4K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                    _7KPlaytime = r.Float64
                    _7K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                    _10KPlaytime = r.Float64
                    _10K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                }
        }

    let get (user_id: int64) =
        GET.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let get_or_default (user_id: int64) =
        GET.Execute user_id core_db |> expect |> Array.tryExactlyOne |> Option.defaultValue DEFAULT

    let private SAVE: NonQuery<int64 * Stats> =
        {
            SQL =
                """
            INSERT OR REPLACE INTO stats (
                UserId, LastSync, Playtime, PracticeTime, GameTime, NotesHit, XP,
                _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream,
                _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream,
                _10KPlaytime, _10KCombined, _10KJacks, _10KChordstream, _10KStream
            )
            VALUES (
                @UserId, @LastSync, @Playtime, @PracticeTime, @GameTime, @NotesHit, @XP,
                @_4KPlaytime, @_4KCombined, @_4KJacks, @_4KChordstream, @_4KStream,
                @_7KPlaytime, @_7KCombined, @_7KJacks, @_7KChordstream, @_7KStream,
                @_10KPlaytime, @_10KCombined, @_10KJacks, @_10KChordstream, @_10KStream
            );
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@LastSync", SqliteType.Integer, 8
                    "@Playtime", SqliteType.Real, 8
                    "@PracticeTime", SqliteType.Real, 8
                    "@GameTime", SqliteType.Real, 8
                    "@NotesHit", SqliteType.Integer, 4
                    "@XP", SqliteType.Integer, 8

                    "@_4KPlaytime", SqliteType.Real, 8
                    "@_4KCombined", SqliteType.Real, 4
                    "@_4KJacks", SqliteType.Real, 4
                    "@_4KChordstream", SqliteType.Real, 4
                    "@_4KStream", SqliteType.Real, 4

                    "@_7KPlaytime", SqliteType.Real, 8
                    "@_7KCombined", SqliteType.Real, 4
                    "@_7KJacks", SqliteType.Real, 4
                    "@_7KChordstream", SqliteType.Real, 4
                    "@_7KStream", SqliteType.Real, 4

                    "@_10KPlaytime", SqliteType.Real, 8
                    "@_10KCombined", SqliteType.Real, 4
                    "@_10KJacks", SqliteType.Real, 4
                    "@_10KChordstream", SqliteType.Real, 4
                    "@_10KStream", SqliteType.Real, 4
                ]
            FillParameters =
                (fun p (user_id, stats) ->
                    p.Int64 user_id
                    p.Int64 stats.LastSync
                    p.Float64 stats.Playtime
                    p.Float64 stats.PracticeTime
                    p.Float64 stats.GameTime
                    p.Float64 stats.NotesHit
                    p.Int64 stats.XP

                    p.Float64 stats._4KPlaytime
                    p.Float32 stats._4K.Combined
                    p.Float32 stats._4K.Jacks
                    p.Float32 stats._4K.Chordstream
                    p.Float32 stats._4K.Stream

                    p.Float64 stats._7KPlaytime
                    p.Float32 stats._7K.Combined
                    p.Float32 stats._7K.Jacks
                    p.Float32 stats._7K.Chordstream
                    p.Float32 stats._7K.Stream

                    p.Float64 stats._10KPlaytime
                    p.Float32 stats._10K.Combined
                    p.Float32 stats._10K.Jacks
                    p.Float32 stats._10K.Chordstream
                    p.Float32 stats._10K.Stream
                )
        }

    let save (user_id: int64) (stats: Stats) =
        SAVE.Execute (user_id, stats) core_db |> expect |> ignore

    let private XP_LEADERBOARD: Query<unit, XPLeaderboardModel> =
        { Query.without_parameters() with
            SQL =
                """
            SELECT UserId, XP, Playtime FROM stats
            ORDER BY XP DESC
            LIMIT 50;
            """
            Read = (fun r -> { UserId = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let private XP_RANK: Query<int64, XPRankModel> =
        {
            SQL =
                """
            SELECT Rank, XP, Playtime FROM
            (
                SELECT RANK() OVER (ORDER BY XP DESC) AS Rank, XP, Playtime, UserId FROM stats
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = (fun r -> { Rank = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let private PLAYTIME_LEADERBOARD: Query<unit, XPLeaderboardModel> =
        { Query.without_parameters() with
            SQL =
                """
            SELECT UserId, XP, Playtime FROM stats
            ORDER BY Playtime DESC
            LIMIT 50;
            """
            Read = (fun r -> { UserId = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let private PLAYTIME_RANK: Query<int64, XPRankModel> =
        {
            SQL =
                """
            SELECT Rank, XP, Playtime FROM
            (
                SELECT RANK() OVER (ORDER BY Playtime DESC) AS Rank, XP, Playtime, UserId FROM stats
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = (fun r -> { Rank = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let xp_leaderboard () =
        XP_LEADERBOARD.Execute () core_db |> expect

    let xp_rank (user_id: int64) =
        XP_RANK.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let playtime_leaderboard () =
        PLAYTIME_LEADERBOARD.Execute () core_db |> expect

    let playtime_rank (user_id: int64) =
        PLAYTIME_RANK.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let private LEADERBOARD_4K (by_column: string) : Query<unit, KeymodeLeaderboardModel> =
        { Query.without_parameters() with
            SQL =
                $"""
            SELECT UserId, _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream FROM stats
            ORDER BY {by_column} DESC
            LIMIT 50;
            """
            Read = (fun r ->
                {
                    UserId = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private RANK_4K (by_column: string) : Query<int64, KeymodeRankModel> =
        {
            SQL =
                $"""
            SELECT Rank, _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream FROM
            (
                SELECT RANK() OVER (ORDER BY {by_column} DESC) AS Rank, _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream, UserId FROM stats
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = (fun r ->
                {
                    Rank = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private LEADERBOARD_4K_PLAYTIME = LEADERBOARD_4K "_4KPlaytime"
    let private LEADERBOARD_4K_COMBINED = LEADERBOARD_4K "_4KCombined"
    let private LEADERBOARD_4K_JACKS = LEADERBOARD_4K "_4KJacks"
    let private LEADERBOARD_4K_CHORDSTREAM = LEADERBOARD_4K "_4KChordstream"
    let private LEADERBOARD_4K_STREAM = LEADERBOARD_4K "_4KStream"

    let private RANK_4K_PLAYTIME = RANK_4K "_4KPlaytime"
    let private RANK_4K_COMBINED = RANK_4K "_4KCombined"
    let private RANK_4K_JACKS = RANK_4K "_4KJacks"
    let private RANK_4K_CHORDSTREAM = RANK_4K "_4KChordstream"
    let private RANK_4K_STREAM = RANK_4K "_4KStream"

    let leaderboard_4k_playtime () =
        LEADERBOARD_4K_PLAYTIME.Execute () core_db |> expect

    let leaderboard_4k_combined () =
        LEADERBOARD_4K_COMBINED.Execute () core_db |> expect

    let leaderboard_4k_jacks () =
        LEADERBOARD_4K_JACKS.Execute () core_db |> expect

    let leaderboard_4k_chordstream () =
        LEADERBOARD_4K_CHORDSTREAM.Execute () core_db |> expect

    let leaderboard_4k_stream () =
        LEADERBOARD_4K_STREAM.Execute () core_db |> expect

    let rank_4k_playtime (user_id: int64) =
        RANK_4K_PLAYTIME.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_4k_combined (user_id: int64) =
        RANK_4K_COMBINED.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_4k_jacks (user_id: int64) =
        RANK_4K_JACKS.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_4k_chordstream (user_id: int64) =
        RANK_4K_CHORDSTREAM.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_4k_stream (user_id: int64) =
        RANK_4K_STREAM.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let private LEADERBOARD_7K (by_column: string) : Query<unit, KeymodeLeaderboardModel> =
        { Query.without_parameters() with
            SQL =
                $"""
            SELECT UserId, _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream FROM stats
            ORDER BY {by_column} DESC
            LIMIT 50;
            """
            Read = (fun r ->
                {
                    UserId = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private RANK_7K (by_column: string) : Query<int64, KeymodeRankModel> =
        {
            SQL =
                $"""
            SELECT Rank, _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream FROM
            (
                SELECT RANK() OVER (ORDER BY {by_column} DESC) AS Rank, _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream, UserId FROM stats
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = (fun r ->
                {
                    Rank = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private LEADERBOARD_7K_PLAYTIME = LEADERBOARD_7K "_7KPlaytime"
    let private LEADERBOARD_7K_COMBINED = LEADERBOARD_7K "_7KCombined"
    let private LEADERBOARD_7K_JACKS = LEADERBOARD_7K "_7KJacks"
    let private LEADERBOARD_7K_CHORDSTREAM = LEADERBOARD_7K "_7KChordstream"
    let private LEADERBOARD_7K_STREAM = LEADERBOARD_7K "_7KStream"

    let private RANK_7K_PLAYTIME = RANK_7K "_7KPlaytime"
    let private RANK_7K_COMBINED = RANK_7K "_7KCombined"
    let private RANK_7K_JACKS = RANK_7K "_7KJacks"
    let private RANK_7K_CHORDSTREAM = RANK_7K "_7KChordstream"
    let private RANK_7K_STREAM = RANK_7K "_7KStream"

    let leaderboard_7k_playtime () =
        LEADERBOARD_7K_PLAYTIME.Execute () core_db |> expect

    let leaderboard_7k_combined () =
        LEADERBOARD_7K_COMBINED.Execute () core_db |> expect

    let leaderboard_7k_jacks () =
        LEADERBOARD_7K_JACKS.Execute () core_db |> expect

    let leaderboard_7k_chordstream () =
        LEADERBOARD_7K_CHORDSTREAM.Execute () core_db |> expect

    let leaderboard_7k_stream () =
        LEADERBOARD_7K_STREAM.Execute () core_db |> expect

    let rank_7k_playtime (user_id: int64) =
        RANK_7K_PLAYTIME.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_7k_combined (user_id: int64) =
        RANK_7K_COMBINED.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_7k_jacks (user_id: int64) =
        RANK_7K_JACKS.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_7k_chordstream (user_id: int64) =
        RANK_7K_CHORDSTREAM.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let rank_7k_stream (user_id: int64) =
        RANK_7K_STREAM.Execute user_id core_db |> expect |> Array.tryExactlyOne

    let private SUM_PLAYTIME : Query<unit, float option> =
        { Query.without_parameters() with
            SQL = """SELECT sum(Playtime) FROM stats;"""
            Read = fun r -> r.Float64Option
        }

    let sum_playtime () =
        SUM_PLAYTIME.Execute () core_db |> expect |> Array.tryExactlyOne |> Option.flatten |> Option.defaultValue 0.0

    let private SUM_GAMETIME : Query<unit, float option> =
        { Query.without_parameters() with
            SQL = """SELECT sum(GameTime) FROM stats;"""
            Read = fun r -> r.Float64Option
        }

    let sum_gametime () =
        SUM_GAMETIME.Execute () core_db |> expect |> Array.tryExactlyOne |> Option.flatten |> Option.defaultValue 0.0

type MonthlyStats =
    {
        LastSync: int64
        Playtime: float
        XP: int64

        _4KPlaytime: float
        _4K: KeymodeTinyBreakdown
        _7KPlaytime: float
        _7K: KeymodeTinyBreakdown
        _10KPlaytime: float
        _10K: KeymodeTinyBreakdown
    }
    static member Default : MonthlyStats =
        {
            LastSync = 0L
            Playtime = 0.0
            XP = 0L
            _4KPlaytime = 0.0
            _4K = KeymodeTinyBreakdown.Default
            _7KPlaytime = 0.0
            _7K = KeymodeTinyBreakdown.Default
            _10KPlaytime = 0.0
            _10K = KeymodeTinyBreakdown.Default
        }

module MonthlyStats =

    let DEFAULT = MonthlyStats.Default

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE monthly_stats (
                UserId INTEGER NOT NULL,
                Month INTEGER NOT NULL,
                LastSync INTEGER NOT NULL,
                Playtime FLOAT NOT NULL,
                XP FLOAT NOT NULL,

                _4KPlaytime FLOAT NOT NULL,
                _4KCombined FLOAT NOT NULL,
                _4KJacks FLOAT NOT NULL,
                _4KChordstream FLOAT NOT NULL,
                _4KStream FLOAT NOT NULL,

                _7KPlaytime FLOAT NOT NULL,
                _7KCombined FLOAT NOT NULL,
                _7KJacks FLOAT NOT NULL,
                _7KChordstream FLOAT NOT NULL,
                _7KStream FLOAT NOT NULL,

                _10KPlaytime FLOAT NOT NULL,
                _10KCombined FLOAT NOT NULL,
                _10KJacks FLOAT NOT NULL,
                _10KChordstream FLOAT NOT NULL,
                _10KStream FLOAT NOT NULL,
                PRIMARY KEY (UserId, Month),
                FOREIGN KEY (UserId) REFERENCES users(Id) ON DELETE CASCADE
            );
            """
        }

    let private GET: Query<int64 * int, MonthlyStats> =
        {
            SQL =
                """
            SELECT
                LastSync, Playtime, XP,
                _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream,
                _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream,
                _10KPlaytime, _10KCombined, _10KJacks, _10KChordstream, _10KStream
            FROM monthly_stats
            WHERE UserId = @UserId AND Month = @Month;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8; "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p (user_id, month) -> p.Int64 user_id; p.Int32 month
            Read = fun r ->
                {
                    LastSync = r.Int64
                    Playtime = r.Float64
                    XP = r.Int64
                    _4KPlaytime = r.Float64
                    _4K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                    _7KPlaytime = r.Float64
                    _7K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                    _10KPlaytime = r.Float64
                    _10K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                }
        }

    let get (month: int) (user_id: int64) : MonthlyStats option =
        GET.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let get_or_default (month: int) (user_id: int64) : MonthlyStats =
        GET.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne |> Option.defaultValue DEFAULT

    let private SAVE: NonQuery<int64 * int * MonthlyStats> =
        {
            SQL =
                """
            INSERT OR REPLACE INTO monthly_stats (
                UserId, Month, LastSync, Playtime, XP,
                _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream,
                _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream,
                _10KPlaytime, _10KCombined, _10KJacks, _10KChordstream, _10KStream
            )
            VALUES (
                @UserId, @Month, @LastSync, @Playtime, @XP,
                @_4KPlaytime, @_4KCombined, @_4KJacks, @_4KChordstream, @_4KStream,
                @_7KPlaytime, @_7KCombined, @_7KJacks, @_7KChordstream, @_7KStream,
                @_10KPlaytime, @_10KCombined, @_10KJacks, @_10KChordstream, @_10KStream
            );
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@Month", SqliteType.Integer, 4
                    "@LastSync", SqliteType.Integer, 8
                    "@Playtime", SqliteType.Real, 8
                    "@XP", SqliteType.Integer, 8

                    "@_4KPlaytime", SqliteType.Real, 8
                    "@_4KCombined", SqliteType.Real, 4
                    "@_4KJacks", SqliteType.Real, 4
                    "@_4KChordstream", SqliteType.Real, 4
                    "@_4KStream", SqliteType.Real, 4

                    "@_7KPlaytime", SqliteType.Real, 8
                    "@_7KCombined", SqliteType.Real, 4
                    "@_7KJacks", SqliteType.Real, 4
                    "@_7KChordstream", SqliteType.Real, 4
                    "@_7KStream", SqliteType.Real, 4

                    "@_10KPlaytime", SqliteType.Real, 8
                    "@_10KCombined", SqliteType.Real, 4
                    "@_10KJacks", SqliteType.Real, 4
                    "@_10KChordstream", SqliteType.Real, 4
                    "@_10KStream", SqliteType.Real, 4
                ]
            FillParameters =
                (fun p (user_id, month, stats) ->
                    p.Int64 user_id
                    p.Int32 month
                    p.Int64 stats.LastSync
                    p.Float64 stats.Playtime
                    p.Int64 stats.XP

                    p.Float64 stats._4KPlaytime
                    p.Float32 stats._4K.Combined
                    p.Float32 stats._4K.Jacks
                    p.Float32 stats._4K.Chordstream
                    p.Float32 stats._4K.Stream

                    p.Float64 stats._7KPlaytime
                    p.Float32 stats._7K.Combined
                    p.Float32 stats._7K.Jacks
                    p.Float32 stats._7K.Chordstream
                    p.Float32 stats._7K.Stream

                    p.Float64 stats._10KPlaytime
                    p.Float32 stats._10K.Combined
                    p.Float32 stats._10K.Jacks
                    p.Float32 stats._10K.Chordstream
                    p.Float32 stats._10K.Stream
                )
        }

    let save (month: int) (user_id: int64) (stats: MonthlyStats) =
        SAVE.Execute (user_id, month, stats) core_db |> expect |> ignore

    let private XP_LEADERBOARD: Query<int, XPLeaderboardModel> =
        {
            SQL =
                """
            SELECT UserId, XP, Playtime FROM monthly_stats
            WHERE Month = @Month
            ORDER BY XP DESC
            LIMIT 50;
            """
            Parameters = [ "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p month -> p.Int32 month
            Read = (fun r -> { UserId = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let private XP_RANK: Query<int64 * int, XPRankModel> =
        {
            SQL =
                """
            SELECT Rank, XP, Playtime FROM
            (
                SELECT RANK() OVER (ORDER BY XP DESC) AS Rank, XP, Playtime, UserId FROM monthly_stats
                WHERE Month = @Month
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8; "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p (user_id, month) -> p.Int64 user_id; p.Int32 month
            Read = (fun r -> { Rank = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let private PLAYTIME_LEADERBOARD: Query<int, XPLeaderboardModel> =
        {
            SQL =
                """
            SELECT UserId, XP, Playtime FROM monthly_stats
            WHERE Month = @Month
            ORDER BY Playtime DESC
            LIMIT 50;
            """
            Parameters = [ "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p month -> p.Int32 month
            Read = (fun r -> { UserId = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let private PLAYTIME_RANK: Query<int64 * int, XPRankModel> =
        {
            SQL =
                """
            SELECT Rank, XP, Playtime FROM
            (
                SELECT RANK() OVER (ORDER BY Playtime DESC) AS Rank, XP, Playtime, UserId FROM monthly_stats
                WHERE Month = @Month
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8; "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p (user_id, month) -> p.Int64 user_id; p.Int32 month
            Read = (fun r -> { Rank = r.Int64; XP = r.Int64; Playtime = r.Float64 })
        }

    let xp_leaderboard (month: int) =
        XP_LEADERBOARD.Execute month core_db |> expect

    let xp_rank (user_id: int64) (month: int) =
        XP_RANK.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let playtime_leaderboard (month: int) =
        PLAYTIME_LEADERBOARD.Execute month core_db |> expect

    let playtime_rank (user_id: int64) (month: int) =
        PLAYTIME_RANK.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let private LEADERBOARD_4K (by_column: string) : Query<int, KeymodeLeaderboardModel> =
        {
            SQL =
                $"""
            SELECT UserId, _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream FROM monthly_stats
            WHERE Month = @Month AND _4KPlaytime > 0
            ORDER BY {by_column} DESC
            LIMIT 50;
            """
            Parameters = [ "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p month -> p.Int32 month
            Read = (fun r ->
                {
                    UserId = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private RANK_4K (by_column: string) : Query<int64 * int, KeymodeRankModel> =
        {
            SQL =
                $"""
            SELECT Rank, _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream FROM
            (
                SELECT RANK() OVER (ORDER BY {by_column} DESC) AS Rank, _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream, UserId FROM monthly_stats
                WHERE Month = @Month
                AND _4KPlaytime > 0
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8; "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p (user_id, month) -> p.Int64 user_id; p.Int32 month
            Read = (fun r ->
                {
                    Rank = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private LEADERBOARD_4K_PLAYTIME = LEADERBOARD_4K "_4KPlaytime"
    let private LEADERBOARD_4K_COMBINED = LEADERBOARD_4K "_4KCombined"
    let private LEADERBOARD_4K_JACKS = LEADERBOARD_4K "_4KJacks"
    let private LEADERBOARD_4K_CHORDSTREAM = LEADERBOARD_4K "_4KChordstream"
    let private LEADERBOARD_4K_STREAM = LEADERBOARD_4K "_4KStream"

    let private RANK_4K_PLAYTIME = RANK_4K "_4KPlaytime"
    let private RANK_4K_COMBINED = RANK_4K "_4KCombined"
    let private RANK_4K_JACKS = RANK_4K "_4KJacks"
    let private RANK_4K_CHORDSTREAM = RANK_4K "_4KChordstream"
    let private RANK_4K_STREAM = RANK_4K "_4KStream"

    let leaderboard_4k_playtime (month: int) =
        LEADERBOARD_4K_PLAYTIME.Execute month core_db |> expect

    let leaderboard_4k_combined (month: int) =
        LEADERBOARD_4K_COMBINED.Execute month core_db |> expect

    let leaderboard_4k_jacks (month: int) =
        LEADERBOARD_4K_JACKS.Execute month core_db |> expect

    let leaderboard_4k_chordstream (month: int) =
        LEADERBOARD_4K_CHORDSTREAM.Execute month core_db |> expect

    let leaderboard_4k_stream (month: int) =
        LEADERBOARD_4K_STREAM.Execute month core_db |> expect

    let rank_4k_playtime (user_id: int64) (month: int) =
        RANK_4K_PLAYTIME.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_4k_combined (user_id: int64) (month: int) =
        RANK_4K_COMBINED.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_4k_jacks (user_id: int64) (month: int) =
        RANK_4K_JACKS.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_4k_chordstream (user_id: int64) (month: int) =
        RANK_4K_CHORDSTREAM.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_4k_stream (user_id: int64) (month: int) =
        RANK_4K_STREAM.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let private LEADERBOARD_7K (by_column: string) : Query<int, KeymodeLeaderboardModel> =
        {
            SQL =
                $"""
            SELECT UserId, _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream FROM monthly_stats
            WHERE Month = @Month AND _7KPlaytime > 0
            ORDER BY {by_column} DESC
            LIMIT 50;
            """
            Parameters = [ "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p month -> p.Int32 month
            Read = (fun r ->
                {
                    UserId = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private RANK_7K (by_column: string) : Query<int64 * int, KeymodeRankModel> =
        {
            SQL =
                $"""
            SELECT Rank, _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream FROM
            (
                SELECT RANK() OVER (ORDER BY {by_column} DESC) AS Rank, _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream, UserId FROM monthly_stats
                WHERE Month = @Month
                AND _7KPlaytime > 0
            )
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8; "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p (user_id, month) -> p.Int64 user_id; p.Int32 month
            Read = (fun r ->
                {
                    Rank = r.Int64
                    Playtime = r.Float64
                    Combined = r.Float32
                    Jacks = r.Float32
                    Chordstream = r.Float32
                    Stream = r.Float32
                }
            )
        }

    let private LEADERBOARD_7K_PLAYTIME = LEADERBOARD_7K "_7KPlaytime"
    let private LEADERBOARD_7K_COMBINED = LEADERBOARD_7K "_7KCombined"
    let private LEADERBOARD_7K_JACKS = LEADERBOARD_7K "_7KJacks"
    let private LEADERBOARD_7K_CHORDSTREAM = LEADERBOARD_7K "_7KChordstream"
    let private LEADERBOARD_7K_STREAM = LEADERBOARD_7K "_7KStream"

    let private RANK_7K_PLAYTIME = RANK_7K "_7KPlaytime"
    let private RANK_7K_COMBINED = RANK_7K "_7KCombined"
    let private RANK_7K_JACKS = RANK_7K "_7KJacks"
    let private RANK_7K_CHORDSTREAM = RANK_7K "_7KChordstream"
    let private RANK_7K_STREAM = RANK_7K "_7KStream"

    let leaderboard_7k_playtime (month: int) =
        LEADERBOARD_7K_PLAYTIME.Execute month core_db |> expect

    let leaderboard_7k_combined (month: int) =
        LEADERBOARD_7K_COMBINED.Execute month core_db |> expect

    let leaderboard_7k_jacks (month: int) =
        LEADERBOARD_7K_JACKS.Execute month core_db |> expect

    let leaderboard_7k_chordstream (month: int) =
        LEADERBOARD_7K_CHORDSTREAM.Execute month core_db |> expect

    let leaderboard_7k_stream (month: int) =
        LEADERBOARD_7K_STREAM.Execute month core_db |> expect

    let rank_7k_playtime (user_id: int64) (month: int) =
        RANK_7K_PLAYTIME.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_7k_combined (user_id: int64) (month: int) =
        RANK_7K_COMBINED.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_7k_jacks (user_id: int64) (month: int) =
        RANK_7K_JACKS.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_7k_chordstream (user_id: int64) (month: int) =
        RANK_7K_CHORDSTREAM.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let rank_7k_stream (user_id: int64) (month: int) =
        RANK_7K_STREAM.Execute (user_id, month) core_db |> expect |> Array.tryExactlyOne

    let private SUM_PLAYTIME : Query<int, float option> =
        {
            SQL = """SELECT sum(Playtime) FROM monthly_stats WHERE Month = @Month;"""
            Parameters = [ "@Month", SqliteType.Integer, 4 ]
            FillParameters = fun p month -> p.Int32 month
            Read = fun r -> r.Float64Option
        }

    let sum_playtime (month: int) =
        SUM_PLAYTIME.Execute month core_db |> expect |> Array.tryExactlyOne |> Option.flatten |> Option.defaultValue 0.0