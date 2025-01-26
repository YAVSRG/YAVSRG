namespace Interlude.Web.Server.Domain.Core

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Gameplay
open Interlude.Web.Server

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

    let private SET: NonQuery<int64 * Stats> =
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

    let set (user_id: int64) (stats: Stats) =
        SET.Execute (user_id, stats) core_db |> expect |> ignore

    type XPLeaderboardModel = { UserId: int64; XP: int64; Playtime: float }

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

    let xp_leaderboard () =
        XP_LEADERBOARD.Execute () core_db |> expect

    type KeymodeLeaderboardModel =
        {
            UserId: int64
            Playtime: float
            Combined: float32
            Jacks: float32
            Chordstream: float32
            Stream: float32
        }

    let private LEADERBOARD_4K: Query<unit, KeymodeLeaderboardModel> =
        { Query.without_parameters() with
            SQL =
                """
            SELECT UserId, _4KPlaytime, _4KCombined, _4KJacks, _4KChordstream, _4KStream FROM stats
            ORDER BY _4KCombined DESC
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

    let leaderboard_4k () =
        LEADERBOARD_4K.Execute () core_db |> expect

    let private LEADERBOARD_7K: Query<unit, KeymodeLeaderboardModel> =
        { Query.without_parameters() with
            SQL =
                """
            SELECT UserId, _7KPlaytime, _7KCombined, _7KJacks, _7KChordstream, _7KStream FROM stats
            ORDER BY _7KCombined DESC
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

    let leaderboard_7k () =
        LEADERBOARD_7K.Execute () core_db |> expect