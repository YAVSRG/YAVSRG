namespace Interlude.Web.Server.Domain.Core

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Gameplay
open Interlude.Web.Server

type Stats =
    {
        LastSync: int64
        XP: int64
        Playtime: float
        _4K: KeymodeTinyBreakdown
        _7K: KeymodeTinyBreakdown
        _10K: KeymodeTinyBreakdown
    }
    static member Default =
        {
            LastSync = 0L
            XP = 0L
            Playtime = 0.0
            _4K = KeymodeTinyBreakdown.Default
            _7K = KeymodeTinyBreakdown.Default
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
                XP FLOAT NOT NULL,
                Playtime FLOAT NOT NULL,

                _4KCombined FLOAT NOT NULL,
                _4KJacks FLOAT NOT NULL,
                _4KChordstream FLOAT NOT NULL,
                _4KStream FLOAT NOT NULL,

                _7KCombined FLOAT NOT NULL,
                _7KJacks FLOAT NOT NULL,
                _7KChordstream FLOAT NOT NULL,
                _7KStream FLOAT NOT NULL,

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
                LastSync, XP, Playtime,
                _4KCombined, _4KJacks, _4KChordstream, _4KStream,
                _7KCombined, _7KJacks, _7KChordstream, _7KStream,
                _10KCombined, _10KJacks, _10KChordstream, _10KStream
            FROM stats
            WHERE UserId = @UserId;
            """
            Parameters = [ "@UserId", SqliteType.Integer, 8 ]
            FillParameters = fun p user_id -> p.Int64 user_id
            Read = fun r ->
                {
                    LastSync = r.Int64
                    XP = r.Int64
                    Playtime = r.Float64
                    _4K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                    _7K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                    _10K = { Combined = r.Float32; Jacks = r.Float32; Chordstream = r.Float32; Stream = r.Float32 }
                }
        }

    let get (user_id: int64) =
        GET.Execute (user_id) core_db |> expect |> Array.tryExactlyOne

    let get_or_default (user_id: int) =
        GET.Execute user_id core_db |> expect |> Array.tryExactlyOne |> Option.defaultValue DEFAULT

    let private SET: NonQuery<int64 * Stats> =
        {
            SQL =
                """
            INSERT OR REPLACE INTO stats (
                UserId, LastSync, XP, Playtime,
                _4KCombined, _4KJacks, _4KChordstream, _4KStream,
                _7KCombined, _7KJacks, _7KChordstream, _7KStream,
                _10KCombined, _10KJacks, _10KChordstream, _10KStream
            )
            VALUES (
                @UserId, @LastSync, @XP, @Playtime,
                @_4KCombined, @_4KJacks, @_4KChordstream, @_4KStream,
                @_7KCombined, @_7KJacks, @_7KChordstream, @_7KStream,
                @_10KCombined, @_10KJacks, @_10KChordstream, @_10KStream,
            );
            """
            Parameters =
                [
                    "@UserId", SqliteType.Integer, 8
                    "@LastSync", SqliteType.Integer, 8
                    "@XP", SqliteType.Integer, 8
                    "@Playtime", SqliteType.Real, 8

                    "@_4KCombined", SqliteType.Real, 4
                    "@_4KJacks", SqliteType.Real, 4
                    "@_4KChordstream", SqliteType.Real, 4
                    "@_4KStream", SqliteType.Real, 4

                    "@_7KCombined", SqliteType.Real, 4
                    "@_7KJacks", SqliteType.Real, 4
                    "@_7KChordstream", SqliteType.Real, 4
                    "@_7KStream", SqliteType.Real, 4

                    "@_10KCombined", SqliteType.Real, 4
                    "@_10KJacks", SqliteType.Real, 4
                    "@_10KChordstream", SqliteType.Real, 4
                    "@_10KStream", SqliteType.Real, 4
                ]
            FillParameters =
                (fun p (user_id, stats) ->
                    p.Int64 user_id
                    p.Int64 stats.LastSync
                    p.Int64 stats.XP
                    p.Float64 stats.Playtime

                    p.Float32 stats._4K.Combined
                    p.Float32 stats._4K.Jacks
                    p.Float32 stats._4K.Chordstream
                    p.Float32 stats._4K.Stream

                    p.Float32 stats._7K.Combined
                    p.Float32 stats._7K.Jacks
                    p.Float32 stats._7K.Chordstream
                    p.Float32 stats._7K.Stream

                    p.Float32 stats._10K.Combined
                    p.Float32 stats._10K.Jacks
                    p.Float32 stats._10K.Chordstream
                    p.Float32 stats._10K.Stream
                )
        }

    let set (user_id: int64) (stats: Stats) =
        SET.Execute (user_id, stats) core_db |> expect |> ignore

    type TableLeaderboardModel = { UserId: int64; Rating: float }

    let private LEADERBOARD: Query<string, TableLeaderboardModel> =
        {
            SQL =
                """
            SELECT UserId, Rating FROM table_ratings
            WHERE TableId = @TableId
            ORDER BY Rating DESC
            LIMIT 50;
            """
            Parameters = [ "@TableId", SqliteType.Text, -1 ]
            FillParameters = fun p table_id -> p.String table_id
            Read = (fun r -> { UserId = r.Int64; Rating = r.Float64 })
        }

    let leaderboard (table_id: string) =
        LEADERBOARD.Execute table_id core_db |> expect