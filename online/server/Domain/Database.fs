namespace Interlude.Web.Server.Domain

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay
open Interlude.Web.Server

open Interlude.Web.Server.Domain

module Migrations =

    let migrate_users_friends(db: Database) =

        DatabaseRef.db <- db

        let users = try User._dump() with _ -> [||]

        let migrate_user : NonQuery<int64 * User> =
            {
                SQL = """
                    INSERT INTO users (Id, Username, DiscordId, DateSignedUp, LastLogin, AuthToken, Badges, Color) 
                    VALUES (@Id, @Username, @DiscordId, @DateSignedUp, @LastLogin, @AuthToken, @Badges, @Color);"""
                Parameters =
                    [
                        "@Id", SqliteType.Integer, 8
                        "@Username", SqliteType.Text, -1
                        "@DiscordId", SqliteType.Text, -1
                        "@DateSignedUp", SqliteType.Integer, 8
                        "@LastLogin", SqliteType.Integer, 8
                        "@AuthToken", SqliteType.Text, -1
                        "@Badges", SqliteType.Text, -1
                        "@Color", SqliteType.Integer, 8
                    ]
                FillParameters = (fun p (id, user) ->
                    p.Int64 id
                    p.String user.Username
                    p.String (string user.DiscordId)
                    p.Int64 user.DateSignedUp
                    p.Int64 user.LastLogin
                    p.String user.AuthToken
                    p.Json JSON user.Badges
                    p.Int32 (user.Color |> Option.defaultValue Objects.Badge.DEFAULT_COLOR)
                )
            }

        migrate_user.Batch users db
        |> expect
        |> sprintf "Migrated %i users successfully"
        |> Logging.Info
        
        let users_that_exist = Set.ofSeq (users |> Seq.map fst)

        for id, _ in users do
            for friend_id in Friends.get_id_list id do
                if users_that_exist.Contains friend_id then
                    Objects.Friends.add (id, friend_id)

        Logging.Info("Migrated friends successfully")

        let scores = try Score._dump() with _ -> [||]

        let now = DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

        let migrate_score : NonQuery<Score> =
            {
                SQL = """
                INSERT INTO scores (UserId, ChartId, RulesetId, TimePlayed, TimeUploaded, Rate, Mods, Ranked, Accuracy, Grade, Lamp, ReplayId)
                VALUES (@UserId, @ChartId, @RulesetId, @TimePlayed, @TimeUploaded, @Rate, @Mods, @Ranked, @Accuracy, @Grade, @Lamp, @ReplayId);
                """
                Parameters =
                    [
                        "@UserId", SqliteType.Integer, 8
                        "@ChartId", SqliteType.Text, -1
                        "@RulesetId", SqliteType.Text, -1
                        "@TimePlayed", SqliteType.Integer, 8
                        "@TimeUploaded", SqliteType.Integer, 8
                        "@Rate", SqliteType.Real, 4
                        "@Mods", SqliteType.Text, -1
                        "@Ranked", SqliteType.Integer, 1
                        "@Accuracy", SqliteType.Real, 8
                        "@Grade", SqliteType.Integer, 4
                        "@Lamp", SqliteType.Integer, 4
                        "@ReplayId", SqliteType.Integer, 8
                    ]
                FillParameters = (fun p score ->
                    p.Int64 score.UserId
                    p.String score.ChartId
                    p.String score.RulesetId
                    p.Int64 score.Timestamp
                    p.Int64 now
                    p.Float32 score.Rate
                    p.Json JSON score.Mods
                    p.Boolean (score.Rate >= 1.0f && Mods.check score.Mods = Ok Mods.ModStatus.Ranked)
                    p.Float64 score.Score
                    p.Int32 score.Grade
                    p.Int32 score.Lamp
                    p.Int64Option None
                )
            }

        migrate_score.Batch (scores |> Seq.where(fun score -> users_that_exist.Contains score.UserId)) db
        |> expect
        |> sprintf "Migrated %i scores successfully"
        |> Logging.Info

    open Interlude.Web.Server.Domain.Objects

    let run (db: Database) =
        Database.migrate 
            "InitialTables" 
            (fun db ->
                Database.create_table User.TABLE db |> expect |> ignore
                Friends.CREATE_TABLE.Execute () db |> expect |> ignore
                Replay.CREATE_TABLE.Execute () db |> expect |> ignore
                Score.CREATE_TABLE.Execute () db |> expect |> ignore
                Leaderboard.CREATE_TABLE.Execute () db |> expect |> ignore
                Logging.Info("Migration created initial tables")
            )
            db
        Database.migrate
            "MigrateUsersAndFriendsFromRedis"
            migrate_users_friends
            db

module Database =

    let startup() =
        if IO.File.Exists("./data/core.db") then IO.File.Delete("./data/core.db") // for debug purposes
        let _db = Database.from_file("./data/core.db")
        Migrations.run _db
        db <- db

    let startup_unit_tests() : IDisposable =
        let _db, keep_alive = Database.in_memory("unit_tests")
        Migrations.run _db
        db <- _db
        keep_alive