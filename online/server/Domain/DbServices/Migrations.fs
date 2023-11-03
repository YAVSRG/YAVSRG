namespace Interlude.Web.Server.Domain

open NRedisStack.Search
open NRedisStack.Search.Literals.Enums
open Percyqaz.Common
open Interlude.Web.Server.Domain.Redis

module Migrations =

    let run () =
        Logging.Debug("Performing any necessary migrations...")

        let migration =
            match db.StringGet("migration") with
            | v when v.IsNull -> 0L
            | v ->
                let _, (value: int64) = v.TryParse()
                value

        if migration < 1L then
            Logging.Debug("Performing migration 1")

            ft.Create(
                "idx:users",
                FTCreateParams().On(IndexDataType.JSON).Prefix("user:"),
                Schema()
                    .AddTagField(FieldName("$.Username", "username"), true)
                    .AddNumericField(FieldName("$.DiscordId", "discord_id"), false)
                    .AddNumericField(FieldName("$.DateSignedUp", "date_signed_up"), true)
                    .AddNumericField(FieldName("$.LastLogin", "last_login"), true)
                    .AddTagField(FieldName("$.AuthToken", "auth_token"), false)
            )
            |> ignore

            db.StringIncrement("migration", 1L) |> ignore
            Logging.Debug("Migration 1 OK")

        if migration < 2L then
            Logging.Debug("Performing migration 2")

            ft.Create(
                "idx:scores",
                FTCreateParams().On(IndexDataType.JSON).Prefix("score:"),
                Schema()
                    .AddTagField(FieldName("$.RulesetId", "ruleset_id"), false)
                    .AddNumericField(FieldName("$.UserId", "user_id"), false)
                    .AddNumericField(FieldName("$.Timestamp", "timestamp"), true)
                    .AddNumericField(FieldName("$.Score", "score"), true)
                    .AddNumericField(FieldName("$.Grade", "grade"), true)
                    .AddNumericField(FieldName("$.Lamp", "lamp"), true)
                    .AddNumericField(FieldName("$.Rate", "rate"), true)
            )
            |> ignore

            db.StringIncrement("migration", 2L) |> ignore
            Logging.Debug("Migration 2 OK")

        if migration < 4L then
            Logging.Debug("Performing migration 3")

            ft.Alter("idx:scores", Schema().AddTagField(FieldName("$.ChartId", "hash"), false))
            |> ignore

            db.StringIncrement("migration", 1L) |> ignore
            Logging.Debug("Migration 3 OK")

        if migration < 5L then
            Logging.Debug("Performing migration 4")

            ft.Create(
                "idx:table_suggest_add",
                FTCreateParams().On(IndexDataType.JSON).Prefix("table_suggest_add:"),
                Schema()
                    .AddTagField(FieldName("$.TableFor", "table_for"), false)
                    .AddNumericField(FieldName("$.UserId", "user_id"), false)
                    .AddNumericField(FieldName("$.Timestamp", "timestamp"), true)
            )
            |> ignore

            db.StringIncrement("migration", 1L) |> ignore
            Logging.Debug("Migration 4 OK")
