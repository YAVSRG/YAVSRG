namespace Interlude.Web.Server.Domain.Core

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Interlude.Web.Server

type Leaderboard = { ChartId: string; RulesetId: string }

module Leaderboard =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE leaderboards (
                ChartId TEXT NOT NULL,
                RulesetId TEXT NOT NULL,
                PRIMARY KEY (ChartId, RulesetId)
            );
            """
        }

    let private CREATE: NonQuery<string * string> =
        {
            SQL =
                """
            INSERT OR IGNORE INTO leaderboards (ChartId, RulesetId)
            VALUES (@ChartId, @RulesetId);
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@RulesetId", SqliteType.Text, -1 ]
            FillParameters =
                (fun p (chart_id, ruleset_id) ->
                    p.String chart_id
                    p.String ruleset_id
                )
        }

    let create (chart_id: string) (ruleset_id: string) =
        CREATE.Execute (chart_id, ruleset_id) core_db |> expect |> ignore

    let private EXISTS: Query<string * string, int32> =
        {
            SQL =
                """
            SELECT 1 FROM leaderboards
            WHERE ChartId = @ChartId AND RulesetId = @RulesetId;
            """
            Parameters = [ "@ChartId", SqliteType.Text, -1; "@RulesetId", SqliteType.Text, -1 ]
            FillParameters =
                (fun p (chart_id, ruleset_id) ->
                    p.String chart_id
                    p.String ruleset_id
                )
            Read = (fun r -> r.Int32)
        }

    let exists (chart_id: string) (ruleset_id: string) =
        EXISTS.Execute (chart_id, ruleset_id) core_db |> expect |> Array.isEmpty |> not
