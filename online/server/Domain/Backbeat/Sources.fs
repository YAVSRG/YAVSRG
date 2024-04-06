namespace Interlude.Web.Server.Domain.Backbeat

open Prelude
open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Data.Sqlite
open Prelude
open Interlude.Web.Server

type Source =
    {
        Id: string
        Mirrors: string list
        Namespace: string
    }

module Source =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE sources (
                Id TEXT PRIMARY KEY NOT NULL,
                Mirrors TEXT NOT NULL,
                Namespace TEXT NOT NULL,
                LastUpdated INTEGER NOT NULL
            );
            """
        }

    let private ADD_OR_UPDATE: NonQuery<Source> =
        {
            SQL =
                """
            INSERT OR REPLACE INTO sources (Id, Mirrors, Namespace, LastUpdated)
            VALUES (@Id, @Mirrors, @Namespace, @LastUpdated);
            """
            Parameters =
                [
                    "@Id", SqliteType.Text, -1
                    "@Mirrors", SqliteType.Text, -1
                    "@Namespace", SqliteType.Text, -1
                    "@LastUpdated", SqliteType.Integer, 8
                ]
            FillParameters =
                (fun p source ->
                    p.String source.Id
                    p.Json JSON source.Mirrors
                    p.String source.Namespace
                    p.Int64(Timestamp.now ())
                )
        }

    let add_or_update (source: Source) =
        ADD_OR_UPDATE.Execute source backbeat_db |> expect |> ignore

    let private BY_ID: Query<string, Source> =
        {
            SQL =
                """
            SELECT Id, Mirrors, Namespace FROM sources
            WHERE Id = @Id;
            """
            Parameters = [ "@Id", SqliteType.Text, -1 ]
            FillParameters = fun p id -> p.String id
            Read =
                (fun r ->
                    {
                        Id = r.String
                        Mirrors = r.Json JSON
                        Namespace = r.String
                    }
                )
        }

    let by_id (id: string) =
        BY_ID.Execute id backbeat_db |> expect |> Array.tryExactlyOne
