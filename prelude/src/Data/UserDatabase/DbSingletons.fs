namespace Prelude.Data.User

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude

module DbSingletons =

    let internal CREATE_TABLE: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE TABLE singletons (
                Id TEXT PRIMARY KEY NOT NULL,
                Data TEXT NOT NULL
            );
            """
        }

    let private GET: Query<string, 'T> =
        {
            SQL =
                """SELECT Data FROM singletons WHERE Id = @Id;"""
            Parameters = [ "@Id", SqliteType.Text, -1 ]
            FillParameters = fun p id -> p.String id
            Read = fun r -> r.Json JSON
        }

    let get_or_default (id: string) (default_value: 'T) (db: Database) : 'T =
        GET.Execute id db
        |> expect
        |> Array.tryExactlyOne
        |> Option.defaultValue default_value

    let private SAVE: NonQuery<string * 'T> =
        {
            SQL =
                """
                INSERT OR REPLACE INTO singletons (Id, Data)
                VALUES (@Id, json(@Data));
            """
            Parameters = [ "@Id", SqliteType.Text, -1; "@Data", SqliteType.Text, -1 ]
            FillParameters = fun p (id, data: 'T) ->
                p.String id
                p.Json JSON data
        }

    [<RequiresExplicitTypeArguments>]
    let save<'T> (id: string) (value: 'T) (db: Database) =
        SAVE.Execute (id, value) db |> expect |> ignore