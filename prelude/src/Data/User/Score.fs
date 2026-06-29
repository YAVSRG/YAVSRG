namespace Prelude.Data.User

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Mods

type Score =
    {
        Timestamp: int64
        Replay: byte array
        Rate: Rate
        Mods: ModState
        IsImported: bool
        IsFailed: bool
        Keys: int
    }