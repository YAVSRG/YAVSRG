namespace Prelude.Data.Scores

open Prelude.Gameplay.Mods

type NewScore =
    {
        Timestamp: int64
        Replay: byte array
        Rate: float32
        Mods: ModState
        IsImported: bool
        Keys: int
    }