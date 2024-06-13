namespace Interlude.Features.Import.Etterna

open Percyqaz.Data

[<Json.AutoCodec>]
type EtternaOnlinePack =
    {
        id: int
        name: string
        play_count: int
        song_count: int
        contains_nsfw: bool
        size: string
        overall: float
        download: string
    }

[<Json.AutoCodec>]
type EtternaOnlineApiResponse =
    {
        data: EtternaOnlinePack array
        meta: {| current_page: int; last_page: int |}
    }