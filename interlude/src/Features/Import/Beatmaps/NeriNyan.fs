namespace Interlude.Features.Import

open Percyqaz.Data

[<Json.AutoCodec>]
type NeriNyanBeatmap =
    {
        id: int
        difficulty_rating: float
        cs: float
        version: string
        mode: string
    }

[<Json.AutoCodec>]
type NeriNyanBeatmapset =
    {
        id: int
        artist: string
        title: string
        creator: string
        favourite_count: int
        play_count: int
        status: string
        beatmaps: NeriNyanBeatmap array
    }

[<Json.AutoCodec>]
type NeriNyanBeatmapSearch = NeriNyanBeatmapset array

[<Json.AutoCodec>]
type NeriNyanBeatmapSearchRequest =
    {
        m: string
        page: int
        query: string
        ranked: string
        sort: string
        cs: {| min: float; max: float |}
    }