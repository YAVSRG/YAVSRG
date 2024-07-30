namespace Interlude.Features.Import.osu

open Percyqaz.Data

[<Json.AutoCodec>]
type MinoBeatmap =
    {
        id: int
        difficulty_rating: float
        cs: float
        version: string
        mode: string
    }

[<Json.AutoCodec>]
type MinoBeatmapSet =
    {
        id: int
        artist: string
        title: string
        creator: string
        favourite_count: int
        play_count: int
        status: string
        beatmaps: MinoBeatmap array
    }

type MinoBeatmapSearch = MinoBeatmapSet array