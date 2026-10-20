namespace Prelude.Charts

open System
open Percyqaz.Data
open Prelude

[<Json.AutoCodec>]
type OsuOrigin =
    {
        Md5: string
        BeatmapSetId: int
        BeatmapId: int
        SourceRate: Rate
        SourceOD: float32
        FirstNoteOffset: Time
    }

[<Json.AutoCodec>]
type QuaverOrigin =
    {
        Md5: string
        MapSetId: int
        MapId: int
        FirstNoteOffset: Time
    }

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
[<CustomComparison>]
[<CustomEquality>]
[<StructuredFormatDisplay("{InfoString}")>]
type ChartOrigin =
    | Osu of OsuOrigin
    | Quaver of QuaverOrigin
    | Etterna of pack_name: string

    override this.ToString() : string =
        match this with
        | Osu _ -> "osu!"
        | Quaver _ -> "Quaver"
        | Etterna pack -> pack

    member this.InfoString: string =
        match this with
        | Osu osu -> sprintf "osu!|%s|%.2fx|%i|%i" osu.Md5 osu.SourceRate osu.BeatmapSetId osu.BeatmapId
        | Quaver quaver -> sprintf "Quaver|%s|%i|%i" quaver.Md5 quaver.MapSetId quaver.MapId
        | Etterna pack -> sprintf "Etterna|%s" pack

    member this.SuitableForUpload: bool =
        match this with
        | Osu osu -> osu.BeatmapSetId <> -1 && osu.BeatmapId <> 0
        | Quaver quaver -> quaver.MapSetId <> -1 && quaver.MapId <> 0
        | Etterna _ -> true

    interface IComparable with
        override this.CompareTo(other: obj) : int =
            match other with
            | :? ChartOrigin as origin ->
                match this, origin with
                | Etterna p1, Etterna p2 -> p1.CompareTo(p2)
                | Osu osu1, Osu osu2 ->
                    if osu1.SourceRate = osu2.SourceRate then
                        osu2.BeatmapId.CompareTo(osu1.BeatmapId)
                    else
                        (float32 osu1.SourceRate - 1.0f |> abs).CompareTo(float32 osu2.SourceRate - 1.0f |> abs)
                | Quaver quaver1, Quaver quaver2 -> quaver2.MapId.CompareTo(quaver1.MapId)
                | Etterna _, _ -> -1
                | _, Etterna _ -> 1
                | Osu _, Quaver _ -> -1
                | Quaver _, Osu _ -> 1
            | _ -> -1

    override this.Equals(other: obj) : bool =
        match other with
        | :? ChartOrigin as origin ->
            match this, origin with
            | Etterna p1, Etterna p2 -> p1 = p2
            | Osu osu1, Osu osu2 -> osu1 = osu2
            | Quaver quaver1, Quaver quaver2 -> quaver1 = quaver2
            | _ -> false
        | _ -> false

    override this.GetHashCode() : int =
        match this with
        | Etterna p -> p.GetHashCode()
        | Osu osu -> osu.GetHashCode()
        | Quaver quaver -> quaver.GetHashCode()
