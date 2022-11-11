namespace Prelude.Data.Charts

open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Common
open Prelude.Gameplay.Mods

module Collections =

    [<RequireQualifiedAccess>]
    [<Json.AutoCodec>]
    type Goal =
    | None
    | Clear of rulesetId: string
    | Lamp of rulesetId: string * lamp: int
    | Accuracy of rulesetId: string * float
    
    [<Json.AutoCodec>]
    type PlaylistData = { Mods: Setting<ModState>; Rate: Setting.Bounded<float32> } with
        static member Make mods rate = { Mods = Setting.simple mods; Rate = Setting.rate rate }

    [<Json.AutoCodec>]
    type GoalData = { Mods: Setting<ModState>; Rate: Setting.Bounded<float32>; Goal: Setting<Goal> } with
        static member Make mods rate goal = { Mods = Setting.simple mods; Rate = Setting.rate rate; Goal = Setting.simple goal }
    
    [<Json.AutoCodec>]
    type Collection =
    | Collection of List<string> // duplicates not allowed
    | Playlist of List<string * PlaylistData> // order of list matters
        member this.ToCollection() =
            match this with
            | Collection l -> Collection l
            | Playlist p -> 
                p
                |> Seq.map fst
                |> Seq.distinct
                |> List
                |> Collection
        member this.ToPlaylist(mods, rate) = 
            match this with
            | Collection l ->
                l
                |> Seq.map (fun i -> i, PlaylistData.Make mods rate)
                |> List
                |> Playlist
            | Playlist p -> Playlist p
        member this.IsEmpty() =
            match this with
            | Collection l -> l.Count = 0
            | Playlist p -> p.Count = 0
        static member Blank = Collection (ResizeArray<_>())

    [<RequireQualifiedAccess>]
    [<CustomEquality>]
    [<NoComparison>]
    type LevelSelectContext =
        | None
        | Table
        | Collection of index: int * id: string
        | Playlist of index: int * id: string * data: PlaylistData
        member this.InCollection =
            match this with
            | None
            | Table -> ""
            | Collection (_, id)
            | Playlist (_, id, _) -> id
        member this.PositionInCollection =
            match this with
            | None
            | Table -> -1
            | Collection (i, _)
            | Playlist (i, _, _) -> i
        override this.Equals(other: obj) =
            match other with
            | :? LevelSelectContext as other ->
                (this.InCollection = other.InCollection) && (this.PositionInCollection = other.PositionInCollection)
            | _ -> false
        override this.GetHashCode() = (this.InCollection, this.PositionInCollection).GetHashCode()