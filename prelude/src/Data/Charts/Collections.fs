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
    | Goals of List<string * GoalData>
        member this.ToCollection() =
            match this with
            | Collection l -> Collection l
            | Playlist p -> 
                Seq.map fst p
                |> Seq.distinct |> List |> Collection
            | Goals g -> 
                Seq.map fst g
                |> Seq.distinct |> List |> Collection
        member this.ToPlaylist(mods, rate) = 
            match this with
            | Collection l -> 
                Seq.map (fun i -> i, PlaylistData.Make mods rate) l
                |> List |> Playlist
            | Playlist p -> Playlist p
            | Goals g -> 
                Seq.map (fun (x, data: GoalData) -> x, PlaylistData.Make data.Mods.Value data.Rate.Value) g
                |> List |> Playlist
        member this.ToGoals(mods, rate) = 
            match this with
            | Collection l ->
                Seq.map (fun i -> i, GoalData.Make mods rate Goal.None) l
                |> List |> Goals
            | Playlist p ->
                Seq.map (fun (x, data: PlaylistData) -> x, GoalData.Make data.Mods.Value data.Rate.Value Goal.None) p
                |> List |> Goals
            | Goals g -> Goals g
        member this.IsEmpty() =
            match this with
            | Collection l -> l.Count = 0
            | Playlist p -> p.Count = 0
            | Goals g -> g.Count = 0
        static member Blank = Collection (ResizeArray<_>())

    [<RequireQualifiedAccess>]
    [<CustomEquality>]
    [<NoComparison>]
    type LevelSelectContext =
        | None
        | Table
        | Collection of index: int * id: string
        | Playlist of index: int * id: string * data: PlaylistData
        | Goal of index: int * id: string * data: GoalData
        member this.InCollection =
            match this with
            | None
            | Table -> ""
            | Collection (_, id)
            | Playlist (_, id, _)
            | Goal (_, id, _) -> id
        member this.PositionInCollection =
            match this with
            | None
            | Table -> -1
            | Collection (i, _)
            | Playlist (i, _, _)
            | Goal (i, _, _) -> i
        override this.Equals(other: obj) =
            match other with
            | :? LevelSelectContext as other ->
                (this.InCollection = other.InCollection) && (this.PositionInCollection = other.PositionInCollection)
            | _ -> false
        override this.GetHashCode() = (this.InCollection, this.PositionInCollection).GetHashCode()