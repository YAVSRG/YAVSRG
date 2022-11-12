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

    type CollectionSource = { Name: string; Position: int }

    [<RequireQualifiedAccess>]
    [<CustomEquality>]
    [<NoComparison>]
    type LibraryContext =
        | None
        | Table
        | Collection of index: int * id: string
        | Playlist of index: int * id: string * data: PlaylistData
        member this.CollectionSource : CollectionSource option =
            match this with
            | None
            | Table -> Option.None
            | Collection (i, id)
            | Playlist (i, id, _) -> Some { Name = id; Position = i }
        override this.Equals(other: obj) =
            match other with
            | :? LibraryContext as other ->
                this.CollectionSource = other.CollectionSource
            | _ -> false
        override this.GetHashCode() = this.CollectionSource.GetHashCode()