namespace Prelude.Data.Charts

open System
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Common
open Prelude.Gameplay.Mods
open Prelude.Data.Charts.Caching

module Collections =

    [<Json.AutoCodec>]
    [<CustomEquality>]
    [<CustomComparison>]
    type Entry = 
        { mutable Hash: string; mutable Path: string }
        override this.Equals(other) =
            match other with
            | :? Entry as other -> other.Hash = this.Hash || other.Path = this.Path
            | _ -> false
        override this.GetHashCode() = this.Hash.GetHashCode()
        static member OfChart(cc: CachedChart) =
            { Hash = cc.Hash; Path = cc.FilePath }
        interface IComparable with
            member this.CompareTo other =
                match other with
                | :? Entry as other -> other.Hash.CompareTo this.Hash
                | _ -> -1

    [<Json.AutoCodec>]
    type Folder =
        {
            Charts: ResizeArray<Entry>
            Icon: Setting<string>
        }
        static member Default = { Charts = ResizeArray<Entry>(); Icon = Setting.simple "" }

        static member Create icon =
            { Charts = ResizeArray<Entry>(); Icon = Setting.simple icon }

        member this.Add (chart: CachedChart) : bool =
            let entry = Entry.OfChart chart
            if this.Charts.Contains entry then false
            else this.Charts.Add entry; true

        member this.Remove (chart: CachedChart) : bool =
            let entry = Entry.OfChart chart
            this.Charts.Remove entry

        member this.Contains (chart: CachedChart) : bool =
            let entry = Entry.OfChart chart
            this.Charts.Contains entry

    [<Json.AutoCodec>]
    type PlaylistEntryInfo = 
        {
            Mods: Setting<ModState>
            Rate: Setting.Bounded<float32>
        }
        static member Create(rate, mods) =
            {
                Mods = Setting.simple mods
                Rate = Prelude.Common.Setting.rate rate
            }
        static member Default = PlaylistEntryInfo.Create(1.0f, Map.empty)

    [<Json.AutoCodec>]
    type Playlist =
        {
            Charts: ResizeArray<Entry * PlaylistEntryInfo>
            Icon: Setting<string>
            // Maybe in future, info about how the playlist is played or pacemaker targets or stuff
            // Useful for dans, warmup sets, marathons
        }
        static member Default = { Charts = ResizeArray<Entry * PlaylistEntryInfo>(); Icon = Setting.simple "" }

        static member Create icon =
            { Charts = ResizeArray<Entry * PlaylistEntryInfo>(); Icon = Setting.simple icon }

        member this.Add(chart: CachedChart, rate: float32, mods: ModState) =
            let entry = Entry.OfChart chart
            let plEntry = PlaylistEntryInfo.Create(rate, mods)
            this.Charts.Add (entry, plEntry)
            true

        member this.RemoveSingle(chart: CachedChart) =
            let entry = Entry.OfChart chart
            let found = this.Charts.FindAll(fun (e, _) -> e = entry)
            if Seq.length found = 1 then
                found |> Seq.exactlyOne|> this.Charts.Remove
            else false

        member this.RemoveAt(index: int) =
            try this.Charts.RemoveAt index; true
            with err -> Logging.Error("Error removing chart from playlist", err); false

        member this.MoveChartUp(index: int) =
            if index < 1 then false else

            let item = this.Charts.[index]
            this.Charts.RemoveAt index
            this.Charts.Insert(index - 1, item)
            true
            
        member this.MoveChartDown(index: int) =
            if index > this.Charts.Count - 2 then false else

            let item = this.Charts.[index]
            this.Charts.RemoveAt index
            this.Charts.Insert(index + 1, item)
            true
            
        member this.Contains (chart: CachedChart) : bool =
            let entry = Entry.OfChart chart
            this.Charts |> Seq.tryFind (fun (e, _) -> e = entry) |> Option.isSome

    type Collection = Folder of Folder | Playlist of Playlist

    [<Json.AutoCodec>]
    type Collections =
        {
            Folders: Dictionary<string, Folder>
            Playlists: Dictionary<string, Playlist>
        }

        member this.List : (string * Collection) seq =
            seq {
                for c in this.Folders.Keys do
                    yield (c, Folder this.Folders.[c])
                for p in this.Playlists.Keys do
                    yield (p, Playlist this.Playlists.[p])
            }

        member this.Get(name: string) : Collection option =
            if this.Folders.ContainsKey name then
                Some (Folder this.Folders.[name])
            elif this.Playlists.ContainsKey name then
                Some (Playlist this.Playlists.[name])
            else None
            
        member this.GetFolder(name: string) : Folder option =
            if this.Folders.ContainsKey name then
                Some this.Folders.[name]
            else None

        member this.GetPlaylist(name: string) : Playlist option =
            if this.Playlists.ContainsKey name then
                Some this.Playlists.[name]
            else None

        member this.Exists(name) = this.Get(name).IsSome

        member this.CreateFolder(name, icon) : Folder option =
            if this.Exists name then None
            else
                let folder = Folder.Create icon
                this.Folders.Add(name, folder)
                Some folder

        member this.CreatePlaylist(name, icon) : Playlist option =
            if this.Exists name then None
            else
                let playlist = Playlist.Create icon
                this.Playlists.Add(name, playlist)
                Some playlist

        member this.Delete(name) : bool =
            if this.Folders.ContainsKey name then this.Folders.Remove name
            elif this.Playlists.ContainsKey name then this.Playlists.Remove name
            else false

        member this.RenameCollection(oldname, newname) : bool =
            if this.Exists newname then false
            elif not (this.Folders.ContainsKey oldname) then false
            else
            let folder = this.Folders.[oldname]
            this.Folders.Remove oldname |> ignore
            this.Folders.Add(newname, folder)
            true
            
        member this.RenamePlaylist(oldname, newname) : bool =
            if this.Exists newname then false
            elif not (this.Playlists.ContainsKey oldname) then false
            else
            let playlist = this.Playlists.[oldname]
            this.Playlists.Remove oldname |> ignore
            this.Playlists.Add(newname, playlist)
            true

    type CollectionSource = { Name: string; Position: int }

    [<RequireQualifiedAccess>]
    [<CustomEquality>]
    [<NoComparison>]
    type LibraryContext =
        | None
        | Table
        | Folder of id: string
        | Playlist of index: int * id: string * data: PlaylistEntryInfo
        member this.CollectionSource : CollectionSource option =
            match this with
            | None
            | Table -> Option.None
            | Folder id -> Some { Name = id; Position = 0 }
            | Playlist (i, id, _) -> Some { Name = id; Position = i }
        override this.Equals(other: obj) =
            match other with
            | :? LibraryContext as other ->
                this.CollectionSource = other.CollectionSource
            | _ -> false
        override this.GetHashCode() = this.CollectionSource.GetHashCode()