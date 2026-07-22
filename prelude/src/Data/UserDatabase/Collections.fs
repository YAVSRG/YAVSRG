namespace Prelude.Data.Library

open System.Collections.Generic
open Percyqaz.Data
open Percyqaz.Common
open Prelude
open Prelude.Mods

// todo: this is user data and should be moved to Prelude.Data.User

[<Json.AutoCodec>]
type CollectionEntry =
    {
        mutable Hash: string
    }
    static member OfChart(chart_meta: ChartMeta) = { Hash = chart_meta.Hash }

[<Json.AutoCodec>]
type Folder =
    {
        Charts: ResizeArray<CollectionEntry>
        Icon: Setting<string>
    }
    static member Default =
        {
            Charts = ResizeArray<CollectionEntry>()
            Icon = Setting.simple ""
        }

    static member Create(icon: string) =
        {
            Charts = ResizeArray<CollectionEntry>()
            Icon = Setting.simple icon
        }

    member this.Add(chart: ChartMeta) : bool =
        let entry = CollectionEntry.OfChart chart

        if this.Charts.Contains entry then
            false
        else
            this.Charts.Add entry
            true

    member this.Remove(chart: ChartMeta) : bool =
        let entry = CollectionEntry.OfChart chart
        this.Charts.Remove entry

    member this.Contains(chart: ChartMeta) : bool =
        let entry = CollectionEntry.OfChart chart
        this.Charts.Contains entry

[<Json.AutoCodec>]
type PlaylistEntryInfo =
    {
        Mods: Setting<ModState>
        Rate: Setting.Bounded<Rate>
    }
    static member Create(rate, mods) =
        {
            Mods = Setting.simple mods
            Rate = Setting.rate rate
        }

    static member Default = PlaylistEntryInfo.Create(1.0f<rate>, Map.empty)

[<Json.AutoCodec>]
type Playlist =
    {
        Charts: ResizeArray<CollectionEntry * PlaylistEntryInfo>
        Icon: Setting<string>
        // Maybe in future, info about how the playlist is played or pacemaker targets or stuff
        // Useful for dans, warmup sets, marathons
    }
    static member Default =
        {
            Charts = ResizeArray<CollectionEntry * PlaylistEntryInfo>()
            Icon = Setting.simple ""
        }

    static member Create(icon: string) =
        {
            Charts = ResizeArray<CollectionEntry * PlaylistEntryInfo>()
            Icon = Setting.simple icon
        }

    member this.Add(chart: ChartMeta, rate: Rate, mods: ModState) =
        let entry = CollectionEntry.OfChart chart
        let plEntry = PlaylistEntryInfo.Create(rate, mods)
        this.Charts.Add(entry, plEntry)
        true

    member this.RemoveSingle(chart: ChartMeta) =
        let entry = CollectionEntry.OfChart chart
        let found = this.Charts.FindAll(fun (e, _) -> e = entry)

        if Seq.length found = 1 then
            found |> Seq.exactlyOne |> this.Charts.Remove
        else
            false

    member this.RemoveAt(index: int) =
        try
            this.Charts.RemoveAt index
            true
        with err ->
            Logging.Error "Error removing chart from playlist: %O" err
            false

    member this.MoveChartUp(index: int) =
        if index < 1 then
            false
        else

            let item = this.Charts.[index]
            this.Charts.RemoveAt index
            this.Charts.Insert(index - 1, item)
            true

    member this.MoveChartDown(index: int) =
        if index > this.Charts.Count - 2 then
            false
        else

            let item = this.Charts.[index]
            this.Charts.RemoveAt index
            this.Charts.Insert(index + 1, item)
            true

    member this.Contains(chart: ChartMeta) : bool =
        let entry = CollectionEntry.OfChart chart
        this.Charts |> Seq.tryFind (fun (e, _) -> e = entry) |> Option.isSome

type Collection =
    | Folder of Folder
    | Playlist of Playlist

[<Json.AutoCodec(false)>]
type Collections =
    {
        Folders: Dictionary<string, Folder>
        Playlists: Dictionary<string, Playlist>
        mutable Likes: Set<string>
    }
    static member Default =
        {
            Folders = Dictionary()
            Playlists = Dictionary()
            Likes = Set.empty
        }

    member this.EnumerateLikes : string seq = this.Likes :> seq<string>
    member this.Like(id: string) = this.Likes <- this.Likes.Add id
    member this.Unlike(id: string) = this.Likes <- this.Likes.Remove id
    member this.IsLiked(id: string) = this.Likes.Contains id

    member this.List : (string * Collection) seq =
        seq {
            for c in this.Folders.Keys do
                yield (c, Folder this.Folders.[c])

            for p in this.Playlists.Keys do
                yield (p, Playlist this.Playlists.[p])
        }

    member this.Get(name: string) : Collection option =
        if this.Folders.ContainsKey name then
            Some(Folder this.Folders.[name])
        elif this.Playlists.ContainsKey name then
            Some(Playlist this.Playlists.[name])
        else
            None

    member this.GetFolder(name: string) : Folder option =
        if this.Folders.ContainsKey name then
            Some this.Folders.[name]
        else
            None

    member this.GetPlaylist(name: string) : Playlist option =
        if this.Playlists.ContainsKey name then
            Some this.Playlists.[name]
        else
            None

    member this.Exists(name: string) : bool = this.Get(name).IsSome

    member this.CreateFolder(name: string, icon: string) : Folder option =
        if this.Exists name || name = "" then
            None
        else
            let folder = Folder.Create icon
            this.Folders.Add(name, folder)
            Some folder

    member this.CreatePlaylist(name: string, icon: string) : Playlist option =
        if this.Exists name || name = "" then
            None
        else
            let playlist = Playlist.Create icon
            this.Playlists.Add(name, playlist)
            Some playlist

    member this.Delete(name: string) : bool =
        if this.Folders.ContainsKey name then
            this.Folders.Remove name
        elif this.Playlists.ContainsKey name then
            this.Playlists.Remove name
        else
            false

    member this.RenameCollection(old_name: string, new_name: string) : bool =
        if this.Exists new_name || new_name = "" then
            false
        elif not (this.Folders.ContainsKey old_name) then
            false
        else

        let folder = this.Folders.[old_name]
        this.Folders.Remove old_name |> ignore
        this.Folders.Add(new_name, folder)
        true

    member this.RenamePlaylist(old_name: string, new_name: string) : bool =
        if this.Exists new_name || new_name = "" then
            false
        elif not (this.Playlists.ContainsKey old_name) then
            false
        else

        let playlist = this.Playlists.[old_name]
        this.Playlists.Remove old_name |> ignore
        this.Playlists.Add(new_name, playlist)
        true