namespace Prelude.Data.Library

open Prelude.Data.UserDatabase

[<RequireQualifiedAccess>]
[<NoEquality>]
[<NoComparison>]
type LibraryContext =
    | None
    | Pack of name: string
    | Table of level: int
    | Likes
    | Folder of id: string
    | Playlist of index: int * id: string * data: PlaylistEntryInfo

    member this.Matches(other: LibraryContext) : bool =
        match this, other with
        | None, None -> true
        | None, Pack _ -> true
        | Pack p, Pack p2 when p = p2 -> true
        | Pack _, None -> true
        | Table _, Table _ -> true
        | Likes, Likes -> true
        | Folder f, Folder f2 when f = f2 -> true
        | Playlist(i, id, _), Playlist(i2, id2, _) when i = i2 && id = id2 -> true
        | _ -> false

    member this.SoftMatches(other: LibraryContext) : bool =
        match this, other with
        | None, _ -> true
        | Pack _, None -> true
        | Table _, None -> true
        | Folder _, None -> true
        | Folder _, Pack _ -> true
        | Likes, None -> true
        | Likes, Pack _ -> true
        | _ -> this.Matches(other)

[<RequireQualifiedAccess>]
type LibraryGroupContext =
    | None
    | Pack of name: string
    | Table of level: int
    | Likes
    | Folder of id: string
    | Playlist of id: string
