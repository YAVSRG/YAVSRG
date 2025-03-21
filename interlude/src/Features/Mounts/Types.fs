namespace Interlude.Features.Mounts

[<RequireQualifiedAccess>]
type MountedGameType =
    | Osu
    | Quaver
    | Etterna
    | Stepmania
    override this.ToString() =
        match this with
        | Osu -> "osu!mania"
        | Quaver -> "Quaver"
        | Etterna -> "Etterna"
        | Stepmania -> "Stepmania"