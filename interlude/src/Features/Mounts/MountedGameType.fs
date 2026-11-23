namespace Interlude.Features.Mounts

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Formats.Conversions
open Prelude.Data.Imports
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import

[<RequireQualifiedAccess>]
type MountedGameType =
    | Osu
    | Quaver
    | Etterna
    | Stepmania

    override this.ToString() : string =
        match this with
        | Osu -> "osu!mania"
        | Quaver -> "Quaver"
        | Etterna -> "Etterna"
        | Stepmania -> "Stepmania"

    member this.Color =
        match this with
        | Osu -> Color.FromArgb(0xFF_FF79B8)
        | Quaver -> Color.FromArgb(0xFF_39B6D1)
        | Etterna -> Color.FromArgb(0xFF_8F60F6)
        | Stepmania -> Color.FromArgb(0xFF_FF703C)

    member this.DefaultSongsLocation: string =
        match this with
        | MountedGameType.Osu -> MountedChartSource.DEFAULT_OSU_SONGS_LOCATION
        | MountedGameType.Quaver -> MountedChartSource.DEFAULT_QUAVER_SONGS_LOCATION
        | MountedGameType.Stepmania -> MountedChartSource.DEFAULT_STEPMANIA_SONGS_LOCATION
        | MountedGameType.Etterna -> MountedChartSource.DEFAULT_ETTERNA_SONGS_LOCATION
