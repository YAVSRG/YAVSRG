namespace Interlude.Features.LevelSelect

open System
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Interlude.Features.Gameplay
open Interlude.UI

module LevelSelect =

    /// Set this to true to have level select "consume" it and refresh on the next update frame
    let mutable refresh = false

    /// Same as above, but just refresh minor info like pbs, whether charts are in collections or not, etc
    let mutable minorRefresh = false