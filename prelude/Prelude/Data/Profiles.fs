namespace Prelude.Data

open System
open System.Text.RegularExpressions
open System.ComponentModel
open Newtonsoft.Json
open Prelude.Gameplay.Layout
open Prelude.Gameplay.NoteColors

module Profiles = 

    type ProfileStats() =
        member val Plays = 0 with get, set
    
    type Profile() =
        //todo: private set, except this isn't part of F# yet
        [<DefaultValue("Default Profile")>] member val Name = "Default Profile" with get, set
        member val UUID = Guid.NewGuid().ToString()
        member val Stats = ProfileStats()

        member val ScrollSpeed = 2.05 with get, set
        member val HitPosition = 0 with get, set
        member val HitLighting = false with get, set
        member val Upscroll = false with get, set
        member val BackgroundDim = 0.5 with get, set
        member val PerspectiveTilt = 0.0 with get, set
        member val ScreenCoverUp = 0.0 with get, set
        member val ScreenCoverDown = 0.0 with get, set
        member val ScreenCoverFadeLength = 200 with get, set
        member val ColorStyle = ColorConfig.Default with get, set

        member val KeymodePreference = None with get, set
        //pacemaker, score saving, fail behaviour
        //choice of score system, hp system

        member val SelectedThemes = ResizeArray<string>()
        member val ChartSortMode = "Title" with get, set
        member val ChartGroupMode = "Pack" with get, set
        member val ChartColorMode = "Nothing" with get, set

        //keybinds (not sure how I will implement this yet)

        member val Playstyles = [|Layout.OneHand, Layout.Spread, Layout.LeftOne, Layout.Spread, Layout.LeftOne, Layout.Spread, Layout.LeftOne, Layout.Spread|]
        member this.Rename (name) = 
            this.Name <- Regex("[^a-zA-Z0-9_-]").Replace(name, "")

