namespace Interlude.UI.Screens.LevelSelect

open System.Drawing
open Prelude.Common
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Scoring
open Interlude.UI
open Interlude.Content
open Interlude.Gameplay
open Interlude.UI.Screens.Play

type PersonalBestData = PersonalBests<float * int> option * PersonalBests<Lamp> option * PersonalBests<bool> option
    
[<Struct>]
[<RequireQualifiedAccess>]
type Navigation =
    | Nothing
    | Backward of string * CachedChart
    | Forward of bool
    
[<Struct>]
[<RequireQualifiedAccess>]
type ScrollTo =
    | Nothing
    | Chart
    | Pack of string

module LevelSelect =

    let mutable refresh = false

module private Globals =
    
    // functionality wishlist:
    // - hotkeys to navigate by pack/close and open quickly
    // - display of keycount for charts
    // - "random chart" hotkey
    // - cropping of text that is too long
        
    // eventual todo:
    // - goals and playlists editor
    
    let mutable selectedGroup = ""
    let mutable selectedChart = "" //filepath
    let mutable expandedGroup = ""
    
    let mutable scrollBy : float32 -> unit = ignore
    let mutable colorVersionGlobal = 0

    let getPb ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) (colorFunc: 'T -> Color) =
        if r1 < rate then ( p2, r2, if r2 < rate then Color.FromArgb(127, Color.White) else colorFunc p2 )
        else ( p1, r1, colorFunc p1 )

    // future todo: different color settings?
    let mutable colorFunc : Bests option -> Color = 
        function
        | None -> Color.FromArgb(90, 150, 150, 150)
        | Some b -> Color.FromArgb(80, Color.White)
    
    // updated whenever screen refreshes
    let mutable scoreSystem = "SC+ (J4)"
    let mutable hpSystem = "VG"
    
    let mutable scrollTo = ScrollTo.Nothing
    let mutable navigation = Navigation.Nothing
    
    let switchCurrentChart(cc, groupName) =
        match Library.load cc with
        | Some c ->
            changeChart(cc, c)
            selectedChart <- cc.FilePath
            expandedGroup <- groupName
            selectedGroup <- groupName
            scrollTo <- ScrollTo.Chart
        | None -> Logging.Error("Couldn't load cached file: " + cc.FilePath)
    
    let playCurrentChart() =
        if currentChart.IsSome then
            Screen.changeNew (fun () -> new Screen(if autoplay then PlayScreenType.Auto else PlayScreenType.Normal) :> Screen.T) Screen.Type.Play Screen.TransitionFlag.Default
        else Logging.Warn "Tried to play selected chart; There is no chart selected"