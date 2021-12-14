namespace Interlude.UI.Screens.LevelSelect

open System.Drawing
open Prelude.Common
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Scoring
open Interlude.UI
open Interlude.Gameplay
open Interlude.UI.Screens.Play

type PersonalBestData = PersonalBests<float * int> option * PersonalBests<Lamp> option * PersonalBests<bool> option
    
[<Struct>]
[<RequireQualifiedAccess>]
type Navigation =
    | Nothing
    | Backward of string * CachedChart
    /// Forward false is consumed by the selected chart, and replaced with Forward true
    /// Forward true is consumed by any other chart, and switched to instantly
    /// Combined effect is navigating forward by one chart
    | Forward of bool
    
[<Struct>]
[<RequireQualifiedAccess>]
type ScrollTo =
    | Nothing
    | Chart
    | Pack of string

module LevelSelect =

    /// Set this to true to have level select "consume" it and refresh on the next update frame
    let mutable refresh = false

/// Level select behaviours are all handled by this bunch of globals
/// It was easier than passing all this data through the entire level select tree
module private Globals =
    
    (*
         functionality wishlist:
         - hotkeys to navigate by pack/close and open quickly
         - display of keycount for charts
         - "random chart" hotkey
         - cropping of text that is too long
        
         eventual todo:
         - goals and playlists editor
    *)
    
    /// Group's name = this string => Selected chart is in this group
    let mutable selectedGroup = ""

    /// Chart's filepath = this string => It's the selected chart
    let mutable selectedChart = ""

    /// Group's name = this string => That group is expanded in level select
    /// Only one group can be expanded at a time, and it is independent of the "selected" group
    let mutable expandedGroup = ""
    
    /// Call this to scroll the level select by the given number of pixels
    let mutable scrollBy : float32 -> unit = ignore

    /// Flag that is cached by items in level select tree.
    /// When the global value increases by 1, update cache and recalculate certain things like color
    /// Gets incremented whenever this needs to happen
    let mutable colorVersionGlobal = 0

    // future todo: different color settings?
    let mutable colorFunc : Bests option -> Color = 
        function
        | None -> Color.FromArgb(90, 150, 150, 150)
        | Some b -> Color.FromArgb(80, Color.White)
    
    /// Updated whenever screen refreshes
    /// Contains name of score/hp systems being used
    let mutable scoreSystem = "SC+ (J4)"
    let mutable hpSystem = "VG"
    
    /// Set these globals to have them "consumed" in the next frame by a level select item with sufficient knowledge to do so
    let mutable scrollTo = ScrollTo.Nothing
    let mutable navigation = Navigation.Nothing

    let getPb ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) (colorFunc: 'T -> Color) =
        if r1 < rate then ( p2, r2, if r2 < rate then Color.FromArgb(127, Color.White) else colorFunc p2 )
        else ( p1, r1, colorFunc p1 )
    
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