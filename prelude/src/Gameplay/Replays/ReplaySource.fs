namespace Prelude.Gameplay.Replays

// todo: represent if more data can come or if knowing the full replay is possible (HasFullReplay)
// todo: what is even the point of finished as is currently is? investigate and then change to be clearer
type ReplaySource =
    abstract member Finished: bool
    abstract member GetFullReplay: unit -> Replay

    abstract member HasNext: ChartTime -> bool
    // Only call if HasNext(max_desired_time) = true
    abstract member GetNext: unit -> ReplayFrame

    /// Walk backwards through recent inputs (for rendering input meter hud element)
    /// todo: change to method taking action? captures intent better as you cannot store this seq and use it later, only immediately
    abstract member EnumerateRecentFrames: unit -> ReplayFrame seq