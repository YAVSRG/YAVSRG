namespace Prelude.Gameplay.Replays

type ReplaySource =
    /// If false, more replay data may come if you call .HasNext later
    abstract member Finished: bool
    /// Only call if Finished = true
    abstract member GetFullReplay: unit -> Replay

    abstract member HasNext: ChartTime -> bool
    // Only call if HasNext(max_desired_time) = true
    abstract member GetNext: unit -> ReplayFrame

    /// Walk backwards through recent inputs (for rendering input meter hud element)
    /// todo: change to method taking action? captures intent better as you cannot store this seq and use it later, only immediately
    abstract member EnumerateRecentFrames: unit -> ReplayFrame seq