namespace Prelude.Mods

type ModStateType =
    | Stateless
    | MultipleModes of states: int64
    | RandomSeed
    | ColumnSwap

type Mod =
    {
        Status: ModStatus
        /// Mods can have an integer "state" passed to them when they are applied - This allows some sub-behaviours within mods
        Type: ModStateType
        /// List of mod ids this mod cannot be used with
        Exclusions: string list
        /// Returns resulting chart + flag
        /// flag is true if the mod made meaningful changes to the chart and the mod should be considered 'applied'
        Apply: int64 -> ModdedChartInternal -> ModdedChartInternal * bool
        /// Short code representing the mod, to be abbreviated in places without room for the full names
        /// Language independent
        Shorthand: int64 -> string
    }

    static member internal Default =
        {
            Status = ModStatus.Unstored
            Type = Stateless
            Exclusions = []
            Apply = (fun _ mc -> mc, false)
            Shorthand = fun _ -> "??"
        }
