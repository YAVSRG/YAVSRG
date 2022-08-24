namespace Interlude.Features.LevelSelect

module LevelSelect =

    /// Set this to true to have level select "consume" it and refresh on the next update frame
    let mutable refresh = false

    /// Same as above, but just refresh minor info like pbs, whether charts are in collections or not, etc
    let mutable minorRefresh = false