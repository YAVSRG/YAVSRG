namespace Interlude.UI

type Screen() =
    inherit Widget()
    abstract member OnEnter: Screen -> unit
    default this.OnEnter(prev: Screen) = ()
    abstract member OnExit: Screen -> unit
    default this.OnExit(next: Screen) = ()

//Collection of mutable values to "tie the knot" in mutual dependence
// - Stuff is defined but not inialised here
// - Stuff is then referenced by screen logic
// - Overall screen manager references screen logic AND initialises values, connecting the loop

module Screens =
    let mutable internal addScreen: Screen -> unit = ignore
    let mutable internal popScreen: unit -> unit = ignore
    //add dialog
    //background fbo
    //accent color as animation