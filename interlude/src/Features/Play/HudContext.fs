namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Prelude.Skins.HudLayouts
open Interlude.Features.Play

type HudContext =
    {
        Screen: IContainer<Widget>
        Playfield: Playfield
        State: PlayState
        Config: HudConfig
    }