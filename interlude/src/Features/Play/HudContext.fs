namespace Interlude.Features.Play

open System.Collections.Generic
open Percyqaz.Flux.UI
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Online

[<RequireQualifiedAccess>]
type HudContextInner =
    | Play
    | Practice
    | Replay of auto: bool * overlay_shown: (unit -> bool)
    | Spectate of replays: Dictionary<string, LobbyPlayerReplayInfo>
    | Multiplayer of replays: Dictionary<string, LobbyPlayerReplayInfo>
    | Editor

type HudContext =
    {
        Screen: IContainer<Widget>
        Playfield: Playfield
        State: PlayState
        Config: HudConfig
        Inner: HudContextInner
    }