namespace Interlude.Features.Online.Players

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude

module private PlayerListContext =

    let mutable current_player = None
    let mutable player_changed = ignore
    let mutable friends_changed = ignore

    let switch (player: string option) =
        current_player <- player
        player_changed()

    let update_friends_list () = friends_changed()
