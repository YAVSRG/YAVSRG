namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI

module LevelSelect =

    let private refresh_all_event = Event<unit>()
    let private refresh_details_event = Event<unit>()

    let refresh_all() = sync(refresh_all_event.Trigger)
    let refresh_details() = sync(refresh_details_event.Trigger)

    let on_refresh_all = refresh_all_event.Publish
    let on_refresh_details = refresh_details_event.Publish