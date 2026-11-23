namespace Interlude.Features.Import.Osu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type BeatmapStatusToggle =
    static member Create(label: string, status: int, color: Color, statuses: Setting<Set<int>>) : AngledButton =
        AngledButton(
            (fun () -> if statuses.Value.Contains(status) then Icons.CHECK + " " + label else Icons.X + " " + label),
            (fun () ->
                if statuses.Value.Contains(status) then
                    Setting.app (Set.remove status) statuses
                else
                    Setting.app (Set.add status) statuses
            ),
            (fun () -> if statuses.Value.Contains(status) then color.O3 else color.O1)
        )
