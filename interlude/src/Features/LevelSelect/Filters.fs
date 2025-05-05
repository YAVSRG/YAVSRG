namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type FiltersPage() =
    inherit Page()

    let inner_filter = LevelSelect.filter.Filter

    let ln_percent_min = inner_filter.LNPercentMin |> Option.defaultValue 0.0f |> Setting.percentf
    let ln_percent_max = inner_filter.LNPercentMax |> Option.defaultValue 1.0f |> Setting.percentf

    let difficulty_min = inner_filter.DifficultyMin |> Option.defaultValue 0.0f |> Setting.bounded (0.0f, 15.0f)
    let difficulty_max = inner_filter.DifficultyMax |> Option.defaultValue 15.0f |> Setting.bounded (0.0f, 15.0f)

    member this.SaveChanges() =
        let inner_filter = LevelSelect.filter.Filter
        LevelSelect.filter <-
            { LevelSelect.filter with
                Filter =
                    { inner_filter with
                        LNPercentMin = match ln_percent_min.Value with 0.0f -> None | other -> Some other
                        LNPercentMax = match ln_percent_max.Value with 1.0f -> None | other -> Some other

                        DifficultyMin = match difficulty_min.Value with 0.0f -> None | other -> Some other
                        DifficultyMax = match difficulty_max.Value with 15.0f -> None | other -> Some other
                    }
            }
        LevelSelect.refresh_all ()

    override this.Content() =

        this.OnClose(this.SaveChanges)

        page_container()
        |+ PageSetting("Min LN%", Slider(ln_percent_min |> Setting.trigger (fun v -> Setting.app (max v) ln_percent_max))).Pos(0)
        |+ PageSetting("Max LN%", Slider(ln_percent_max |> Setting.trigger (fun v -> Setting.app (min v) ln_percent_min))).Pos(2)

        |+ PageSetting("Min rating",
            Slider(
                difficulty_min |> Setting.trigger (fun v -> Setting.app (max v) difficulty_max),
                Format = fun x -> sprintf "%s %.2f" Icons.STAR x
            )
        ).Pos(5)
        |+ PageSetting("Max rating",
            Slider(
                difficulty_max |> Setting.trigger (fun v -> Setting.app (min v) difficulty_min),
                Format = function 15.0f -> "Any" | x -> sprintf "%s %.2f" Icons.STAR x
            )
        ).Pos(7)
        :> Widget

    override this.Title = %"levelselect.filters"