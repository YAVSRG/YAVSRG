namespace Prelude.Data.Library

open System.Collections.Generic

type GroupFunc = ChartMeta * LibraryViewContext -> int * string

type GroupMethod =
    | Normal of GroupFunc
    | Packs
    | Collections
    | Levels

module Grouping =

    let modes: IDictionary<string, GroupMethod> =
        dict
            [
                "none", Normal <| fun (c, _) -> 0, "No grouping"
                "pack", Packs
                "collection", Collections
                "level", Levels
                "difficulty", Normal format_difficulty
                "date_played", Normal format_date_last_played
                "date_installed", Normal format_date_added
                "grade", Normal grade_achieved
                "lamp", Normal lamp_achieved
                "title", Normal <| fun (c, _) -> 0, first_character c.Title
                "artist", Normal <| fun (c, _) -> 0, first_character c.Artist
                "creator", Normal <| fun (c, _) -> 0, first_character c.Creator
                "keymode", Normal <| fun (c, _) -> c.Keys, c.Keys.ToString() + "K"
                "patterns", Normal <| fun (c, ctx) -> 0, c.Patterns.Category
            ]

type Group =
    {
        Charts: (ChartMeta * LibraryContext) array
        Context: LibraryGroupContext
    }

type private GroupWithSorting =
    {
        Charts: ResizeArray<ChartMeta * LibraryContext * SortingTag>
        Context: LibraryGroupContext
    }
    member this.ToGroup (reverse_sorting: bool) : Group =
        {
            Charts =
                this.Charts
                |> Seq.sortBy (fun (_, _, key) -> key)
                |> if reverse_sorting then Seq.rev else id
                |> Seq.map (fun (chart_meta, ctx, _) -> (chart_meta, ctx))
                |> Array.ofSeq
            Context = this.Context
        }