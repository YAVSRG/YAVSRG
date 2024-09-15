namespace Prelude.Data.Library

open System.Collections.Generic

type GroupMethod = ChartMeta * LibraryViewContext -> (int * string) seq

module Grouping =

    let modes: IDictionary<string, GroupMethod> =
        dict
            [
                "none", (fun (c, _) -> [0, "No grouping"])
                "pack", (fun (c, _) -> c.Packs |> Seq.map (fun p -> 0, p))
                "date_played", format_date_last_played >> Seq.singleton
                "date_installed", format_date_added >> Seq.singleton
                "grade", grade_achieved >> Seq.singleton
                "lamp", lamp_achieved >> Seq.singleton
                "title", (fun (c, _) -> [0, first_character c.Title])
                "artist", (fun (c, _) -> [0, first_character c.Artist])
                "creator", (fun (c, _) -> [0, first_character c.Creator])
                "keymode", (fun (c, _) -> [c.Keys, c.Keys.ToString() + "K"])
                "patterns", fun (c, ctx) -> [0, c.Patterns.Category.Category]
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
    member this.ToGroup : Group =
        {
            Charts = 
                this.Charts
                |> Seq.distinctBy (fun (cc, _, _) -> cc.Hash)
                |> Seq.sortBy (fun (_, _, key) -> key)
                |> Seq.map (fun (cc, ctx, _) -> (cc, ctx))
                |> Array.ofSeq
            Context = this.Context
        }