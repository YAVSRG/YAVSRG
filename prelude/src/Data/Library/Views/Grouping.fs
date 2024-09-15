namespace Prelude.Data.Library

open System.Collections.Generic

type GroupMethod = 
    {
        Func: ChartMeta * LibraryViewContext -> (int * string) seq
        IsPacks: bool
    }

module Grouping =

    let modes: IDictionary<string, GroupMethod> =
        dict
            [
                "none", { IsPacks = false; Func = fun (c, _) -> [0, "No grouping"] }
                "pack", { IsPacks = true; Func = fun (c, _) -> c.Packs |> Seq.map (fun p -> 0, p) }
                "date_played", { IsPacks = false; Func = format_date_last_played >> Seq.singleton }
                "date_installed", { IsPacks = false; Func = format_date_added >> Seq.singleton }
                "grade", { IsPacks = false; Func = grade_achieved >> Seq.singleton }
                "lamp", { IsPacks = false; Func = lamp_achieved >> Seq.singleton }
                "title", { IsPacks = false; Func = fun (c, _) -> [0, first_character c.Title] }
                "artist", { IsPacks = false; Func = fun (c, _) -> [0, first_character c.Artist]  }
                "creator", { IsPacks = false; Func = fun (c, _) -> [0, first_character c.Creator] }
                "keymode", { IsPacks = false; Func = fun (c, _) -> [c.Keys, c.Keys.ToString() + "K"] }
                "patterns", { IsPacks = false; Func = fun (c, ctx) -> [0, c.Patterns.Category.Category] }
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
                |> Seq.distinctBy (fun (cc, _, _) -> cc.Hash)
                |> Seq.sortBy (fun (_, _, key) -> key)
                |> if reverse_sorting then Seq.rev else id
                |> Seq.map (fun (cc, ctx, _) -> (cc, ctx))
                |> Array.ofSeq
            Context = this.Context
        }