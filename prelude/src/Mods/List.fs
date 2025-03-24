namespace Prelude.Mods

open Prelude

/// This module defines the list of mods that are available ingame, their display order and application order

module Mods =

    let AVAILABLE_MODS : Map<string, Mod> =
        Map.ofList [

            "mirror",
            { Mod.Default with
                Status = ModStatus.Ranked
                Exclusions = [ "shuffle"; "random" ]
                Apply = fun _ mc -> Mirror.apply mc
                Shorthand = fun _ -> "MR"
            }

            "shuffle",
            { Mod.Default with
                Status = ModStatus.Unranked
                RandomSeed = true
                Apply = fun s mc -> Randomise.shuffle s mc
                Exclusions = [ "random"; "mirror" ]
                Shorthand = fun _ -> "SHF"
            }

            "random",
            { Mod.Default with
                Status = ModStatus.Unranked
                RandomSeed = true
                Apply = fun s mc -> Randomise.randomise s mc
                Exclusions = [ "shuffle"; "mirror" ]
                Shorthand = fun _ -> "RD"
            }

            "nosv",
            { Mod.Default with
                Status = ModStatus.Unranked
                Apply = fun _ mc -> NoSV.apply mc
                Shorthand = fun _ -> "NSV"
            }

            "noln",
            { Mod.Default with
                Status = ModStatus.Unranked
                States = 4
                Exclusions = []
                Apply =
                    fun state mc ->
                        match state with
                        | 0 -> NoLN.apply mc
                        | 1 -> NoLN.apply_shorter_than 1.0f<beat> mc
                        | 2 -> NoLN.apply_shorter_than 0.5f<beat>  mc
                        | 3 -> NoLN.apply_shorter_than 0.25f<beat> mc
                        | _ -> failwith "impossible"
                Shorthand = function 3 -> "LN-1" | 2 -> "LN-2" | 1 -> "LN-3" | _ -> "NLN"
            }

            "inverse",
            { Mod.Default with
                Status = ModStatus.Unranked
                States = 3
                Exclusions = []
                Apply = fun state mc ->
                    match state with
                    | 0 -> Inverse.apply 0.25f<beat> mc
                    | 1 -> Inverse.apply 0.125f<beat> mc
                    | 2 -> Inverse.apply 0.5f<beat> mc
                    | _ -> failwith "impossible"
                Shorthand = function 1 -> "INV+1" | 2 -> "INV-1" | _ -> "INV"
            }

            "more_notes",
            { Mod.Default with
                Status = ModStatus.Unstored
                States = 2
                Apply = fun s mc -> if s = 1 then MoreNotes.apply_chordjacks mc else MoreNotes.apply_minijacks mc
                Shorthand = function 1 -> "MNC" | _ -> "MNM"
            }
        ]

    let APPLICATION_PRIORITY_ORDER =
        [
            "noln"
            "more_notes"
            "mirror"
            "shuffle"
            "random"
            "inverse"
            "nosv"
        ]

    let MENU_DISPLAY_ORDER =
        [
            "mirror"
            "random"
            "shuffle"
            "inverse"
            "more_notes"
            "noln"
            "nosv"
        ]

    do
        assert(APPLICATION_PRIORITY_ORDER.Length = AVAILABLE_MODS.Count)
        assert(MENU_DISPLAY_ORDER.Length = AVAILABLE_MODS.Count)

    let name (id: string) (state: int option) : string =
        match state with
        | Some i when i > 0 -> Localisation.localise (sprintf "mod.%s.%i" id i)
        | _ -> Localisation.localise (sprintf "mod.%s" id)

    let desc (id: string) (state: int option) : string =
        match state with
        | Some i when i > 0 -> Localisation.localise (sprintf "mod.%s.%i.desc" id i)
        | _ -> Localisation.localise (sprintf "mod.%s.desc" id)