module CompareRulesets

open Prelude.Gameplay.Rulesets

let tabulate_ruleset (ruleset: Ruleset) =

    printfn ""
    printfn "%s\t|\t0\t|\t1\t|\t3\t|\t8\t|\t25\t| <- misses per 1000 notes" (ruleset.Name.Replace(" ", ""))
    printfn "vv SD\t|\t-\t|\t-\t|\t-\t|\t-\t|\t--\t|"

    for sd in [11.0; 13.0; 16.0; 20.0; 25.0] do
        printf "%.0fms\t|\t" sd

        for miss_count in [0.0; 1.0; 3.0; 8.0; 15.0] do
            printf "%.2f%%\t|\t" (RulesetComparison.simulate ruleset sd miss_count)

        printfn ""

let compare_rulesets_print (accuracy: float) (base_ruleset: Ruleset) (compare_to: Ruleset) : unit =
    match RulesetComparison.compare accuracy base_ruleset compare_to with
    | Some cmp ->
        printfn "%.1f%% on %s is about %.1f%% (%.1f%% - %.1f%%) on %s" accuracy base_ruleset.Name cmp.Average cmp.Min cmp.Max compare_to.Name
    | None ->
        printfn "Failed to decently compare %.1f%% on %s to an equivalent on %s" accuracy base_ruleset.Name compare_to.Name

let multi_compare (base_ruleset: Ruleset) (compare_to: Ruleset) =

    compare_rulesets_print 91.0 base_ruleset compare_to
    compare_rulesets_print 92.0 base_ruleset compare_to
    compare_rulesets_print 93.0 base_ruleset compare_to
    compare_rulesets_print 94.0 base_ruleset compare_to
    compare_rulesets_print 95.0 base_ruleset compare_to
    compare_rulesets_print 96.0 base_ruleset compare_to
    compare_rulesets_print 97.0 base_ruleset compare_to
    compare_rulesets_print 98.0 base_ruleset compare_to
    printfn ""

let main() =

    let sc_4 = SC.create 4
    let osu_8 = OsuMania.create 8.0f OsuMania.NoMod
    let osu_5 = OsuMania.create 5.0f OsuMania.NoMod
    let wife_4 = Wife3.create 4
    let wife_5 = Wife3.create 5
    let quaver_standard = Quaver.create Quaver.Standard

    multi_compare sc_4 sc_4

    multi_compare sc_4 osu_8
    multi_compare osu_8 sc_4
    multi_compare sc_4 quaver_standard
    multi_compare quaver_standard sc_4
    multi_compare sc_4 wife_4
    multi_compare wife_4 sc_4

    multi_compare sc_4 osu_5
    multi_compare sc_4 wife_5
    multi_compare wife_4 wife_5
    multi_compare osu_8 osu_5