namespace Prelude.Tests.Mods

open NUnit.Framework
open Prelude.Mods

module State =

    [<Test>]
    let Basic_Usage() =

        let empty : ModState = Map.empty

        let result = ModState.cycle "mirror" empty
        Assert.AreEqual(empty.Add("mirror", 0), result)

        let result2 = ModState.cycle "mirror" result
        Assert.AreEqual(empty, result2)

    [<Test>]
    let Random_Seed() =

        let mod_state = ModState.cycle "mirror" Map.empty

        let result = ModState.cycle "random" mod_state
        Assert.True(result.ContainsKey "random")

        printfn "%A" result

        let result2 = ModState.cycle "random" result
        Assert.False(result2.ContainsKey "random")

        printfn "%A" result2

    [<Test>]
    let Mutual_Exclusion() =

        let mod_state = ModState.cycle "mirror" Map.empty

        let result = ModState.cycle "random" mod_state
        Assert.True(result.ContainsKey "random")

        printfn "%A" result

        let result2 = ModState.cycle "shuffle" result
        Assert.False(result2.ContainsKey "random")
        Assert.True(result2.ContainsKey "shuffle")

        printfn "%A" result2

    [<Test>]
    let Check() =

        match ModState.check (Map.empty) with
        | Ok ModStatus.Ranked -> ()
        | unexpected -> Assert.Fail("Expected Ranked status", unexpected)

        match ModState.check (Map.ofList ["mirror", 0]) with
        | Ok ModStatus.Ranked -> ()
        | unexpected -> Assert.Fail("Expected Ranked status", unexpected)

        match ModState.check (Map.ofList ["nosv", 0]) with
        | Ok ModStatus.Unranked -> ()
        | unexpected -> Assert.Fail("Expected Unranked status", unexpected)

        match ModState.check (Map.ofList ["mirror", 1]) with
        | Error reason -> printfn "%s" reason
        | unexpected -> Assert.Fail("Expected Ranked status", unexpected)

        match ModState.check (Map.ofList ["random", 1; "shuffle", 1]) with
        | Error reason -> printfn "%s" reason
        | unexpected -> Assert.Fail("Expected Ranked status", unexpected)

        match ModState.check (Map.ofList ["mirror", 0; "nosv", 0; "DOES NOT EXIST", 0]) with
        | Error reason -> printfn "%s" reason
        | unexpected -> Assert.Fail("Expected Ranked status", unexpected)

        Assert.Pass()