namespace Prelude.Tests.Stats

open NUnit.Framework
open Prelude.Data.User.Stats

module Stats =

    [<Test>]
    let MergeKeymodePlaytimes_CaseA () =
        let original = Map.ofSeq [ 4, 3600.0; 7, 1800.0 ]
        let incoming = Map.ofSeq [ 8, 900.0; 4, 400.0 ]

        let combined = add_playtimes original incoming
        Assert.AreEqual(3, combined.Count)
        Assert.AreEqual(Some 4000.0, combined.TryFind 4)
        Assert.AreEqual(Some 1800.0, combined.TryFind 7)
        Assert.AreEqual(Some 900.0, combined.TryFind 8)

    [<Test>]
    let MergeKeymodePlaytimes_CaseB () =
        let original = Map.ofSeq [ 4, 3600.0; 7, 1800.0 ]
        let incoming = Map.empty

        let combined = add_playtimes original incoming
        Assert.AreEqual(original, combined)

        let combined2 = add_playtimes incoming original
        Assert.AreEqual(original, combined2)