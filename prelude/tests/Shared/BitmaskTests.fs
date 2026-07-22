namespace Prelude.Tests.Shared

open NUnit.Framework
open Prelude

module BitmaskTests =

    [<Test>]
    let Expected_Results () =
        let bitmask = Bitmask.Empty.Add(2).Remove(1).Toggle(3).Add(4)

        Assert.True(bitmask.Contains(2))
        Assert.True(bitmask.Contains(3))
        Assert.True(bitmask.Contains(4))
        Assert.AreEqual(3, bitmask.Count)

    [<Test>]
    let Seq_RoundTrip () =
        let sequence = [ 0; 2; 3; 4; 7 ]

        Assert.AreEqual(sequence :> int seq, Bitmask.FromSeq(sequence).ToSeq())
