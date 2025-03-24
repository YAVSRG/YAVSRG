namespace Prelude.Tests.Calculator

open NUnit.Framework
open Prelude
open Prelude.Calculator

module Color =

    [<Test>]
    let Color_Green_Red_Pink() =

        printfn "%O" (Difficulty.color 0.0f)
        printfn "%O" (Difficulty.color 7.5f)
        printfn "%O" (Difficulty.color 10.0f)
        printfn "%O" (Difficulty.color 15.0f)

        Assert.Pass()

    [<Test>]
    let Color_SafeOnStrangeValues() =

        Assert.AreEqual(Color.FromArgb 0xFF_00FF00, Difficulty.color nanf)
        Assert.AreEqual(Color.FromArgb 0xFF_FF00FF, Difficulty.color infinityf)
        Assert.AreEqual(Color.FromArgb 0xFF_00FF00, Difficulty.color -infinityf)