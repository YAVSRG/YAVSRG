namespace Prelude.Tests.Imports

open NUnit.Framework
open Percyqaz.Common
open Prelude.Data.Library.Imports


module RateDetection =

    [<TestCase("[4K] Test [1.1x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test[1.1x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test [1.10x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test[01.10]", 1.1f<rate>)>]
    [<TestCase("[4K] Test [x1.10x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test (x2.5)", 2.5f<rate>)>]
    [<TestCase("[4K] Test (2.5x)", 2.5f<rate>)>]
    [<TestCase("[4K] Test 1.1", 1.1f<rate>)>]
    [<TestCase("[4K] Test1.1x", 1.1f<rate>)>]
    [<TestCase("[4K] Test [1.1x] [1.2x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test 0.8 0.9 1.0 1.1 1.2", 0.8f<rate>)>]
    [<TestCase("[4K] Test 0,8x 0.9x", 0.8f<rate>)>]
    [<TestCase("[4K] Test 3.0x", 3.0f<rate>)>]
    [<TestCase("[4K] Test 0.5x", 0.5f<rate>)>]
    [<TestCase("3.00x", 0.5f<rate>)>]
    [<TestCase("0.50x", 0.5f<rate>)>]
    let RateDetection_Regex_ExpectedValue (difficulty_name: string, expected_value: float32<rate>) =

        let result = detect_rate_mod difficulty_name
        match result with
        | Some value -> Assert.AreEqual(expected_value, value)
        | None -> Assert.Fail(sprintf "Expected %f to be detected rate" expected_value)
        
    [<TestCase("[4K] Test1.1")>]
    [<TestCase("[4K] Test [1.0x]")>]
    [<TestCase("[4K] Test [x1.0]")>]
    [<TestCase("[4K] Test 1.0")>]
    [<TestCase("[4K] Words1.1words")>]
    [<TestCase("[4K] 1.1words")>]
    [<TestCase("Words only")>]
    [<TestCase("3.01x")>]
    [<TestCase("3.10x")>]
    [<TestCase("x0.4")>]
    [<TestCase("0.49x")>]
    let RateDetection_Regex_ExpectNoRate (difficulty_name: string) =

        let result = detect_rate_mod difficulty_name
        match result with
        | Some value -> Assert.Fail(sprintf "%f" value)
        | None -> Assert.Pass()