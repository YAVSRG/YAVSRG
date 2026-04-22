namespace Prelude.Tests.Imports

open NUnit.Framework
open Percyqaz.Common
open Prelude.Data.Library.Imports


module RateDetection =

    [<Test>]
    let RateDetection_BaseRate () =

        let result = detect_rate_mod "[4K] Test [1.0x]"
        let expected_value = None
        
        Assert.AreEqual(expected_value, result)

    let RateDetection_BaseRate_DoubleDecimal () =

        let result = detect_rate_mod "[4K] Test [1.00x]"
        let expected_value = None
        
        Assert.AreEqual(expected_value, result)
    
    [<Test>]
    let RateDetection_BaseRate_NoRate () =

        let result = detect_rate_mod "[4K] Test"
        let expected_value = None
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_SingleDecimalRate () =

        let result = detect_rate_mod "[4K] Test [1.1x]"
        let expected_value = Some 1.1f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_DoubleDecimalRate () =

        let result = detect_rate_mod "[4K] Test [0.95x]"
        let expected_value = Some 0.95f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_HighRate () =

        let result = detect_rate_mod "[4K] Test [2.2x]"
        let expected_value = Some 2.2f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_Parenthesis () =

        let result = detect_rate_mod "[4K] Test (1.05x)"
        let expected_value = Some 1.05f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_BpmOnly () =

        let result = detect_rate_mod "[4K] Test [235bpm]"
        let expected_value = None
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_BpmAfterRate () =

        let result = detect_rate_mod "[4K] Test [1.05x] [235bpm]"
        let expected_value = Some 1.05f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_BpmBeforeRate () =

        let result = detect_rate_mod "[4K] Test [235bpm] (1.05x)"
        let expected_value = Some 1.05f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_RateBeforeDiffName () =

        let result = detect_rate_mod "[4K] [1.05x] Test"
        let expected_value = Some 1.05f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_NoSpaces () =

        let result = detect_rate_mod "[4K]Test[1.05x]"
        let expected_value = Some 1.05f<rate>
        
        Assert.AreEqual(expected_value, result)

    [<Test>]
    let RateDetection_JunkDiffName () =

        let result = detect_rate_mod "sdsfsdf 1.05 sdfsdfsdf"
        let expected_value = Some 1.05f<rate>
        
        Assert.AreEqual(expected_value, result)