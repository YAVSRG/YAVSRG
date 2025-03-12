namespace Prelude.Tests.Charts

open NUnit.Framework
open Prelude
open Prelude.Charts

module Origins =

    [<Test>]
    let ChartOrigins_ExpectedOrder() =

        let origins_backwards =
            [
                ChartOrigin.Quaver { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; MapSetId = -1; MapId = 0 }
                ChartOrigin.Quaver { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; MapSetId = 555; MapId = 999 }
                ChartOrigin.Osu { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; SourceRate = 1.4f<rate>; BeatmapSetId = -1; BeatmapId = 0; SourceOD = 8.0f }
                ChartOrigin.Osu { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; SourceRate = 1.4f<rate>; BeatmapSetId = 555; BeatmapId = 999; SourceOD = 8.0f }
                ChartOrigin.Osu { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; SourceRate = 0.8f<rate>; BeatmapSetId = -1; BeatmapId = 0; SourceOD = 8.0f }
                ChartOrigin.Osu { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; SourceRate = 0.8f<rate>; BeatmapSetId = 555; BeatmapId = 999; SourceOD = 8.0f }
                ChartOrigin.Osu { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; SourceRate = 1.0f<rate>; BeatmapSetId = -1; BeatmapId = 0; SourceOD = 8.0f }
                ChartOrigin.Osu { Md5 = "md5"; FirstNoteOffset = 0.0f<ms>; SourceRate = 1.0f<rate>; BeatmapSetId = 555; BeatmapId = 999; SourceOD = 8.0f }
                ChartOrigin.Etterna "zzz"
                ChartOrigin.Etterna "AAA"
            ]

        Assert.AreEqual(Seq.sort origins_backwards, Seq.rev origins_backwards)

    [<Test>]
    let ChartOrigins_Set_ExpectedOrder() =

        let origins_a : ChartOrigin list = [ ChartOrigin.Etterna "AAA" ]
        let origins_b : ChartOrigin list = [ ]

        Assert.True(origins_b < origins_a)
        Assert.True(Set.ofList origins_b < Set.ofList origins_a)