namespace Interlude.Features

open Percyqaz.Common
open Prelude.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.Options
open Interlude.UI.Menu

type WaveformRender() =
    inherit StaticContainer(NodeType.None)
    
    let waveform = 
        try
            let audioPath = Interlude.Features.Gameplay.Chart.current.Value.AudioPath
            Percyqaz.Flux.Audio.Waveform.generate audioPath
        with err -> Logging.Error("Waveform error", err); { MsPerPoint = 1.0f<ms>; Points = Array.zeroCreate 0 }

    override this.Draw() =
        // SCALE IS 1 PIXEL PER POINT
        let scale = 1.0f</ms>
        let mutable x = this.Bounds.Left
        let mutable i = int <| Percyqaz.Flux.Audio.Song.time() / waveform.MsPerPoint - this.Bounds.Width * 0.75f / scale / waveform.MsPerPoint
        let y = this.Bounds.CenterY
        let h = this.Bounds.Height / 2.0f
        let points = waveform.Points
        while x < this.Bounds.Right && i < points.Length do
            if i >= 0 then
                Draw.rect (Rect.Create(x, this.Bounds.CenterY - h * points.[i].High, x + 1.0f, this.Bounds.CenterY + h * points.[i].High)) (Color.FromArgb(80, 160, 160, 255))
                Draw.rect (Rect.Create(x, this.Bounds.CenterY - h * points.[i].Mid, x + 1.0f, this.Bounds.CenterY + h * points.[i].Mid)) (Color.FromArgb(80, 190, 190, 255))
                Draw.rect (Rect.Create(x, this.Bounds.CenterY - h * points.[i].Low, x + 1.0f, this.Bounds.CenterY + h * points.[i].Low)) (Color.FromArgb(80, 130, 130, 255))
                Draw.rect (Rect.Create(x, this.Bounds.CenterY - h * points.[i].Left, x + 1.0f, this.Bounds.CenterY + h * points.[i].Right)) (Color.FromArgb(160, Color.White))
            i <- i + 1
            x <- x + waveform.MsPerPoint * scale
        Draw.rect (this.Bounds.TrimRight(this.Bounds.Width * 0.25f).SliceRight(5.0f)) Color.White

type OffsetPage() as this =
    inherit Page()

    do 
        this.Content(
            column()
            |+ PrettySetting("gameplay.pacemaker.saveunderpace", Selector<_>.FromBool options.SaveScoreIfUnderPace).Pos(200.0f)
            |+ WaveformRender(Position = Position.SliceBottom(200.0f))
        )

    override this.Title = N"gameplay.pacemaker"
    override this.OnClose() = ()