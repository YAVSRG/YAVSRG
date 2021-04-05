namespace Interlude.UI

open OpenTK
open System
open System.Drawing
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Gameplay.Score
open Prelude.Data.Themes
open Prelude.Data.ScoreManager
open Interlude
open Interlude.Input
open Interlude.UI.Animation
open Interlude.Render
open Interlude.Options

(*
    WIP, will be a fancy animation when beginning to play a chart
*)

type GameStartDialog() as this =
    inherit Dialog()

    let anim1 = new AnimationFade(0.0f)

    do
        this.Animation.Add(anim1)
        anim1.Target <- 1.0f
        this.Animation.Add(
            Animation.Serial(
                AnimationTimer 600.0,
                AnimationAction(fun () -> anim1.Target <- 0.0f),
                AnimationAction(this.Close)
            )
        )

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let w = right - left
        let bounds =
            let m = (top + bottom) * 0.5f
            Rect.create left (m - 100.0f) right (m + 100.0f)
        if anim1.Target = 1.0f then
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceRight(w * anim1.Value))(Screens.accentShade(255, 0.5f, 0.0f))(Sprite.Default)
            Draw.rect(bounds |> Rect.sliceLeft(w * anim1.Value))(Screens.accentShade(255, 1.0f, 0.0f))(Sprite.Default)
        else
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceLeft(w * anim1.Value))(Screens.accentShade(255, 0.5f, 0.0f))(Sprite.Default)
            Draw.rect(bounds |> Rect.sliceRight(w * anim1.Value))(Screens.accentShade(255, 1.0f, 0.0f))(Sprite.Default)

    override this.OnClose() = ()

//TODO LIST
//  SEEKING/REWINDING SUPPORT
//  COLUMN INDEPENDENT SV
//  MAYBE FIX HOLD TAIL CLIPPING

(*
    Note rendering code. Designed so it could be repurposed as an editor but that may now never happen (editor will be another project)
*)

type NoteRenderer() as this =
    inherit Widget()
    //scale, column width, note provider should be options
    
    //functions to get bounding boxes for things. used to place other gameplay widgets on the playfield.

    //constants
    let (keys, notes, bpm, sv, mods) = Gameplay.coloredChart.Force()
    let columnPositions = Array.init keys (fun i -> float32 i * Themes.noteskinConfig.ColumnWidth)
    let columnWidths = Array.create keys (float32 Themes.noteskinConfig.ColumnWidth)
    let noteHeight = Themes.noteskinConfig.ColumnWidth
    let scale = float32(Options.options.ScrollSpeed.Get()) / Gameplay.rate * 1.0f</ms>
    let hitposition = float32 <| Options.options.HitPosition.Get()
    let holdnoteTrim = Themes.noteskinConfig.ColumnWidth * Themes.noteskinConfig.HoldNoteTrim

    let tailsprite = Themes.getTexture(if Themes.noteskinConfig.UseHoldTailTexture then "holdtail" else "holdhead")
    let animation = new AnimationCounter(200.0)

    //arrays of stuff that are reused/changed every frame. the data from the previous frame is not used, but making new arrays causes garbage collection
    let mutable note_seek = 0 //see comments for sv_seek and sv_peek. same role but for index of next row
    let mutable note_peek = note_seek
    let sv = Array.init (keys + 1) (fun i -> sv.GetChannelData(i - 1).Data)
    let sv_seek = Array.create (keys + 1) 0 //index of next appearing SV point for each channel; = number of SV points if there are no more
    let sv_peek = Array.create (keys + 1) 0 //same as sv_seek, but sv_seek stores the relevant point for t = now (according to music) and this stores t > now
    let sv_value = Array.create (keys + 1) 1.0f //value of the most recent sv point per channel, equal to the sv value at index (sv_peek - 1), or 1 if that point doesn't exist
    let sv_time = Array.zeroCreate (keys + 1)
    let column_pos = Array.zeroCreate keys //running position calculation of notes for sv
    let hold_presence = Array.create keys false
    let hold_pos = Array.create keys 0.0f
    let hold_colors = Array.create keys 0

    let scrollDirectionPos bottom = if Options.options.Upscroll.Get() then id else fun (struct (l, t, r, b): Rect) -> struct (l, bottom - b, r, bottom - t)
    let scrollDirectionFlip = if (not Themes.noteskinConfig.FlipHoldTail) || Options.options.Upscroll.Get() then id else Quad.flip
    let noteRotation =
        if keys = 4 && Themes.noteskinConfig.UseRotation then
            fun k (struct (s, q): SpriteQuad) -> struct (s, Quad.rotate(match k with 0 -> 3 | 1 -> 0 | 2 -> 2 | 3 -> 1 | _ -> 0) q)
        else fun k -> id

    do
        let width = Array.mapi (fun i n -> n + columnWidths.[i]) columnPositions |> Array.max
        let (screenAlign, columnAlign) = Themes.noteskinConfig.PlayfieldAlignment
        this.Reposition(-width * columnAlign, screenAlign, 0.0f, 0.0f, width * (1.0f - columnAlign), screenAlign, 0.0f, 1.0f)
        this.Animation.Add(animation)

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let playfieldHeight = bottom - top
        let now = Audio.timeWithOffset()

        //seek to appropriate sv and note locations in data.
        //all of this stuff could be wrapped in an object handling seeking/peeking but would give slower performance because it's based on Seq and not ResizeArray
        //i therefore sadly had to make a mess here. see comments on the variables for more on whats going on
        while note_seek < notes.Data.Count && (offsetOf notes.Data.[note_seek]) < now do
            note_seek <- note_seek + 1
        note_peek <- note_seek
        for i in 0 .. keys do
            while sv_seek.[i] < sv.[i].Count && (offsetOf <| sv.[i].[sv_seek.[i]]) < now do
                sv_seek.[i] <- sv_seek.[i] + 1
            sv_peek.[i] <- sv_seek.[i]
            sv_value.[i] <- if sv_seek.[i] > 0 then snd sv.[i].[sv_seek.[i] - 1] else 1.0f

        for k in 0 .. (keys - 1) do
            Draw.rect(Rect.create (left + columnPositions.[k]) top (left + columnPositions.[k] + columnWidths.[k]) bottom) (Color.FromArgb(127, 0, 0, 0)) Sprite.Default
            sv_time.[k] <- now
            column_pos.[k] <- hitposition
            hold_pos.[k] <- hitposition
            hold_presence.[k] <-
                if note_seek > 0 then
                    let (_, struct (nd, c)) = notes.Data.[note_seek - 1] in
                    hold_colors.[k] <- int c.[k]
                    (testForNote k NoteType.HOLDHEAD nd || testForNote k NoteType.HOLDBODY nd)
                else false
            Draw.quad(Rect.create (left + columnPositions.[k]) hitposition (left + columnPositions.[k] + columnWidths.[k]) (hitposition + noteHeight) |> scrollDirectionPos bottom |> Quad.ofRect) (Color.White |> Quad.colorOf) (Sprite.gridUV(animation.Loops, 0)(Themes.getTexture("receptor")) |> noteRotation k) //animation for being pressed

        //main render loop - until the last note rendered in every column appears off screen
        let mutable min = hitposition
        while min < playfieldHeight && note_peek < notes.Data.Count do
            min <- playfieldHeight
            let (t, struct (nd, color)) = notes.Data.[note_peek]
            //until no sv adjustments needed...
            //update main sv
            while (sv_peek.[0] < sv.[0].Count && offsetOf sv.[0].[sv_peek.[0]] < t) do
                let (t2, v) = sv.[0].[sv_peek.[0]]
                for k in 0 .. (keys - 1) do
                    column_pos.[k] <- column_pos.[k] + scale * sv_value.[0] * (t2 - sv_time.[k])
                    sv_time.[k] <- t2
                sv_value.[0] <- v
                sv_peek.[0] <- sv_peek.[0] + 1
            //update column sv

            //render notes
            for k in 0 .. (keys - 1) do
                column_pos.[k] <- column_pos.[k] + scale * sv_value.[0] * (t - sv_time.[k])
                sv_time.[k] <- t
                min <- Math.Min(column_pos.[k], min)
                if testForNote k NoteType.NORMAL nd then
                    Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) column_pos.[k] (left + columnPositions.[k] + columnWidths.[k]) (column_pos.[k] + noteHeight) |> scrollDirectionPos bottom)) (Quad.colorOf Color.White) (Sprite.gridUV(animation.Loops, int color.[k])(Themes.getTexture("note")) |> noteRotation k)
                elif testForNote k NoteType.HOLDHEAD nd then
                    hold_pos.[k] <- column_pos.[k]
                    hold_colors.[k] <- int color.[k]
                    hold_presence.[k] <- true
                elif testForNote k NoteType.HOLDTAIL nd then
                    let headpos = hold_pos.[k]
                    let pos = column_pos.[k] - holdnoteTrim
                    if headpos < pos then
                        Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) (headpos + noteHeight * 0.5f) (left + columnPositions.[k] + columnWidths.[k]) (pos + noteHeight * 0.5f) |> scrollDirectionPos bottom)) (Quad.colorOf Color.White) (Sprite.gridUV(animation.Loops, hold_colors.[k])(Themes.getTexture("holdbody")))
                    if headpos - pos < noteHeight * 0.5f then
                        Draw.quad
                            (Quad.ofRect (Rect.create(left + columnPositions.[k]) (Math.Max(pos, headpos)) (left + columnPositions.[k] + columnWidths.[k]) (pos + noteHeight) |> scrollDirectionPos bottom))
                            (Quad.colorOf Color.White)
                            (Sprite.gridUV(animation.Loops, int color.[k]) tailsprite |> fun struct (s, q) -> struct (s, scrollDirectionFlip q))
                    Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) headpos (left + columnPositions.[k] + columnWidths.[k]) (headpos + noteHeight) |> scrollDirectionPos bottom)) (Quad.colorOf Color.White) (Sprite.gridUV(animation.Loops, hold_colors.[k])(Themes.getTexture("holdhead")) |> noteRotation k)
                    hold_presence.[k] <- false
                elif testForNote k NoteType.MINE nd then
                    Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) column_pos.[k] (left + columnPositions.[k] + columnWidths.[k]) (column_pos.[k] + noteHeight) |> scrollDirectionPos bottom)) (Quad.colorOf Color.White) (Sprite.gridUV(animation.Loops, int color.[k])(Themes.getTexture("mine")))
                    
            note_peek <- note_peek + 1
        
        for k in 0 .. (keys - 1) do
            if hold_presence.[k] then
                let headpos = hold_pos.[k]
                Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) (headpos + noteHeight * 0.5f) (left + columnPositions.[k] + columnWidths.[k]) bottom |> scrollDirectionPos bottom)) (Quad.colorOf Color.White) (Sprite.gridUV(animation.Loops, hold_colors.[k])(Themes.getTexture("holdbody")))
                Draw.quad (Quad.ofRect (Rect.create(left + columnPositions.[k]) headpos (left + columnPositions.[k] + columnWidths.[k]) (headpos + noteHeight) |> scrollDirectionPos bottom)) (Quad.colorOf Color.White) (Sprite.gridUV(animation.Loops, hold_colors.[k])(Themes.getTexture("holdhead")))
        base.Draw()

(*
    Handful of widgets that directly pertain to gameplay
    They can all be toggled/repositioned/configured using themes
*)

module GameplayWidgets = 
    type HitEvent = (struct(int * Time * Time))
    type Helper = {
        Scoring: AccuracySystem
        HP: HPSystem
        OnHit: IEvent<HitEvent>
    }

    type AccuracyMeter(conf: WidgetConfig.AccuracyMeter, helper) as this =
        inherit Widget()

        let color = new AnimationColorMixer(if conf.GradeColors then Themes.themeConfig.GradeColors.[0] else Color.White)
        let listener =
            if conf.GradeColors then
                helper.OnHit.Subscribe(fun _ -> color.SetColor(Themes.themeConfig.GradeColors.[grade helper.Scoring.Value Themes.themeConfig.GradeThresholds]))
            else null

        do
            this.Animation.Add(color)
            this.Add(new Components.TextBox(helper.Scoring.Format, (fun () -> color.GetColor()), 0.5f) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.7f))
            if conf.ShowName then
                this.Add(new Components.TextBox(Utils.K helper.Scoring.Name, (Utils.K Color.White), 0.5f) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f))
        
        override this.Dispose() =
            if isNull listener then () else listener.Dispose()

    type HitMeter(conf: WidgetConfig.HitMeter, helper) =
        inherit Widget()
        let hits = ResizeArray<struct (Time * float32 * int)>()
        let mutable w = 0.0f
        let listener =
            helper.OnHit.Subscribe(
                fun struct (_, delta, now) -> hits.Add(struct (now, delta/MISSWINDOW * w * 0.5f, helper.Scoring.JudgeFunc(Time.Abs delta) |> int)))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if w = 0.0f then w <- Rect.width this.Bounds
            let now = Audio.timeWithOffset()
            while hits.Count > 0 && let struct (time, _, _) = (hits.[0]) in time + conf.AnimationTime * 1.0f<ms> < now do
                hits.RemoveAt(0)

        override this.Draw() =
            base.Draw()
            let struct (left, top, right, bottom) = this.Bounds
            let centre = (right + left) * 0.5f
            if conf.ShowGuide then
                Draw.rect
                    (Rect.create (centre - conf.Thickness) top (centre + conf.Thickness) bottom)
                    Color.White
                    Sprite.Default
            let now = Audio.timeWithOffset()
            for struct (time, pos, j) in hits do
                Draw.rect
                    (Rect.create (centre + pos - conf.Thickness) top (centre + pos + conf.Thickness) bottom)
                    (let c = Themes.themeConfig.JudgementColors.[j] in
                        Color.FromArgb(Math.Clamp(255 - int (255.0f * (now - time) / conf.AnimationTime), 0, 255), int c.R, int c.G, int c.B))
                    Sprite.Default

        override this.Dispose() =
            listener.Dispose()

    type JudgementMeter(conf: WidgetConfig.JudgementMeter, helper) =
        inherit Widget()
        let atime = conf.AnimationTime * 1.0f<ms>
        let mutable tier = 0
        let mutable late = 0
        let mutable time = -atime * 2.0f - Audio.LEADIN_TIME
        let texture = Themes.getTexture("judgements")
        let listener =
            helper.OnHit.Subscribe(
                fun struct (_, delta, now) ->
                    let judge = helper.Scoring.JudgeFunc(Time.Abs delta)
                    if
                        match judge with
                        | JudgementType.RIDICULOUS
                        | JudgementType.MARVELLOUS -> conf.ShowRDMA
                        | JudgementType.OK
                        | JudgementType.NG -> conf.ShowOKNG
                        | _ -> true
                    then
                        let j = int judge in
                        if j >= tier || now - atime > time then
                            tier <- j
                            time <- now
                            late <- if delta > 0.0f<ms> then 1 else 0)
        override this.Draw() =
            let a = 255 - Math.Clamp(255.0f * (Audio.timeWithOffset() - time) / atime |> int, 0, 255)
            Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf (Color.FromArgb(a, Color.White))) (Sprite.gridUV(late, tier) texture)

        override this.Dispose() =
            listener.Dispose()

    type ComboMeter(conf: WidgetConfig.Combo, helper) as this =
        inherit Widget()
        let popAnimation = new AnimationFade(0.0f)
        let color = new AnimationColorMixer(Color.White)
        let mutable hits = 0
        let listener =
            helper.OnHit.Subscribe(
                fun _ ->
                    hits <- hits + 1
                    if (conf.LampColors && hits > 50) then
                        color.SetColor(Themes.themeConfig.LampColors.[helper.Scoring.State |> lamp |> int])
                    popAnimation.Value <- conf.Pop)

        do
            this.Animation.Add(color)
            this.Animation.Add(popAnimation)

        override this.Draw() =
            base.Draw()
            let (_, _, _, combo, _, _) = helper.Scoring.State
            let amt = popAnimation.Value + (((combo, 1000) |> Math.Min |> float32) * conf.Growth)
            Text.drawFill(Themes.font(), combo.ToString(), Rect.expand(amt, amt)this.Bounds, color.GetColor(), 0.5f)

        override this.Dispose() =
            listener.Dispose()

    type SkipButton(conf: WidgetConfig.SkipButton, helper) as this =
        inherit Widget()
        let firstNote = 
            let (_, notes, _, _, _) = Gameplay.coloredChart.Force()
            if notes.IsEmpty() then 0.0f<ms> else notes.First() |> offsetOf
        do
            this.Add(Components.TextBox(sprintf "Press %O to skip" (options.Hotkeys.Skip.Get()) |> Utils.K, Utils.K Color.White, 0.5f))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if Audio.time() + Audio.LEADIN_TIME * 2.5f < firstNote then
                if options.Hotkeys.Skip.Get().Tapped() then
                    Audio.playFrom(firstNote - Audio.LEADIN_TIME)
            else this.Destroy()

    (*
        These widgets are not repositioned by theme
    *)

    type ColumnLighting(keys, binds: Bind array, lightTime, helper) as this =
        inherit Widget()
        let sliders = Array.init keys (fun _ -> new AnimationFade(0.0f))
        let sprite = Themes.getTexture("receptorlighting")
        let lightTime = Math.Min(0.99f, lightTime)

        do
            Array.iter this.Animation.Add sliders
            let hp = float32 <| Options.options.HitPosition.Get()
            this.Reposition(0.0f, hp, 0.0f, -hp)

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            Array.iteri (fun k (s: AnimationFade) -> if binds.[k].Pressed() then s.Value <- 1.0f) sliders

        override this.Draw() =
            base.Draw()
            let struct (l, t, r, b) = this.Bounds
            let cw = (r - l) / (float32 keys)
            let threshold = 1.0f - lightTime
            let f k (s: AnimationFade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / lightTime
                    let a = 255.0f * p |> int
                    Draw.rect
                        (
                            if Options.options.Upscroll.Get() then
                                Sprite.alignedBoxX(l + cw * (float32 k + 0.5f), t, 0.5f, 1.0f, cw * p, -1.0f / p) sprite
                            else
                                Sprite.alignedBoxX(l + cw * (float32 k + 0.5f), b, 0.5f, 1.0f, cw * p, 1.0f / p) sprite
                        )
                        (Color.FromArgb(a, Color.White))
                        sprite
            Array.iteri f sliders

open GameplayWidgets

(*
    Play screen. Mostly input handling code + handling gameplay widget config
*)

type ScreenPlay() as this =
    inherit Screen()
    
    let (keys, notes, bpm, sv, mods) = Gameplay.coloredChart.Force()
    let scoreData = Gameplay.createScoreData()
    let scoring = createAccuracyMetric(options.AccSystems.Get() |> fst)
    let hp = createHPMetric (options.HPSystems.Get() |> fst) scoring
    let onHit = new Event<HitEvent>()
    let widgetHelper: Helper = { Scoring = scoring; HP = hp; OnHit = onHit.Publish }
    let binds = Options.options.GameplayBinds.[keys - 3]
    let missWindow = MISSWINDOW * Gameplay.rate

    let mutable noteSeek = 0

    do
        let noteRenderer = new NoteRenderer()
        this.Add(noteRenderer)
        let inline f name (constructor: 'T -> Widget) = 
            let config: ^T = Themes.getGameplayConfig(name)
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) (config))
            if pos.Enabled then
                config
                |> constructor
                |> Components.positionWidget(pos.Left, pos.LeftA, pos.Top, pos.TopA, pos.Right, pos.RightA, pos.Bottom, pos.BottomA)
                |> if pos.Float then this.Add else noteRenderer.Add
        f "accuracyMeter" (fun c -> new AccuracyMeter(c, widgetHelper) :> Widget)
        f "hitMeter" (fun c -> new HitMeter(c, widgetHelper) :> Widget)
        f "combo" (fun c -> new ComboMeter(c, widgetHelper) :> Widget)
        f "skipButton" (fun c -> new SkipButton(c, widgetHelper) :> Widget)
        f "judgementMeter" (fun c -> new JudgementMeter(c, widgetHelper) :> Widget)
        //todo: rest of widgets
        if Themes.noteskinConfig.ColumnLightTime >= 0.0f then
            noteRenderer.Add(new ColumnLighting(keys, binds, Themes.noteskinConfig.ColumnLightTime, widgetHelper))

    override this.OnEnter(prev) =
        Screens.backgroundDim.Target <- Options.options.BackgroundDim.Get() |> float32
        //discord presence
        Screens.setToolbarCollapsed(true)
        Screens.setCursorVisible(false)
        Audio.changeRate(Gameplay.rate)
        Audio.playLeadIn()
        //Screens.addDialog(new GameStartDialog())

    override this.OnExit(next) =
        Screens.backgroundDim.Target <- 0.7f
        Screens.setCursorVisible(true)
        if (next :? ScreenScore) then () else
            Screens.setToolbarCollapsed(false)

    member private this.Hit(i, k, delta, bad, now) =
        let _, deltas, status = scoreData.[i]
        match status.[k] with
        | HitStatus.Hit
        | HitStatus.SpecialNG
        | HitStatus.SpecialOK -> ()
        | HitStatus.NotHit ->
            deltas.[k] <- delta
            status.[k] <- HitStatus.Hit
            scoring.HandleHit(k)(i)(scoreData)
            hp.HandleHit(k)(i)(scoreData)
            onHit.Trigger(struct(k, delta, now))
        | HitStatus.Special ->
            deltas.[k] <- delta
            status.[k] <- if bad then HitStatus.SpecialNG else HitStatus.SpecialOK
            scoring.HandleHit(k)(i)(scoreData)
            hp.HandleHit(k)(i)(scoreData)
        | HitStatus.Nothing
        | _ -> failwith "impossible"

    member private this.HandleHit(k, now, release) =
        let i, _ = notes.IndexAt(now - missWindow) //maybe optimise this with another seeker?
        let mutable i = i + 1 //next index
        let mutable delta = missWindow
        let mutable hitAt = -1
        let mutable noteType = enum -1
        while i < notes.Count && offsetOf notes.Data.[i] < now + missWindow do
            let (time, struct (nd, _)) = notes.Data.[i]
            let (_, deltas, status) = scoreData.[i]
            if (status.[k] = HitStatus.NotHit || status.[k] = HitStatus.Special || deltas.[k] < -missWindow * 0.5f) then
                let d = now - time
                if release then
                    if (testForNote k NoteType.HOLDTAIL nd) then
                        if noteType = NoteType.HOLDBODY || Time.Abs(delta) > Time.Abs(d)  then
                            delta <- d
                            hitAt <- i
                            noteType <- NoteType.HOLDTAIL
                    else if noteType <> NoteType.HOLDTAIL&& (testForNote k NoteType.HOLDBODY nd) then
                        if (Time.Abs(delta) > Time.Abs(d)) then
                            delta <- d
                            hitAt <- i
                            noteType <- NoteType.HOLDBODY
                    else if testForNote k NoteType.HOLDHEAD nd then
                        i <- notes.Count //ln fix
                else 
                    if (testForNote k NoteType.HOLDHEAD nd) || (testForNote k NoteType.NORMAL nd) then
                        if (Time.Abs(delta) > Time.Abs(d)) || noteType = NoteType.MINE  then
                            delta <- d
                            hitAt <- i
                            noteType <- NoteType.NORMAL
                    else if noteType <> NoteType.NORMAL && (testForNote k NoteType.MINE nd) then
                        if (Time.Abs(delta) > Time.Abs(d)) then
                            delta <- d
                            hitAt <- i
                            noteType <- NoteType.MINE
            i <- i + 1
        if hitAt >= 0 && (not release || noteType <> NoteType.HOLDHEAD) then
            delta <- delta / Gameplay.rate * (if release then 0.666f else 1.0f)
            this.Hit(hitAt, k, delta, noteType = NoteType.MINE || noteType = NoteType.HOLDBODY, now)


    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Audio.timeWithOffset()
        if now > -missWindow then
            for k in 0 .. (keys - 1) do
                //unable to reason about correctness. possible todo: merge these into one so that all actions are ordered by time
                Input.consumeGameplay(binds.[k], InputEvType.Press, fun t -> this.HandleHit(k, t, false))
                Input.consumeGameplay(binds.[k], InputEvType.Release, fun t -> this.HandleHit(k, t, true))
        //seek up to miss threshold and display missed notes in widgets
        while noteSeek < notes.Count && offsetOf notes.Data.[noteSeek] < now - missWindow do
            let (_, _, s) = scoreData.[noteSeek]
            Array.iteri (fun i state -> if state = HitStatus.NotHit then onHit.Trigger(struct (i, MISSWINDOW, now))) s
            noteSeek <- noteSeek + 1
        //update release mask - if there is a LN head or tail within +- 180ms then not holding the key during a HOLDBODY should be ignored
        let mutable i = noteSeek
        let mutable releaseMask = 0us
        while i < notes.Count && offsetOf notes.Data.[i] < now + missWindow do
            let (_, struct (nd, _)) = notes.Data.[i]
            releaseMask <- releaseMask ||| noteData NoteType.HOLDTAIL nd ||| noteData NoteType.HOLDHEAD nd
            i <- i + 1
        releaseMask <- ~~~releaseMask
        //detect holding through a mine or releasing through a holdbody
        //todo: have a mask for mines as well
        let mutable i = noteSeek
        while i < notes.Count && offsetOf notes.Data.[i] < now + missWindow * 0.125f do
            let (t, struct (nd, _)) = notes.Data.[i]
            if (t > now - missWindow * 0.125f) then
                let (_, _, s) = scoreData.[i]
                for k in noteData NoteType.HOLDBODY nd &&& releaseMask |> getBits do
                    if s.[k] = HitStatus.Special && not (binds.[k].Pressed()) then s.[k] <- HitStatus.SpecialNG
                for k in noteData NoteType.MINE nd |> getBits do
                    if s.[k] = HitStatus.Special && binds.[k].Pressed() then s.[k] <- HitStatus.SpecialNG
            i <- i + 1
        //todo: handle in all watchers
        scoring.Update(now - missWindow)(scoreData)(true)
        hp.Update(now - missWindow)(scoreData)(true)
        if noteSeek = notes.Count then
            noteSeek <- noteSeek + 1 //hack to prevent running this code twice
            ((fun () ->
                let sd =
                    ScoreInfoProvider(
                        Gameplay.makeScore(scoreData, keys),
                        Gameplay.currentChart.Value,
                        options.AccSystems.Get() |> fst,
                        options.HPSystems.Get() |> fst,
                        ModChart = Gameplay.modifiedChart.Force(),
                        Difficulty = Gameplay.difficultyRating.Value)
                (sd, Gameplay.setScore(sd))
                |> ScreenScore
                :> Screen), ScreenType.Score, ScreenTransitionFlag.Default)
            |> Screens.newScreen
