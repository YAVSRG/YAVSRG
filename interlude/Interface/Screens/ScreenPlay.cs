using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Beatmap;
using YAVSRG.Gameplay;
using YAVSRG.Interface.Widgets.Gameplay;
using OpenTK.Input;

namespace YAVSRG.Interface.Screens
{
    class ScreenPlay : Screen
    {
        class HitLighting : Widget
        {
            public AnimationSlider NoteLight = new AnimationSlider(0);
            public AnimationSlider ReceptorLight = new AnimationSlider(0);
            Sprite s = Content.LoadTextureFromAssets("receptorlighting");

            public HitLighting() : base()
            {

            }

            public override void Draw(float left, float top, float right, float bottom)
            {
                base.Draw(left, top, right, bottom);
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (ReceptorLight.Val > 0.5f)
                {
                    float w = (right - left);
                    SpriteBatch.Draw(s, left + w * (1 - ReceptorLight.Val), top - 3 * w * (1 - ReceptorLight.Val), right - w * (1 - ReceptorLight.Val), bottom, Color.White);
                }
            }

            public override void Update(float left, float top, float right, float bottom)
            {
                base.Update(left, top, right, bottom);
                    ReceptorLight.Update();
                    NoteLight.Update();
            }
        }

        int COLUMNWIDTH = Game.Options.Theme.ColumnWidth;
        float SCROLLSPEED = Game.Options.Profile.ScrollSpeed / (float)Game.Options.Profile.Rate;
        int HITPOSITION = Game.Options.Profile.HitPosition;

        float end;

        readonly Color bgdim = Color.FromArgb(140, 140, 140);
        Sprite note, hold, holdhead, receptor, playfield, screencover;

        int index = 0;
        int lasti; int lastt;
        Chart Chart;
        ScoreTracker scoreTracker;
        Widgets.HitMeter hitmeter;
        float missWindow;
        float[] holds;
        Snap.BinarySwitcher holdsInHitpos = new Snap.BinarySwitcher(0);
        Key[] binds;
        HitLighting[] lighting;

        public ScreenPlay()
        {
            note = Game.Options.Theme.GetNoteTexture(Game.CurrentChart.Keys);
            receptor = Game.Options.Theme.GetReceptorTexture(Game.CurrentChart.Keys);
            hold = Game.Options.Theme.GetBodyTexture(Game.CurrentChart.Keys);
            holdhead = Game.Options.Theme.GetHeadTexture(Game.CurrentChart.Keys);
            playfield = Content.LoadTextureFromAssets("playfield");
            screencover = Content.LoadTextureFromAssets("screencover");

            Chart = Game.CurrentChart;
            scoreTracker = new ScoreTracker(Game.CurrentChart);

            //i make all this stuff ahead of time so i'm not creating a shitload of new objects/recalculating the same thing/sending stuff to garbage every 8ms
            lasti = Chart.States.Count;
            lastt = Chart.Timing.Count;
            holds = new float[Chart.Keys];
            missWindow = scoreTracker.Scoring.MissWindow * (float)Game.Options.Profile.Rate;

            end = Chart.States.Points[Chart.States.Count - 1].Offset;
            binds = Game.Options.Profile.Bindings[Chart.Keys];
            hitmeter = new Widgets.HitMeter(Chart.Keys);

            AddChild(new Playfield(scoreTracker).PositionTopLeft(-COLUMNWIDTH*Chart.Keys*0.5f,0,AnchorType.CENTER,AnchorType.MIN).PositionBottomRight(COLUMNWIDTH*Chart.Keys*0.5f,0,AnchorType.CENTER,AnchorType.MAX));

            AddChild(hitmeter.PositionTopLeft(-COLUMNWIDTH * Chart.Keys / 2, 0, AnchorType.CENTER, AnchorType.CENTER).PositionBottomRight(COLUMNWIDTH * Chart.Keys / 2, 0, AnchorType.CENTER, AnchorType.MAX));

            AddChild(new Widgets.ProgressBar(scoreTracker).PositionTopLeft(-500, 10, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(500, 50, AnchorType.CENTER, AnchorType.MIN));

            scoreTracker.Scoring.OnMiss = (k) => { OnMiss(k); };

            lighting = new HitLighting[Chart.Keys];
            float x = Chart.Keys * 0.5f;
            for (int i = 0; i < Chart.Keys; i++)
            {
                lighting[i] = new HitLighting();
                lighting[i].PositionTopLeft(COLUMNWIDTH * (i - x), HITPOSITION + COLUMNWIDTH * 2, AnchorType.CENTER, AnchorType.MAX)
                    .PositionBottomRight(COLUMNWIDTH * (i - x + 1), HITPOSITION, AnchorType.CENTER, AnchorType.MAX);
                AddChild(lighting[i]);
            }
        }

        public override void OnEnter(Screen prev)
        {
            if (prev is ScreenScore)
            {
                Game.Screens.PopScreen(); return;
            }
            Utils.SetDiscordData("Playing", ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title + " [" + Chart.DifficultyName + "]");
            base.OnEnter(prev);
            Game.Options.Profile.Stats.TimesPlayed++;
            Options.Colorizer.Colorize(Chart, Game.Options.Profile.ColorStyle);
            Game.Screens.Toolbar(false);
            Game.Audio.Loop = false;
            Game.Audio.Stop();
            Game.Audio.SetRate(Game.Options.Profile.Rate);
            Game.Audio.PlayLeadIn();
        }

        public override void OnExit(Screen next)
        {
            Utils.SetDiscordData("Looking for something to play", "");
            Game.Screens.Toolbar(true);
            Game.Audio.Loop = true;
            base.OnExit(next);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            float now = (float)Game.Audio.Now();
            if (Input.KeyTap(Key.Escape))
            {
                Game.Screens.PopScreen();
                Widgets.Clear();
                Game.Options.Profile.Stats.TimesQuit++;
            }
            for (int k = 0; k < Chart.Keys; k++)
            {
                if (Input.KeyTap(binds[k]))
                {
                    OnKeyDown(k, now);
                }
                else if (Input.KeyRelease(binds[k]))
                {
                    OnKeyUp(k, now);
                }
            }
            scoreTracker.Update(now - missWindow);
            if (scoreTracker.EndOfChart())
            {
                Game.Screens.PopScreen();
                Game.Screens.AddScreen(new ScreenScore(scoreTracker));
                Widgets.Clear();
            }
        }

        public void OnMiss(int k)
        {
            hitmeter.AddHit(k, scoreTracker.Scoring.MissWindow, (float)Game.Audio.Now(), 5);
            //some other stuff
        }

        public void OnKeyDown(int k, float now)
        {
            HandleHit(k, now, false);
            lighting[k].ReceptorLight.Target = 1;
            lighting[k].ReceptorLight.Val = 1;
        }

        public void OnKeyUp(int k, float now)
        {
            HandleHit(k, now, true);
            lighting[k].ReceptorLight.Target = 0;
        }

        public void HandleHit(int k, float now, bool release)
        {
            //basically, this whole algorithm finds the closest snap to the receptors (above or below) that is relevant (has a note in the column you're pressing)
            int i = Chart.States.GetNextIndex(now - missWindow);
            if (i >= lasti) { return; }
            int c = Chart.States.Count;
            float delta = missWindow;
            float d;
            int hitAt = -1;
            while (Chart.States.Points[i].Offset < now + missWindow) //search loop
            {
                Snap s = Chart.States.Points[i];
                Snap.BinarySwitcher b = release ? s.ends : new Snap.BinarySwitcher(s.taps.value + s.holds.value);
                if (b.GetColumn(k))
                {
                    d = (now - s.Offset);
                    if (Math.Abs(d) < Math.Abs(delta))
                    {
                        delta = d;
                        hitAt = i;
                    }
                }
                i++;
                if (i == c) { break; }
            } //delta is HITWINDOW * 4 if nothing found
            if (release) delta *= 0.5f; //releasing long notes is more lenient (hit windows twice as big). needs a setting to turn on and off
            if (hitAt >= 0)
            {
                delta /= (float)Game.Options.Profile.Rate; //convert back to time relative to map instead of to player
                scoreTracker.RegisterHit(hitAt, k, delta);
                hitmeter.AddHit(k, delta, now, scoreTracker.Scoring.JudgeHit(Math.Abs(delta)));
            }//put else statement here for cb on unecessary keypress
        }
        
        public override void Draw(float left, float top, float right, float bottom)
        {
            float offset = Chart.Keys * COLUMNWIDTH * -0.5f; //offset means actual horizontal offset of the playfield
            base.Draw(left, top, right, bottom);
            DrawScreenCoverUp(offset, offset + COLUMNWIDTH * Chart.Keys, Game.Options.Profile.ScreenCoverUp); //draws the screencover
            DrawScreenCoverDown(offset, offset + COLUMNWIDTH * Chart.Keys, Game.Options.Profile.ScreenCoverDown);

            SpriteBatch.DrawCentredText(scoreTracker.Combo().ToString(), 40f, 0, -100, Color.White); //combo
            SpriteBatch.DrawCentredText(Utils.RoundNumber(scoreTracker.Accuracy()), 40f, 0, -Height + 70, Color.White); //acc

        }

        private void DrawScreenCoverUp(float left, float right, float amount)
        {
            if (amount <= 0.1) { return; }
            int h = Height * 2 - HITPOSITION;
            SpriteBatch.Draw(screencover, left, Height - HITPOSITION - h * amount - COLUMNWIDTH, right, Height - HITPOSITION - h * amount, Color.White, 0, 0, 0);
            SpriteBatch.Draw(screencover, left, Height - HITPOSITION - h * amount, right, Height - HITPOSITION, Color.White, 0, 1, 0);
        }

        private void DrawScreenCoverDown(float left, float right, float amount)
        {
            if (amount <= 0.1) { return; }
            int h = Height * 2 - HITPOSITION;
            SpriteBatch.Draw(screencover, left, h * amount - COLUMNWIDTH - Height, right, h * amount - Height, Color.White, 0, 0, 2);
            SpriteBatch.Draw(screencover, left, -Height, right,  h * amount - COLUMNWIDTH - Height, Color.White, 0, 1, 2);
        }
    }
}
