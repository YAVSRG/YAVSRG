using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Beatmap;
using YAVSRG.Gameplay;
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
        PlayingChart scoreTracker;
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
            scoreTracker = new PlayingChart(Game.CurrentChart);

            //i make all this stuff ahead of time so i'm not creating a shitload of new objects/recalculating the same thing/sending stuff to garbage every 8ms
            lasti = Chart.States.Count;
            lastt = Chart.Timing.Count;
            holds = new float[Chart.Keys];
            missWindow = scoreTracker.Scoring.MissWindow * (float)Game.Options.Profile.Rate;

            end = Chart.States.Points[Chart.States.Count - 1].Offset;
            binds = Game.Options.Profile.Bindings[Chart.Keys];
            hitmeter = new Widgets.HitMeter(Chart.Keys);

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
            base.OnEnter(prev);
            Game.Options.Profile.Stats.TimesPlayed++;
            Options.Colorizer.Colorize(Chart, Game.Options.Profile.ColorStyle);
            Game.Screens.Toolbar(false);
            Game.Audio.Stop();
            Game.Audio.SetRate(Game.Options.Profile.Rate);
            Game.Audio.PlayLeadIn();
        }

        public override void OnExit(Screen next)
        {
            Game.Screens.Toolbar(true);
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

        //This is the core rhythm game engine bit
        public override void Draw(float left, float top, float right, float bottom)
        {
            float offset = Chart.Keys * COLUMNWIDTH * -0.5f; //offset means actual horizontal offset of the playfield
            //0 offset = playfield left edge is in centre of screen

            for (int c = 0; c < Chart.Keys; c++) //draw columns and empty receptors
            {
                DrawColumn(offset, c);
                DrawReceptor(offset, c);
            }

            float now = (float)Game.Audio.Now(); //where are we in the song
            while (index < lasti && Chart.States.Points[index].Offset < now) //"catch up" algorithm to find position in chart data
            {
                index++;
            }
            int i = index; //we need a copy of this number so we can increase it without messing the thing up next frame
            int t = Chart.Timing.GetLastIndex(now); //no catch up algorithm used for SV because there are less SVs and this is optimised pretty neatly
            float y = HITPOSITION; //keeps track of where we're drawing vertically on the screen
            float v = 0; //needs a better name

            holdsInHitpos.value = 0; //tracker of hold notes that need to be shown in the hit position
            for (int k = 0; k < Chart.Keys; k++) //more tracker data for drawing long notes
            {
                holds[k] = 0;//used in DrawSnapWithHolds. it's only initialised once to reduce garbage collection
            }

            while (y + v < Height * 2 && i < lasti)//continue drawing until we reach the end of the map or the top of the screen (don't need to draw notes beyond it)
            {
                while (!Game.Options.Profile.FixedScroll && t < lastt - 1 && Chart.Timing.Points[t + 1].Offset < Chart.States.Points[i].Offset) //check if we've gone past any timing points
                {
                    y += SCROLLSPEED * Chart.Timing.Points[t].ScrollSpeed * (Chart.Timing.Points[t + 1].Offset - now); //handle scrollspeed adjustments
                    //SpriteBatch.DrawRect(offset, Height - y, -offset, Height - y + 5, Color.White); //bar line
                    t++;//tracks which timing point we're looking at
                    now = Chart.Timing.Points[t].Offset; //we're now drawing relative to the most recent timing point
                }
                v = (Game.Options.Profile.FixedScroll ? 1 : Chart.Timing.Points[t].ScrollSpeed) * (Chart.States.Points[i].Offset - now) * SCROLLSPEED; //draw distance between "now" and the row of notes
                DrawSnapWithHolds(Chart.States.Points[i], offset, y + v);//draw whole row of notes
                i++;//move on to next row of notes
            }
            if (holdsInHitpos.value > 0)//this has been updated by DrawSnapWithHolds
            {
                DrawSnap(new Snap(0, 0, holdsInHitpos.value, 0, 0), offset, HITPOSITION); //draw hold heads in hit position
            }

            DrawScreenCoverUp(offset, offset + COLUMNWIDTH * Chart.Keys, Game.Options.Profile.ScreenCoverUp); //draws the screencover
            DrawScreenCoverDown(offset, offset + COLUMNWIDTH * Chart.Keys, Game.Options.Profile.ScreenCoverDown);

            SpriteBatch.DrawCentredText(scoreTracker.Combo().ToString(), 40f, 0, -100, Color.White); //combo
            SpriteBatch.DrawCentredText(Utils.RoundNumber(scoreTracker.Accuracy()), 40f, 0, -Height + 70, Color.White); //acc

            base.Draw(left, top, right, bottom);
        }

        private void DrawLongTap(float offset, int i, float start, float end)
        {
            if (start == 0)
            {
                start = HITPOSITION;
            }
            SpriteBatch.Draw(hold, i * COLUMNWIDTH + offset, Height - start - COLUMNWIDTH * 0.5f, (i + 1) * COLUMNWIDTH + offset, Height - end - COLUMNWIDTH * 0.5f, Color.White);
        }

        private void DrawColumn(float offset, int i)
        {
            SpriteBatch.Draw(playfield, i * COLUMNWIDTH + offset, -Height, (i + 1) * COLUMNWIDTH + offset, Height, Color.White);
        }

        private void DrawReceptor(float offset, int k)
        {
            Game.Options.Theme.DrawReceptor(receptor, k * COLUMNWIDTH + offset, Height - COLUMNWIDTH - HITPOSITION, (k + 1) * COLUMNWIDTH + offset, Height - HITPOSITION, k, Game.CurrentChart.Keys, Input.KeyPress(binds[k]));
        }

        private void DrawSnapWithHolds(Snap s, float offset, float y)
        {
            foreach (int k in s.middles.GetColumns())
            {
                DrawLongTap(offset, k, holds[k], y);
                if (holds[k] == 0)
                {
                    holdsInHitpos.SetColumn(k);
                }
                holds[k] = y;
            }
            foreach (int k in s.ends.GetColumns())
            {
                DrawLongTap(offset, k, holds[k], y);
                if (holds[k] == 0)
                {
                    holdsInHitpos.SetColumn(k);
                }
                holds[k] = Height * 2;
            }
            foreach (int k in s.holds.GetColumns())
            {
                holds[k] = y;
            }
            DrawSnap(s, offset, y);
        }

        private void DrawSnap(Snap s, float offset, float pos)
        {
            pos = Height - pos;
            foreach (int k in s.taps.GetColumns())
            {
                Game.Options.Theme.DrawNote(note, k * COLUMNWIDTH + offset, pos - COLUMNWIDTH, (k + 1) * COLUMNWIDTH + offset, pos, k, Game.CurrentChart.Keys, s.colors[k], 2);
            }
            foreach (int k in s.ends.GetColumns())
            {
                Game.Options.Theme.DrawHead(holdhead, k * COLUMNWIDTH + offset, pos - COLUMNWIDTH, (k + 1) * COLUMNWIDTH + offset, pos, k, Game.CurrentChart.Keys);
            }
            foreach (int k in s.holds.GetColumns())
            {
                Game.Options.Theme.DrawTail(holdhead, k * COLUMNWIDTH + offset, pos - COLUMNWIDTH, (k + 1) * COLUMNWIDTH + offset, pos, k, Game.CurrentChart.Keys);
            }
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
