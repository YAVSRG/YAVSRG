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
                    SpriteBatch.Draw(s, left + w * (1 - ReceptorLight.Val), top + 3 * w * (1 - ReceptorLight.Val), right - w * (1 - ReceptorLight.Val), bottom, Color.White);
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
        int HITPOSITION = Game.Options.Profile.HitPosition;

        float end;

        readonly Color bgdim = Color.FromArgb(140, 140, 140);
        Sprite screencover;
        int lasti; int lastt;
        ChartWithModifiers Chart;
        ScoreTracker scoreTracker;
        Widget playfield;
        float missWindow;
        Key[] binds;
        HitLighting[] lighting;

        public ScreenPlay()
        {
            screencover = Content.LoadTextureFromAssets("screencover");

            Chart = Game.Gameplay.ModifiedChart;
            scoreTracker = new ScoreTracker(Game.Gameplay.ModifiedChart);

            //i make all this stuff ahead of time so i'm not creating a shitload of new objects/recalculating the same thing/sending stuff to garbage every 8ms
            lasti = Chart.Notes.Count;
            lastt = Chart.Timing.Count;
            missWindow = scoreTracker.Scoring.MissWindow * (float)Game.Options.Profile.Rate;

            end = Chart.Notes.Points[Chart.Notes.Count - 1].Offset;
            binds = Game.Options.Profile.Bindings[Chart.Keys];
            
            AddChild(playfield = new Playfield(scoreTracker).PositionTopLeft(-COLUMNWIDTH*Chart.Keys*0.5f,0,AnchorType.CENTER,AnchorType.MIN).PositionBottomRight(COLUMNWIDTH*Chart.Keys*0.5f,0,AnchorType.CENTER,AnchorType.MAX));
            AddChild(new HitMeter(scoreTracker).PositionTopLeft(-COLUMNWIDTH * Chart.Keys / 2, 0, AnchorType.CENTER, AnchorType.CENTER).PositionBottomRight(COLUMNWIDTH * Chart.Keys / 2, 0, AnchorType.CENTER, AnchorType.MAX));
            AddChild(new ProgressBar(scoreTracker).PositionTopLeft(-500, 10, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(500, 50, AnchorType.CENTER, AnchorType.MIN));
            AddChild(new ComboDisplay(scoreTracker).PositionTopLeft(0, -100, AnchorType.CENTER, AnchorType.CENTER));

            lighting = new HitLighting[Chart.Keys];
            float x = Chart.Keys * 0.5f;
            for (int i = 0; i < Chart.Keys; i++)
            {
                lighting[i] = new HitLighting();
                lighting[i].PositionTopLeft(COLUMNWIDTH * i, HITPOSITION + COLUMNWIDTH * 2, AnchorType.MIN, AnchorType.CENTER)
                    .PositionBottomRight(COLUMNWIDTH * (i + 1), HITPOSITION, AnchorType.MIN, AnchorType.CENTER);
                playfield.AddChild(lighting[i]);
            }
            //playfield.AddChild(new Screencover(scoreTracker, true)
                //.PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, Height * 2 * Game.Options.Profile.ScreenCoverUp, AnchorType.MAX, AnchorType.MIN));
            //playfield.AddChild(new Screencover(scoreTracker, false)
                //.PositionTopLeft(0, Height * 2 * Game.Options.Profile.ScreenCoverDown, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
        }

        public override void OnEnter(Screen prev)
        {
            if (prev is ScreenScore)
            {
                Game.Audio.Loop = true;
                Game.Screens.PopScreen(); return;
            }
            Utils.SetDiscordData("Playing", ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title + " [" + Game.CurrentChart.DifficultyName + "]");
            base.OnEnter(prev);
            Game.Options.Profile.Stats.TimesPlayed++;
            //Options.Colorizer.Colorize(Chart, Game.Options.Profile.ColorStyle);
            Game.Screens.Toolbar(false);
            Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
            Game.Audio.Loop = false;
            Game.Audio.Stop();
            Game.Audio.SetRate(Game.Options.Profile.Rate);
            Game.Audio.PlayLeadIn();
        }

        public override void OnExit(Screen next)
        {
            Utils.SetDiscordData("Looking for something to play", "");
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
            else if (Game.Audio.LeadingIn && Input.KeyTap(Key.Plus))
            {
                Game.Audio.Stop();
                Game.Screens.AddDialog(new Dialogs.TextDialog("Change sync by... (ms)", (x) => {
                    float f = 0; float.TryParse(x, out f); Game.Gameplay.ChartSaveData.Offset += f;
                    Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
                    Game.Audio.PlayLeadIn();
                }));
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
            //hitmeter.AddHit(k, scoreTracker.Scoring.MissWindow, (float)Game.Audio.Now(), 5);
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
            int i = Chart.Notes.GetNextIndex(now - missWindow);
            if (i >= lasti) { return; }
            int c = Chart.Notes.Count;
            float delta = missWindow;
            float d;
            int hitAt = -1;
            while (Chart.Notes.Points[i].Offset < now + missWindow) //search loop
            {
                Snap s = Chart.Notes.Points[i];
                BinarySwitcher b = release ? s.ends : new BinarySwitcher(s.taps.value + s.holds.value);
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
            }//put else statement here for cb on unecessary keypress
        }
        
        public override void Draw(float left, float top, float right, float bottom)
        {
            float offset = Chart.Keys * COLUMNWIDTH * -0.5f; //offset means actual horizontal offset of the playfield
            base.Draw(left, top, right, bottom);
           // DrawScreenCoverUp(offset, offset + COLUMNWIDTH * Chart.Keys, Game.Options.Profile.ScreenCoverUp); //draws the screencover
           // DrawScreenCoverDown(offset, offset + COLUMNWIDTH * Chart.Keys, Game.Options.Profile.ScreenCoverDown);
            
            SpriteBatch.DrawCentredText(Utils.RoundNumber(scoreTracker.Accuracy()), 40f, 0, -ScreenHeight + 70, Color.White); //acc

        }
        /*

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
        }*/
    }
}
