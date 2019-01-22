using System;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Gameplay.Charts.YAVSRG;
using YAVSRG.Gameplay;
using YAVSRG.Interface.Widgets.Gameplay;
using YAVSRG.Interface.Animations;
using YAVSRG.IO;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Screens
{
    class ScreenPlay : Screen //i cleaned up this file a little but it's a bit of a mess. sorry! update: a lot of a mess
    {
        ChartWithModifiers Chart;
        ScoreTracker scoreTracker;
        Widget playfield;
        float missWindow;
        OpenTK.Input.Key[] binds;
        HitLighting[] lighting;
        AnimationFade bannerIn, bannerOut;

        public ScreenPlay()
        {
            Chart = Game.Gameplay.ModifiedChart;
            scoreTracker = new ScoreTracker(Game.Gameplay.ModifiedChart);

            int columnwidth = Game.Options.Theme.ColumnWidth;
            int hitposition = Game.Options.Profile.HitPosition;

            missWindow = scoreTracker.Scoring.MissWindow * (float)Game.Options.Profile.Rate;
            binds = Game.Options.Profile.KeymodeBindings[Chart.Keys - 3];

            var widgetData = Game.Options.Theme.Gameplay;

            //this stuff is ok to stay here
            AddChild(playfield = new Playfield(scoreTracker).PositionTopLeft(-columnwidth * Chart.Keys * 0.5f, 0, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(columnwidth * Chart.Keys * 0.5f, 0, AnchorType.CENTER, AnchorType.MAX));
            AddChild(new PerformanceMeter(scoreTracker));
            AddChild(new HitMeter(scoreTracker, widgetData.GetPosition("hitMeter")));
            AddChild(new ComboDisplay(scoreTracker, widgetData.GetPosition("combo")));
            AddChild(new ProgressBar(scoreTracker, widgetData.GetPosition("progressBar")).PositionTopLeft(-500, 10, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(500, 50, AnchorType.CENTER, AnchorType.MIN));
            AddChild(new AccMeter(scoreTracker, widgetData.GetPosition("accuracy")));
            AddChild(new HPMeter(scoreTracker, widgetData.GetPosition("healthBar")));
            AddChild(new MiscInfoDisplay(scoreTracker, widgetData.GetPosition("time"), () => { return DateTime.Now.ToLongTimeString(); }));
            AddChild(new MiscInfoDisplay(scoreTracker, widgetData.GetPosition("timeLeft"), () => { return Utils.FormatTime((Chart.Notes.Points[Chart.Notes.Points.Count - 1].Offset - (float)Game.Audio.Now()) / (float)Game.Options.Profile.Rate) + " left"; }));
            AddChild(new MiscInfoDisplay(scoreTracker, widgetData.GetPosition("fps"), () => { return ((int)Game.Instance.FPS).ToString() + "fps"; }));
            AddChild(new JudgementCounter(scoreTracker, widgetData.GetPosition("judgements")));
            //all this stuff needs to be moved to Playfield under a method that adds gameplay elements (not used when in editor)
            //playfield.InitGameplay();
            lighting = new HitLighting[Chart.Keys];
            float x = Chart.Keys * 0.5f;
            //this places a hitlight on every column
            for (int i = 0; i < Chart.Keys; i++)
            {
                lighting[i] = new HitLighting();
                lighting[i].PositionTopLeft(columnwidth * i, hitposition, AnchorType.MIN, AnchorType.CENTER)
                    .PositionBottomRight(columnwidth * (i + 1), hitposition + columnwidth, AnchorType.MIN, AnchorType.CENTER);
                playfield.AddChild(lighting[i]);
            }
            //this places the screencovers
            if (Game.Options.Profile.ScreenCoverUp > 0)
                playfield.AddChild(new Screencover(scoreTracker, false)
                    .PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.CENTER).PositionBottomRight(0, ScreenHeight * 2 * Game.Options.Profile.ScreenCoverUp, AnchorType.MAX, AnchorType.CENTER));
            if (Game.Options.Profile.ScreenCoverDown > 0)
                playfield.AddChild(new Screencover(scoreTracker, true)
                .PositionTopLeft(0, ScreenHeight * 2 * (1 - Game.Options.Profile.ScreenCoverDown), AnchorType.MIN, AnchorType.CENTER).PositionBottomRight(0, ScreenHeight * 2, AnchorType.MAX, AnchorType.CENTER));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            //if returning from score screen, go straight to menu
            if (prev is ScreenScore)
            {
                Game.Screens.PopScreen(); return;
            }
            //some misc stuff
            Game.Screens.BackgroundDim.Target = 1 - Game.Options.Profile.BackgroundDim;
            IO.Discord.SetPresence("Playing a chart", Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title + " [" + Game.CurrentChart.Data.DiffName + "]\nFrom " + Game.CurrentChart.Data.SourcePack, false); ;
            Game.Options.Profile.Stats.TimesPlayed++;
            Game.Screens.Toolbar.SetState(WidgetState.DISABLED);
            Game.Screens.Toolbar.SetCursorState(false);
            AnimationSeries s = new AnimationSeries(false);
            s.Add(bannerIn = new AnimationFade(0, 2.001f, 2f));
            s.Add(new AnimationCounter(60, false));
            s.Add(new AnimationAction(() => { scoreTracker.WidgetColor.Target = 1; }));
            s.Add(bannerOut = new AnimationFade(0, 255, 254));
            Animation.Add(s);

            //prep audio and play from beginning
            Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
            Game.Audio.OnPlaybackFinish = null;
            Game.Audio.Stop();
            Game.Audio.SetRate(Game.Options.Profile.Rate);
            Game.Audio.PlayLeadIn();
        }

        public override void OnExit(Screen next)
        {
            //some misc stuff
            Game.Screens.BackgroundDim.Target = 0.3f;
            IO.Discord.SetPresence("Selecting another chart", Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title + " [" + Game.CurrentChart.Data.DiffName + "]\nFrom " + Game.CurrentChart.Data.SourcePack, true);
            Game.Screens.Toolbar.SetState(WidgetState.ACTIVE);
            Game.Screens.Toolbar.SetCursorState(true);
            base.OnExit(next);
        }

        public override void Update(Rect bounds) //update loop
        {
            float now = (float)Game.Audio.Now(); //get where we are now
            if (Input.KeyTap(Game.Options.General.Binds.Exit)) //escape quits
            {
                Game.Screens.PopScreen();
                Children.Clear();
                Game.Options.Profile.Stats.TimesQuit++;
            }
            else if (Game.Audio.LeadingIn && Input.KeyTap(Game.Options.General.Binds.ChangeOffset)) //if map hasn't started you can sync it with + key
            {
                Game.Audio.Stop();
                Game.Screens.AddDialog(new Dialogs.TextDialog("Change sync by... (ms)", (x) =>
                {
                    float f = 0; float.TryParse(x, out f); Game.Gameplay.ChartSaveData.Offset += f;
                    Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
                    Game.Audio.PlayLeadIn();
                }));
            }
            else if (Input.KeyTap(Game.Options.General.Binds.Skip) && (Chart.Notes.Points[0].Offset - Game.Audio.Now() > 5000))
            {
                Game.Audio.Stop();
                Game.Audio.Seek(Chart.Notes.Points[0].Offset - 5000);
                Game.Audio.Play(); //in case of leading in
            }
            //actual input stuff
            for (byte k = 0; k < Chart.Keys; k++)
            {
                if (Input.KeyTap(binds[k])) //if you press a key
                {
                    OnKeyDown(k, now); //handle it in the context of the map and where we are (now)
                }
                else if (Input.KeyRelease(binds[k])) //if you release a key
                {
                    OnKeyUp(k, now); //handle it
                }
            }
            scoreTracker.Update(now - missWindow); //check for notes you've missed and handle them
            if (scoreTracker.ReachedEnd()) //if the chart is over, go to score screen
            {
                Game.Screens.PopScreen();
                Game.Screens.AddScreen(new ScreenScore(scoreTracker));
                Children.Clear();
            }
            base.Update(bounds);
        }

        public void OnKeyDown(byte k, float now) //handle but also do the hit lighting stuff
        {
            HandleHit(k, now, false);
            lighting[k].ReceptorLight.Target = 1;
            lighting[k].ReceptorLight.Val = 1;
        }

        public void OnKeyUp(byte k, float now) //handle but also do the hit lighting stuff
        {
            HandleHit(k, now, true);
            lighting[k].ReceptorLight.Target = 0;
        }

        public void HandleHit(byte k, float now, bool release)
        {
            //basically, this whole algorithm finds the closest snap to the receptors (above or below) that is relevant (has a note in the column you're pressing)
            //- missWindow and + missWindow are used because all snaps found in that time slice are considered. the closest note to now in that column is the one you hit
            //this mechanic is different to other rhythm games where it finds the earliest unhit note in this window
            //you will miss more, but get fucked by column lockouts in jacks and dense streams less
            int i = Chart.Notes.GetNextIndex(now - missWindow);
            if (i >= Chart.Notes.Count) { return; } //if there are no more notes, stop
            float delta = missWindow; //default value for the final "found" delta"
            float d; //temp delta for the note we're looking at
            int hitAt = -1; //when we find the note with the smallest d, its index is put in here
            int needsToHold = 1023;
            bool canHitLN = true;
            while (Chart.Notes.Points[i].Offset < now + missWindow) //search loop
            {
                Snap s = Chart.Notes.Points[i];
                needsToHold &= ~(s.ends.value + s.holds.value); //if there are any starts of hold or releases within range, don't worry about finger independence penalty
                BinarySwitcher b = release ? new BinarySwitcher(s.ends.value + s.holds.value) : new BinarySwitcher(s.taps.value + s.holds.value);
                if (b.GetColumn(k) && (scoreTracker.Hitdata[i].hit[k] != 2 || scoreTracker.Hitdata[i].delta[k] < -missWindow / 2)) //todo: finalise
                {
                    d = (now - s.Offset);
                    if (release)
                    {
                        if (Chart.Notes.Points[i].ends.GetColumn(k) && canHitLN)
                        {
                            if (Math.Abs(d) < Math.Abs(delta))
                            {
                                delta = d;
                                hitAt = i;
                            }
                        }
                        else
                        {
                            canHitLN = scoreTracker.Hitdata[i].hit[k] != 1;
                        }
                    }
                    else
                    {
                        if (Math.Abs(d) < Math.Abs(delta))
                        {
                            delta = d;
                            hitAt = i;
                        }
                    }
                }
                i++;
                if (i == Chart.Notes.Count) { break; }
            } //delta is misswindow if nothing found

            if (hitAt >= 0 && (!release || Chart.Notes.Points[hitAt].ends.GetColumn(k))) //if we found a note to hit (it's -1 if nothing found)
            //this first && is added because releasing looks for anything the the column and if a release was the closest it hits it. fixes some unhittable ln behaviours.
            //the second && checks that you are holding all the columns you're supposed to. it will just miss otherwise to prevent LN cheesing
            {
                foreach (byte c in new BinarySwitcher(needsToHold & Chart.Notes.Points[hitAt].middles.value).GetColumns())
                {
                    if (!Input.KeyPress(binds[c]))
                    {
                        return;
                    }
                }
                delta /= (float)Game.Options.Profile.Rate; //convert back to time relative to what player observes instead of map data
                //i.e on 2x rate if you hit 80ms away in the map data, you hit 40ms away in reality
                if (release) { delta *= 0.5f; }
                //else { Game.Audio.PlaySFX("hit", pitch: 1f - delta * 0.5f / missWindow, volume: 1f - Math.Abs(delta) / missWindow); } //auditory feedback tests (causes performance issues)
                scoreTracker.RegisterHit(hitAt, k, delta); //handle the hit
                lighting[k].NoteLight.Val = 1;
            } //put else statement here for cb on unecessary keypress if i ever want to do that
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            if (Chart.Notes.Points[0].Offset - Game.Audio.Now() > 5000)
            {
                SpriteBatch.Font1.DrawCentredText("Press " + Game.Options.General.Binds.Skip.ToString().ToUpper() + " to Skip", 50f, 0, 130, Game.Options.Theme.MenuFont);
            }
            if (Animation.Running)
            {
                int a = 255 - (int)bannerOut;
                SpriteBatch.DrawRect(new Rect(bounds.Left, -55, bounds.Left + ScreenWidth * bannerIn, -50), Color.FromArgb(a, Game.Screens.DarkColor));
                SpriteBatch.DrawRect(new Rect(bounds.Left, 50, bounds.Left + ScreenWidth * bannerIn, 55), Color.FromArgb(a, Game.Screens.DarkColor));
                Game.Screens.DrawChartBackground(new Rect(bounds.Right - ScreenWidth * bannerIn, -50, bounds.Right, 50), Color.FromArgb(a, Game.Screens.BaseColor));
                SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title, new Rect(bounds.Right - ScreenWidth * bannerIn, -50, bounds.Right, 30), Color.FromArgb(a, Game.Options.Theme.MenuFont));
                SpriteBatch.Font2.DrawCentredTextToFill(Game.CurrentChart.Data.DiffName + " // " + Game.CurrentChart.Data.Creator, new Rect(bounds.Left, 10, bounds.Left + ScreenWidth * bannerIn, 50), Color.FromArgb(a, Game.Options.Theme.MenuFont));
            }
        }
    }
}
