using System;
using System.Drawing;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay;
using Interlude.Gameplay;
using Interlude.Interface.Widgets.Gameplay;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;
using static Interlude.Interface.ScreenUtils;

namespace Interlude.Interface.Screens
{
    /// <summary>
    /// The screen for playing and scoring on a selected chart. Uses the NoteRenderer class to render the playfield and then handles input and note hitting logic.
    /// </summary>
    class ScreenPlay : Screen
    {
        ChartWithModifiers Chart;
        ScoreTracker scoreTracker;
        float missWindow;
        Bind[] binds;
        AnimationFade bannerIn, bannerOut;

        public ScreenPlay()
        {
            Chart = Game.Gameplay.ModifiedChart;
            scoreTracker = new ScoreTracker(Game.Gameplay.ModifiedChart);

            missWindow = scoreTracker.Scoring.MissWindow * (float)Game.Options.Profile.Rate;
            binds = Game.Options.Profile.KeyBinds[Chart.Keys - 3];

            var widgetData = Game.Options.Themes.GetUIConfig("gameplay");

            AddChild(new NoteRenderer(Chart));

            AddChild(new HitMeter(scoreTracker, widgetData.GetWidgetConfig("hitMeter", -250, 0.5f, 150, 0.5f, 250, 0.5f, 20, 0.5f, true)));
            AddChild(new ComboDisplay(scoreTracker, widgetData.GetWidgetConfig("combo", -100, 0.5f, 100, 0.5f, 100, 0.5f, 101, 0.5f, true)));
            AddChild(new ProgressBar(scoreTracker, widgetData.GetWidgetConfig("progressBar", 0, 0, -10, 1, 0, 1, 0, 1, true)));
            AddChild(new AccMeter(scoreTracker, widgetData.GetWidgetConfig("accuracy", -200, 0.5f, 50, 0, 200, 0.5f, 150, 0, true)));
            AddChild(new HPMeter(scoreTracker, widgetData.GetWidgetConfig("healthBar", 20, 0, 20, 0, 520, 0, 50, 0, true)));
            AddChild(new MiscInfoDisplay(scoreTracker, widgetData.GetWidgetConfig("fps", -220, 1, -180, 1, -20, 1, -100, 1, false), () => { return ((int)Game.Instance.FPS).ToString() + "fps"; }));
            AddChild(new MiscInfoDisplay(scoreTracker, widgetData.GetWidgetConfig("time", -220, 1, -100, 1, -20, 1, -20, 1, true), () => { return DateTime.Now.ToLongTimeString(); }));
            AddChild(new MiscInfoDisplay(scoreTracker, widgetData.GetWidgetConfig("timeLeft", -220, 1, 20, 0, -20, 1, 100, 0, false), () => { return Utils.FormatTime((Chart.Notes.Points[Chart.Notes.Points.Count - 1].Offset - (float)Game.Audio.Now()) / (float)Game.Options.Profile.Rate) + " left"; }));
            AddChild(new JudgementCounter(scoreTracker, widgetData.GetWidgetConfig("judgements", 70, 0, -180, 0.5f, 320, 0, 180, 0.5f, false)));
            //AddChild(new Mascot(scoreTracker, widgetData.GetWidgetConfig("mascot", 0, 0, 0, 0, 1, 1, 1, 1, false)));
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
            Discord.SetPresence("Playing a chart", Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title + " [" + Game.CurrentChart.Data.DiffName + "]\nFrom " + Game.CurrentChart.Data.SourcePack, false); ;
            Game.Options.Profile.Stats.TimesPlayed++;
            Game.Screens.Toolbar.SetState(WidgetState.DISABLED);
            Game.Screens.Toolbar.SetCursorState(false);
            AnimationSeries s = new AnimationSeries(false);
            s.Add(bannerIn = new AnimationFade(0, 2.001f, 2f));
            s.Add(new AnimationCounter(60, false));
            s.Add(new AnimationAction(() => { scoreTracker.WidgetColor.Target = Game.Options.General.HideGameplayUI || Game.Gameplay.SelectedMods.ContainsKey("Auto") ? 0 : 1; }));
            s.Add(bannerOut = new AnimationFade(0, 255, 254));
            Animation.Add(s);

            //prep audio and play from beginning
            Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
            Game.Audio.OnPlaybackFinish = null;
            Game.Audio.Stop();
            Game.Audio.SetRate(Game.Options.Profile.Rate);
            Game.Audio.PlayLeadIn();
            System.Runtime.GCSettings.LatencyMode = System.Runtime.GCLatencyMode.SustainedLowLatency;
        }

        public override void OnExit(Screen next)
        {
            //some misc stuff
            Game.Screens.BackgroundDim.Target = 0.3f;
            Discord.SetPresence("Selecting another chart", Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title + " [" + Game.CurrentChart.Data.DiffName + "]\nFrom " + Game.CurrentChart.Data.SourcePack, true);
            Game.Screens.Toolbar.SetState(WidgetState.ACTIVE);
            Game.Screens.Toolbar.SetCursorState(true);
            base.OnExit(next);
        }

        public override void Update(Rect bounds) //update loop
        {
            float now = (float)Game.Audio.Now(); //get where we are now
            if (Game.Options.General.Hotkeys.Exit.Tapped()) //escape quits
            {
                Game.Screens.PopScreen();
                Children.Clear();
                Game.Options.Profile.Stats.TimesQuit++;
            }
            else if (Game.Audio.Using_Timer && Game.Options.General.Hotkeys.ChangeOffset.Tapped()) //todo: proper pre-gameplay menu with offset changer, score system changer, scroll speed changer
            {
                Game.Audio.Stop();
                Game.Screens.AddDialog(new Dialogs.TextDialog("Change sync by... (ms)", (x) =>
                {
                    float.TryParse(x, out float f); Game.Gameplay.ChartSaveData.Offset += f;
                    Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
                    Game.Audio.PlayLeadIn();
                }));
            }
            else if (Game.Options.General.Hotkeys.Skip.Tapped() && (Chart.Notes.Points[0].Offset - Game.Audio.Now() > 3000))
             {
                Game.Audio.Stop();
                Game.Audio.Play((long)(Chart.Notes.Points[0].Offset - 3000)); //in case of leading in
            }
            else if (Game.Options.General.Hotkeys.HideUI.Tapped() && !Animation.Running)
            {
                Game.Options.General.HideGameplayUI = !Game.Options.General.HideGameplayUI;
                scoreTracker.WidgetColor.Target = Game.Options.General.HideGameplayUI ? 0 : 1;
            }

            //actual input stuff
            for (byte k = 0; k < Chart.Keys; k++)
            {
                if (binds[k].Tapped()) //if you press a key
                {
                    OnKeyDown(k, now); //handle it in the context of the map and where we are (now)
                }
                else if (binds[k].Released()) //if you release a key
                {
                    OnKeyUp(k, now); //handle it
                }
            }
            scoreTracker.Update(now); //check for notes you've missed and handle them
            if (scoreTracker.GameOver()) //if the chart is over, go to score screen
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
        }

        public void OnKeyUp(byte k, float now) //handle but also do the hit lighting stuff
        {
            HandleHit(k, now, true);
        }

        public void HandleHit(byte k, float now, bool release)
        {
            //basically, this whole algorithm finds the snap that is
            // - closest to the receptors (can be above or below)
            // - contains a note in the column being hit
            // - this note has not been hit with a great or better judgement
            //this is different to most rhythm games, where it is normally "earliest note within the miss window that has not been hit"
            //the intention behind this is to largely reduce "cbrushing" where hitting a combo breaking judgement can cause a chain of more combo breakers
            //basically, if you continue to play the rest of the notes as normal most rhythm game engines will grab notes you don't mean to be hitting because you expected to have already hit them and are aiming for the next note
            //this system should typically cause one or two combo breaks where the player actually trips up but not then form a loop
            //i think this is generally a lot fairer and better (despite a lot of "just get good" arguments) and will lead to a greater ability to rate difficulty,
                //since some charts are more vulnerable to the soul crushing defeat of locking out of a whole column than others but should not be rated differently due to farming by getting lucky
            //imo this is a fairly key thing holding high level keyboard rhythm games back, as this was clearly never intended but rather a side effect of game engines designed around notes that would not be within 180ms of each other in the same column
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
                    if (!binds[c].Held())
                    {
                        return;
                    }
                }
                delta /= (float)Game.Options.Profile.Rate; //convert back to time relative to what player observes instead of map data
                //i.e on 2x rate if you hit 80ms away in the map data, you hit 40ms away in reality
                if (release) { delta *= 0.5f; }
                //else { Game.Audio.PlaySFX("hit", pitch: 1f - delta * 0.5f / missWindow, volume: 1f - Math.Abs(delta) / missWindow); } //auditory feedback tests (causes performance issues)
                scoreTracker.RegisterHit(hitAt, k, delta); //handle the hit
            } //put else statement here for cb on unecessary keypress if i ever want to do that
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            if (Chart.Notes.Points[0].Offset - Game.Audio.Now() > 5000)
            {
                SpriteBatch.Font1.DrawCentredText("Press " + Game.Options.General.Hotkeys.Skip.ToString().ToUpper() + " to Skip", 50f, 0, 130, Game.Options.Theme.MenuFont);
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
