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
    class ScreenPlay : Screen //i cleaned up this file a little but it's a bit of a mess. sorry!
    {
        class HitLighting : Widget //this is the hitlighting widget and needs to be moved to its own file
        {
            public AnimationSlider NoteLight = new AnimationSlider(0);
            public AnimationSlider ReceptorLight = new AnimationSlider(0);
            Sprite s = Content.LoadTextureFromAssets("receptorlighting");

            public HitLighting() : base()
            {
                Animation.Add(NoteLight);
                Animation.Add(ReceptorLight);
            }

            public override void Draw(float left, float top, float right, float bottom) //draws hitlight, right now just the receptor light and not a flash when you hit a note
            {
                base.Draw(left, top, right, bottom);
                ConvertCoordinates(ref left, ref top, ref right, ref bottom);
                if (ReceptorLight.Val > 0.5f)
                {
                    float w = (right - left);
                    SpriteBatch.Draw(s, left + w * (1 - ReceptorLight.Val), top + 3 * w * (1 - ReceptorLight.Val), right - w * (1 - ReceptorLight.Val), bottom, Color.White);
                }
            }
        }

        int COLUMNWIDTH = Game.Options.Theme.ColumnWidth; //some constants that really ought not to be here any more
        int HITPOSITION = Game.Options.Profile.HitPosition;
        
        int lasti; int lastt; //the number of snaps and number of timing points respectively. used to be an optimisation and now they're just a convenience
        ChartWithModifiers Chart;
        ScoreTracker scoreTracker;
        Widget playfield;
        float missWindow;
        Key[] binds;
        HitLighting[] lighting;

        public ScreenPlay()
        {
            Chart = Game.Gameplay.ModifiedChart;
            scoreTracker = new ScoreTracker(Game.Gameplay.ModifiedChart);

            //i make all this stuff ahead of time as a small optimisation
            lasti = Chart.Notes.Count;
            lastt = Chart.Timing.Count;
            missWindow = scoreTracker.Scoring.MissWindow * (float)Game.Options.Profile.Rate;
            binds = Game.Options.Profile.Bindings[Chart.Keys];

            //this stuff is absolutely fine to stay here
            AddChild(playfield = new Playfield(scoreTracker).PositionTopLeft(-COLUMNWIDTH * Chart.Keys * 0.5f, 0, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(COLUMNWIDTH * Chart.Keys * 0.5f, 0, AnchorType.CENTER, AnchorType.MAX));
            AddChild(new HitMeter(scoreTracker).PositionTopLeft(-COLUMNWIDTH * Chart.Keys / 2, 0, AnchorType.CENTER, AnchorType.CENTER).PositionBottomRight(COLUMNWIDTH * Chart.Keys / 2, 0, AnchorType.CENTER, AnchorType.MAX));
            AddChild(new ProgressBar(scoreTracker).PositionTopLeft(-500, 10, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(500, 50, AnchorType.CENTER, AnchorType.MIN));
            AddChild(new ComboDisplay(scoreTracker).PositionTopLeft(0, -100, AnchorType.CENTER, AnchorType.CENTER));

            //all this stuff needs to be moved to Playfield under a method that adds gameplay elements (not used when in editor)
            //playfield.InitGameplay();
            lighting = new HitLighting[Chart.Keys];
            float x = Chart.Keys * 0.5f;
            //this places a hitlight on every column
            for (int i = 0; i < Chart.Keys; i++)
            {
                lighting[i] = new HitLighting();
                lighting[i].PositionTopLeft(COLUMNWIDTH * i, HITPOSITION + COLUMNWIDTH * 2, AnchorType.MIN, AnchorType.CENTER)
                    .PositionBottomRight(COLUMNWIDTH * (i + 1), HITPOSITION, AnchorType.MIN, AnchorType.CENTER);
                playfield.AddChild(lighting[i]);
            }
            //this places the screencovers
            playfield.AddChild(new Screencover(scoreTracker, false)
                .PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.CENTER).PositionBottomRight(0, ScreenHeight * 2 * Game.Options.Profile.ScreenCoverUp, AnchorType.MAX, AnchorType.CENTER));
            playfield.AddChild(new Screencover(scoreTracker, true)
                .PositionTopLeft(0, ScreenHeight * 2 * (1 - Game.Options.Profile.ScreenCoverDown), AnchorType.MIN, AnchorType.CENTER).PositionBottomRight(0, ScreenHeight * 2, AnchorType.MAX, AnchorType.CENTER));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            //if returning from score screen, go straight to menu
            if (prev is ScreenScore)
            {
                Game.Audio.Loop = true;
                Game.Screens.PopScreen(); return;
            }
            //some misc stuff
            Utils.SetDiscordData("Playing", ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title + " [" + Game.CurrentChart.DifficultyName + "]");
            Game.Options.Profile.Stats.TimesPlayed++;
            Game.Screens.toolbar.SetHidden(true);

            //prep audio and play from beginning
            Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
            Game.Audio.Loop = false;
            Game.Audio.Stop();
            Game.Audio.SetRate(Game.Options.Profile.Rate);
            Game.Audio.PlayLeadIn();
        }

        public override void OnExit(Screen next)
        {
            //some misc stuff
            Utils.SetDiscordData("Looking for something to play", "");
            Game.Screens.toolbar.SetHidden(false);
            base.OnExit(next);
        }

        public override void Update(float left, float top, float right, float bottom) //update loop
        {
            float now = (float)Game.Audio.Now(); //get where we are now
            if (Input.KeyTap(Key.Escape)) //escape quits
            {
                Game.Screens.PopScreen();
                Widgets.Clear();
                Game.Options.Profile.Stats.TimesQuit++;
            }
            else if (Game.Audio.LeadingIn && Input.KeyTap(Key.Plus)) //if map hasn't started you can sync it with + key
            {
                Game.Audio.Stop();
                Game.Screens.AddDialog(new Dialogs.TextDialog("Change sync by... (ms)", (x) => {
                    float f = 0; float.TryParse(x, out f); Game.Gameplay.ChartSaveData.Offset += f;
                    Game.Audio.LocalOffset = Game.Gameplay.GetChartOffset();
                    Game.Audio.PlayLeadIn();
                }));
            }
            //actual input stuff
            for (int k = 0; k < Chart.Keys; k++)
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
            if (scoreTracker.EndOfChart()) //if the chart is over, go to score screen
            {
                Game.Screens.PopScreen();
                Game.Screens.AddScreen(new ScreenScore(scoreTracker));
                Widgets.Clear();
            }
            base.Update(left, top, right, bottom);
        }

        public void OnKeyDown(int k, float now) //handle but also do the hit lighting stuff
        {
            HandleHit(k, now, false);
            lighting[k].ReceptorLight.Target = 1;
            lighting[k].ReceptorLight.Val = 1;
        }

        public void OnKeyUp(int k, float now) //handle but also do the hit lighting stuff
        {
            HandleHit(k, now, true);
            lighting[k].ReceptorLight.Target = 0;
        }

        public void HandleHit(int k, float now, bool release)
        {
            //basically, this whole algorithm finds the closest snap to the receptors (above or below) that is relevant (has a note in the column you're pressing)
            //- missWindow and + missWindow are used because all snaps found in that time slice are considered. the closest note to now in that column is the one you hit
            //this mechanic is different to other rhythm games where it finds the earliest unhit note in this window
            //you will miss more, but get fucked by column lockouts in jacks and dense streams less
            int i = Chart.Notes.GetNextIndex(now - missWindow);
            if (i >= lasti) { return; } //if there are no more notes, stop
            float delta = missWindow; //default value for the final "found" delta"
            float d; //temp delta for the note we're looking at
            int hitAt = -1; //when we find the note with the smallest d, its index is put in here
            while (Chart.Notes.Points[i].Offset < now + missWindow) //search loop
            {
                Snap s = Chart.Notes.Points[i];
                BinarySwitcher b = release ? new BinarySwitcher(s.ends.value + s.taps.value + s.holds.value) : new BinarySwitcher(s.taps.value + s.holds.value);
                if (b.GetColumn(k)) //if there's a note here
                {
                    d = (now - s.Offset);
                    if (Math.Abs(d) < Math.Abs(delta))
                    {
                        delta = d;
                        hitAt = i;
                    }
                }
                i++;
                if (i == lasti) { break; }
            } //delta is misswindow if nothing found
            //LOOK AT THIS LINE \/ \/ \/ IT IS IMPORTANT I DO SOMETHING ABOUT IT
            if (release) delta *= 0.5f; //releasing long notes is more lenient (hit windows twice as big). needs a setting to turn on and off
            if (hitAt >= 0 && (!release || Chart.Notes.Points[hitAt].ends.GetColumn(k))) //if we found a note to hit (it's -1 if nothing found)
                //this extra && is added because releasing looks for anything the the column and if a release was the closest it hits it. fixes some unhittable ln behaviours.
            {
                delta /= (float)Game.Options.Profile.Rate; //convert back to time relative to what player observes instead of map data
                //i.e on 2x rate if you hit 80ms away in the map data, you hit 40ms away in reality
                scoreTracker.RegisterHit(hitAt, k, delta); //handle the hit
            } //put else statement here for cb on unecessary keypress if i ever want to do that
        }
        
        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            SpriteBatch.Font1.DrawCentredText(Utils.RoundNumber(scoreTracker.Accuracy()), 40f, 0, -ScreenHeight + 70, Color.White); //acc needs to be moved to a widget
        }
    }
}
