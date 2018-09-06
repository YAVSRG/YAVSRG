using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Charts.YAVSRG;
using YAVSRG.Gameplay;
using static YAVSRG.Interface.ScreenUtils;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    class Playfield : GameplayWidget
    {
        int lasti; int lastt; //final indices for notes and timing points to avoid index out of bounds at end of chart

        //all storage variables for LN logic
        float[] holds;
        BinarySwitcher holdMiddles, bugFix;
        BinarySwitcher holdsInHitpos = new BinarySwitcher(0);
        int[] holdColors, holdColorsHitpos;

        protected int Keys { get { return scoreTracker.c.Keys; } }
        protected int HitPos { get { return Game.Options.Profile.HitPosition; } }
        protected int ColumnWidth { get { return Game.Options.Theme.ColumnWidth; } }
        protected float ScrollSpeed { get { return Game.Options.Profile.ScrollSpeed / (float)Game.Options.Profile.Rate; } }
        protected ChartWithModifiers Chart { get { return scoreTracker.c; } }

        protected Animations.AnimationCounter animation;

        public Playfield(ScoreTracker s) : base(s)
        {
            Game.Options.Theme.LoadTextures(Chart.Keys);

            Animation.Add(animation = new Animations.AnimationCounter(25, true));

            //i make all this stuff ahead of time so i'm not creating a ton of new objects/recalculating the same thing/sending stuff to garbage every frame
            lasti = Chart.Notes.Count;
            lastt = Chart.Timing.Count;
            holds = new float[Chart.Keys];
            holdMiddles = new BinarySwitcher(0);
            bugFix = new BinarySwitcher(0);
            holdColors = new int[Chart.Keys];
            holdColorsHitpos = new int[Chart.Keys];
        }

        //This is the core rhythm game engine bit
        public override void Draw(Rect bounds)
        {
            Rect parentBounds = bounds;
            bounds = GetBounds(bounds);
            SpriteBatch.EnableTransform(Game.Options.Profile.Upscroll);
            SpriteBatch.Enable3D();
            for (byte c = 0; c < Keys; c++) //draw columns and empty receptors
            {
                DrawColumn(bounds.Left, c);
                DrawReceptor(bounds.Left, c);
            }

            float now = (float)Game.Audio.Now(); //where are we in the song
            int i = Chart.Notes.GetNextIndex(now); //we need a copy of this number so we can increase it without messing the thing up next frame
            int t = Chart.Timing.GetLastIndex(now); //no catch up algorithm used for SV because there are less SVs and this is optimised pretty neatly
            float y = HitPos; //keeps track of where we're drawing vertically on the screen
            float v = 0; //needs a better name
            
            holdsInHitpos.value = 0; //tracker of hold notes that need to be shown in the hit position
            for (byte k = 0; k < Chart.Keys; k++) //more tracker data for drawing long notes
            {
                holds[k] = 0; //used in DrawSnapWithHolds. it's only initialised once to reduce garbage collection
                holdMiddles.RemoveColumn(k);
            }

            while (y + v < ScreenHeight * 2 && i < lasti) //continue drawing until we reach the end of the map or the top of the screen (don't need to draw notes beyond it)
            {
                while (t < lastt - 1 && Chart.Timing.Points[t + 1].Offset < Chart.Notes.Points[i].Offset) //check if we've gone past any timing points
                {
                    y += ScrollSpeed * Chart.Timing.Points[t].ScrollSpeed * (Chart.Timing.Points[t + 1].Offset - now); //handle scrollspeed adjustments
                    //SpriteBatch.DrawRect(offset, Height - y, -offset, Height - y + 5, Color.White); //bar line <-- uncomment this for white lines where timing points are (may no longer work)
                    t++; //tracks which timing point we're looking at
                    now = Chart.Timing.Points[t].Offset; //we're now drawing relative to the most recent timing point
                }
                v = Chart.Timing.Points[t].ScrollSpeed * (Chart.Notes.Points[i].Offset - now) * ScrollSpeed; //draw distance between "now" and the row of notes
                DrawSnap(Chart.Notes.Points[i], bounds.Left, y + v); //draw whole row of notes
                i++; //move on to next row of notes
            }

            if (holdsInHitpos.value > 0) //this has been updated by DrawSnapWithHolds
            {
                foreach (byte k in holdsInHitpos.GetColumns())
                {
                    Game.Options.Theme.DrawHead(new Rect(k * ColumnWidth + bounds.Left, HitPos, (k + 1) * ColumnWidth + bounds.Left, HitPos + ColumnWidth), k, Keys, holdColorsHitpos[k], animation.cycles % 8);
                }
                bugFix.value &= (ushort)~holdsInHitpos.value;
            }

            base.Draw(parentBounds);
            SpriteBatch.Disable3D();
            SpriteBatch.DisableTransform();
        }

        private void DrawLongTap(float offset, byte i, float start, float end, int color) //method name is an old inside joke
        {
            if (start == 0)
            {
                start = HitPos;
            }
            Game.Options.Theme.DrawHold(new Rect(i * ColumnWidth + offset, start + ColumnWidth * 0.5f, (i + 1) * ColumnWidth + offset, end + ColumnWidth * 0.5f), i, Keys, color, animation.cycles % 8); //Math.Abs corrects neg number
            if (holdMiddles.GetColumn(i)) //draw hold head if this isn't a middle section of a long note
            { holdMiddles.RemoveColumn(i); }
            else
            {
                Game.Options.Theme.DrawHead(new Rect(i * ColumnWidth + offset, start, (i + 1) * ColumnWidth + offset, start + ColumnWidth), i, Keys, color, animation.cycles % 8);
            }
        }

        private void DrawColumn(float offset, byte i)
        {
            SpriteBatch.Draw("playfield", new Rect(i * ColumnWidth + offset, 0, (i + 1) * ColumnWidth + offset, ScreenHeight * 2), Color.White);
        }

        private void DrawReceptor(float offset, int k)
        {
            Game.Options.Theme.DrawReceptor(new Rect(k * ColumnWidth + offset, HitPos + ColumnWidth, (k + 1) * ColumnWidth + offset, HitPos), k, Keys, Input.KeyPress(Game.Options.Profile.Bindings[Keys][k]));
        }


        private void DrawSnap(GameplaySnap s, float offset, float pos)
        {
            foreach (byte k in s.middles.GetColumns())
            {
                if (holds[k] == 0)
                {
                    holdMiddles.SetColumn(k);
                    DrawLongTap(offset, k, holds[k], pos, holdColorsHitpos[k]);
                    holdsInHitpos.SetColumn(k);
                }
                else
                {
                    DrawLongTap(offset, k, holds[k], pos, holdColors[k]);
                }
                holds[k] = pos;
                holdColors[k] = s.colors[k];
                holdMiddles.SetColumn(k);
            }
            foreach (byte k in s.ends.GetColumns())
            {
                if (holds[k] == 0)
                {
                    holdMiddles.SetColumn(k);
                    DrawLongTap(offset, k, holds[k], pos, holdColorsHitpos[k]);
                    holdsInHitpos.SetColumn(k);
                }
                else
                {
                    DrawLongTap(offset, k, holds[k], pos, holdColors[k]);
                }
                holds[k] = ScreenHeight * 2;
                holdMiddles.RemoveColumn(k);
            }
            foreach (byte k in s.holds.GetColumns())
            {
                holds[k] = pos;
                holdColors[k] = s.colors[k];
                if (!(holdsInHitpos.GetColumn(k) || bugFix.GetColumn(k)))
                {
                    holdColorsHitpos[k] = s.colors[k];
                    bugFix.SetColumn(k);
                }
            }
            foreach (byte k in s.taps.GetColumns())
            {
                //todo: optimise by generating rect once
                Game.Options.Theme.DrawNote(new Rect(k * ColumnWidth + offset, pos, (k + 1) * ColumnWidth + offset, pos + ColumnWidth), k, Keys, s.colors[k], animation.cycles % 8);
            }
            foreach (byte k in s.mines.GetColumns())
            {
                Game.Options.Theme.DrawMine(new Rect(k * ColumnWidth + offset, pos, (k + 1) * ColumnWidth + offset, pos + ColumnWidth), k, Keys, s.colors[k], animation.cycles % 8);
            }
            foreach (byte k in s.ends.GetColumns())
            {
                Game.Options.Theme.DrawTail(new Rect(k * ColumnWidth + offset, pos, (k + 1) * ColumnWidth + offset, pos + ColumnWidth), k, Keys, s.colors[k], animation.cycles % 8);
            }
        }
    }
}