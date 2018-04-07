using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Beatmap;
using YAVSRG.Gameplay;
using static YAVSRG.Interface.ScreenUtils;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    class Playfield : GameplayWidget
    {
        Sprite mine, note, hold, holdhead, receptor, playfield, screencover;

        int lasti; int lastt;
        float[] holds;
        BinarySwitcher holdsInHitpos = new BinarySwitcher(0);

        protected int Keys { get { return scoreTracker.c.Keys; } }
        protected int HitPos { get { return Game.Options.Profile.HitPosition; } }
        protected int ColumnWidth { get { return Game.Options.Theme.ColumnWidth; } }
        protected float ScrollSpeed { get { return Game.Options.Profile.ScrollSpeed / (float)Game.Options.Profile.Rate; } }
        protected ChartWithModifiers Chart { get { return scoreTracker.c; } }

        protected Animations.AnimationCounter animation;

        public Playfield(ScoreTracker s) : base(s)
        {
            mine = Content.LoadTextureFromAssets("mine");
            note = Game.Options.Theme.GetNoteTexture(Game.CurrentChart.Keys);
            receptor = Game.Options.Theme.GetReceptorTexture(Game.CurrentChart.Keys);
            hold = Game.Options.Theme.GetBodyTexture(Game.CurrentChart.Keys);
            holdhead = Game.Options.Theme.GetHeadTexture(Game.CurrentChart.Keys);
            playfield = Content.LoadTextureFromAssets("playfield");
            screencover = Content.LoadTextureFromAssets("screencover");

            Animation.Add(animation = new Animations.AnimationCounter(25, true));

            //i make all this stuff ahead of time so i'm not creating a shitload of new objects/recalculating the same thing/sending stuff to garbage every 8ms
            lasti = Chart.Notes.Count;
            lastt = Chart.Timing.Count;
            holds = new float[Chart.Keys];
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
        }

        //This is the core rhythm game engine bit
        public override void Draw(float left, float top, float right, float bottom)
        {
            float ol = left;
            float or = right;
            float ot = top;
            float ob = bottom;
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.EnableTransform(Game.Options.Profile.Upscroll);
            for (int c = 0; c < Keys; c++) //draw columns and empty receptors
            {
                DrawColumn(left, c);
                DrawReceptor(left, c);
            }

            float now = (float)Game.Audio.Now(); //where are we in the song
            int i = Chart.Notes.GetNextIndex(now); //we need a copy of this number so we can increase it without messing the thing up next frame
            int t = Chart.Timing.GetLastIndex(now); //no catch up algorithm used for SV because there are less SVs and this is optimised pretty neatly
            float y = HitPos; //keeps track of where we're drawing vertically on the screen
            float v = 0; //needs a better name

            holdsInHitpos.value = 0; //tracker of hold notes that need to be shown in the hit position
            for (int k = 0; k < Chart.Keys; k++) //more tracker data for drawing long notes
            {
                holds[k] = 0;//used in DrawSnapWithHolds. it's only initialised once to reduce garbage collection
            }

            while (y + v < ScreenHeight * 2 && i < lasti)//continue drawing until we reach the end of the map or the top of the screen (don't need to draw notes beyond it)
            {
                while (t < lastt - 1 && Chart.Timing.Points[t + 1].Offset < Chart.Notes.Points[i].Offset) //check if we've gone past any timing points
                {
                    y += ScrollSpeed * Chart.Timing.Points[t].ScrollSpeed * (Chart.Timing.Points[t + 1].Offset - now); //handle scrollspeed adjustments
                    //SpriteBatch.DrawRect(offset, Height - y, -offset, Height - y + 5, Color.White); //bar line
                    t++; //tracks which timing point we're looking at
                    now = Chart.Timing.Points[t].Offset; //we're now drawing relative to the most recent timing point
                }
                v = Chart.Timing.Points[t].ScrollSpeed * (Chart.Notes.Points[i].Offset - now) * ScrollSpeed; //draw distance between "now" and the row of notes
                DrawSnap(Chart.Notes.Points[i], left, y + v);//draw whole row of notes
                i++;//move on to next row of notes
            }

            if (holdsInHitpos.value > 0)//this has been updated by DrawSnapWithHolds
            {
                DrawSnap(new GameplaySnap(0, 0, holdsInHitpos.value, 0, 0, 0), left, HitPos); //draw hold heads in hit position

                foreach (int k in holdsInHitpos.GetColumns())
                {
                    Game.Options.Theme.DrawHead(holdhead, k * ColumnWidth + left, HitPos, (k + 1) * ColumnWidth + left, HitPos + ColumnWidth, k, Keys);
                }
            }

            base.Draw(ol, ot, or, ob);
            SpriteBatch.DisableTransform();
        }

        private void DrawLongTap(float offset, int i, float start, float end)
        {
            if (start == 0)
            {
                start = HitPos;
            }
            bool drawhead = start < 0; //if y was negative
            start = Math.Abs(start);
            SpriteBatch.Draw(hold, i * ColumnWidth + offset, start + ColumnWidth * 0.5f, (i + 1) * ColumnWidth + offset, end + ColumnWidth * 0.5f, Color.White); //Math.Abs corrects neg number
            if (drawhead)
            {
                Game.Options.Theme.DrawHead(holdhead, i * ColumnWidth + offset, start, (i + 1) * ColumnWidth + offset, start + ColumnWidth, i, Keys);
            }
        }

        private void DrawColumn(float offset, int i)
        {
            SpriteBatch.Draw(playfield, i * ColumnWidth + offset, 0, (i + 1) * ColumnWidth + offset, ScreenHeight * 2, Color.White);
        }

        private void DrawReceptor(float offset, int k)
        {
            Game.Options.Theme.DrawReceptor(receptor, k * ColumnWidth + offset, HitPos + ColumnWidth, (k + 1) * ColumnWidth + offset, HitPos, k, Keys, Input.KeyPress(Game.Options.Profile.Bindings[Keys][k]));
        }

        private void DrawSnap(GameplaySnap s, float offset, float pos)
        {
            foreach (int k in s.middles.GetColumns())
            {
                DrawLongTap(offset, k, holds[k], pos);
                if (holds[k] == 0)
                {
                    holdsInHitpos.SetColumn(k);
                }
                holds[k] = pos; //negative ys mark middle so don't draw a hold head
            }
            foreach (int k in s.ends.GetColumns())
            {
                DrawLongTap(offset, k, holds[k], pos);
                if (holds[k] == 0)
                {
                    holdsInHitpos.SetColumn(k);
                }
                holds[k] = ScreenHeight * 2;
            }
            foreach (int k in s.holds.GetColumns())
            {
                holds[k] = -pos;
            }
            foreach (int k in s.taps.GetColumns())
            {
                Game.Options.Theme.DrawNote(note, k * ColumnWidth + offset, pos, (k + 1) * ColumnWidth + offset, pos + ColumnWidth, k, Keys, s.colors[k], animation.cycles % note.UV_X);
            }
            foreach (int k in s.mines.GetColumns())
            {
                Game.Options.Theme.DrawMine(mine, k * ColumnWidth + offset, pos, (k + 1) * ColumnWidth + offset, pos + ColumnWidth, k, Keys, s.colors[k], animation.cycles % mine.UV_X);
            }
            foreach (int k in s.ends.GetColumns())
            {
                Game.Options.Theme.DrawTail(holdhead, k * ColumnWidth + offset, pos, (k + 1) * ColumnWidth + offset, pos + ColumnWidth, k, Keys);
            }
        }
    }
}