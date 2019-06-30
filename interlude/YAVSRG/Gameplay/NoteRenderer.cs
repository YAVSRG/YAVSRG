using System;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay;
using Interlude.Interface;
using Interlude.Interface.Animations;
using Interlude.Gameplay.Mods;
using Interlude.Graphics;
using Interlude.IO;

namespace Interlude.Gameplay
{
    public class NoteRenderer : Widget
    {
        float[] holds;
        BinarySwitcher holdMiddles, bugFix;
        BinarySwitcher holdsInHitpos = new BinarySwitcher(0);
        int[] holdColors, holdColorsHitpos, svindex;
        float[] pos, time, sv;

        ChartWithModifiers Chart;
        protected int HitPos { get { return Game.Options.Profile.HitPosition; } }
        protected int ColumnWidth { get { return Game.Options.Theme.ColumnWidth; } }
        readonly int Keys;
        readonly float ScrollSpeed;
        public readonly bool UseSV = true;

        protected IVisualMod ScrollScheme;

        protected AnimationCounter animation;

        public NoteRenderer(ChartWithModifiers chart, IVisualMod mod)
        {
            Chart = chart;
            Game.Options.Theme.LoadGameplayTextures(Chart.Keys);
            ScrollScheme = mod;

            Animation.Add(animation = new AnimationCounter(25, true));

            //i make all this stuff ahead of time so i'm not creating a ton of new arrays/recalculating the same thing/sending stuff to garbage every frame
            Keys = Chart.Keys;
            ScrollSpeed = Game.Options.Profile.ScrollSpeed / (float)Game.Options.Profile.Rate;
            holds = new float[Chart.Keys];
            holdMiddles = new BinarySwitcher(0);
            bugFix = new BinarySwitcher(0);
            holdColors = new int[Chart.Keys];
            holdColorsHitpos = new int[Chart.Keys];
            pos = new float[Chart.Keys];
            time = new float[Chart.Keys];
            sv = new float[Chart.Keys + 1];
            svindex = new int[Chart.Keys + 1];
        }

        public void AddHitlights()
        {
            //todo: fix/bring back hitlighting
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            SpriteBatch.Enable3D();
            SpriteBatch.EnableTransform(false);
            float Height = ScrollScheme.Height;
            float now = (float)Game.Audio.Now(); //where are we in the song

            for (byte c = 0; c < Keys; c++) //draw columns and empty receptors
            {
                //DrawColumn(bounds.Left, c);
                SpriteBatch.Draw("playfield", ScrollScheme.PlaceObject(c, 0, IVisualMod.ObjectType.Backdrop), 0, 0);
                Game.Options.Theme.DrawReceptor(ScrollScheme.PlaceObject(c, 0, IVisualMod.ObjectType.Receptor), c, Keys, Game.Options.Profile.KeyBinds[Keys - 3][c].Held());
                //DrawReceptor(bounds.Left, c);
                pos[c] = HitPos;
                time[c] = now;
            }

            int i = Chart.Notes.GetNextIndex(now); //find next row above hitpos to render
            for (byte k = 0; k < Keys + 1; k++)
            {
                svindex[k] = Chart.Timing.SV[k].GetLastIndex(now);
                sv[k] = !UseSV || svindex[k] == -1 ? 1f : Chart.Timing.SV[k].Points[svindex[k]].ScrollSpeed;
            }

            holdsInHitpos.value = 0; //tracker of hold notes that need to be shown in the hit position
            for (byte k = 0; k < Chart.Keys; k++) //more tracker data for drawing long notes
            {
                holds[k] = 0; //used in DrawSnapWithHolds. it's only initialised once to reduce garbage collection
                holdMiddles.RemoveColumn(k);
            }

            float min = 0;
            while (min < Height && i < Chart.Notes.Count) //continue drawing until we reach the end of the map or the top of the screen (don't need to draw notes beyond it)
            {
                min = Height; //used to see if we've gone off the screen in all columns yet (and therefore stop rendering more notes, they'd be offscreen)

                if (UseSV)
                {
                    //calculates main SV, affecting all columns
                    while (svindex[0] < Chart.Timing.SV[0].Count - 1 && Chart.Timing.SV[0].Points[svindex[0] + 1].Offset < Chart.Notes.Points[i].Offset)
                    {
                        for (byte k = 0; k < Keys; k++)
                        {
                            pos[k] += ScrollSpeed * sv[0] * sv[k + 1] * (Chart.Timing.SV[0].Points[svindex[0] + 1].Offset - time[k]);
                            time[k] = Chart.Timing.SV[0].Points[svindex[0] + 1].Offset;
                        }
                        svindex[0]++;
                        sv[0] = Chart.Timing.SV[0].Points[svindex[0]].ScrollSpeed;
                    }

                    //calculates column specific SV
                    for (byte k = 0; k < Keys; k++)
                    {
                        byte j = (byte)(k + 1); //for sv and svindex
                        while (svindex[j] < Chart.Timing.SV[j].Count - 1 && Chart.Timing.SV[j].Points[svindex[j] + 1].Offset < Chart.Notes.Points[i].Offset)
                        {
                            pos[k] += ScrollSpeed * sv[0] * sv[j] * (Chart.Timing.SV[j].Points[svindex[j] + 1].Offset - time[k]);
                            time[k] = Chart.Timing.SV[j].Points[svindex[j] + 1].Offset;
                            svindex[j]++;
                            sv[j] = Chart.Timing.SV[j].Points[svindex[j]].ScrollSpeed;
                        }
                    }
                }

                //updates position of notes after SV changes (if any)
                for (byte k = 0; k < Keys; k++)
                {
                    pos[k] += ScrollSpeed * sv[0] * sv[k + 1] * (Chart.Notes.Points[i].Offset - time[k]); //draw distance between "now" and the row of notes
                    time[k] = Chart.Notes.Points[i].Offset;
                    min = Math.Min(pos[k], min);
                }

                //renders next row of notes (positions per column are globally accessible)
                DrawSnap(Chart.Notes.Points[i], bounds.Left);
                i++; //move on to next row of notes
            }

            if (holdsInHitpos.value > 0) //this has been updated by DrawSnapWithHolds
            {
                foreach (byte k in holdsInHitpos.GetColumns())
                {
                    Game.Options.Theme.DrawHead(ScrollScheme.PlaceObject(k, 0, IVisualMod.ObjectType.Head), k, Keys, holdColorsHitpos[k], animation.cycles % 8);
                }
                bugFix.value &= (ushort)~holdsInHitpos.value;
            }

            //DrawWidgets(bounds);
            SpriteBatch.DisableTransform();
            SpriteBatch.Disable3D();

        }

        private void DrawLongTap(float offset, byte i, float start, float end, int color)
        {
            foreach (Plane p in ScrollScheme.DrawHold(i, start, end)) { Game.Options.Theme.DrawHold(p, i, Keys, color, animation.cycles % 8); }
            if (holdMiddles.GetColumn(i)) //draw hold head if this isn't a middle section of a long note
            { holdMiddles.RemoveColumn(i); }
            else
            {
                Game.Options.Theme.DrawHead(ScrollScheme.PlaceObject(i, start, IVisualMod.ObjectType.Head), i, Keys, color, animation.cycles % 8);
            }
        }
        private void DrawSnap(GameplaySnap s, float offset)
        {
            foreach (byte k in s.middles.GetColumns())
            {
                if (holds[k] == 0)
                {
                    holdMiddles.SetColumn(k);
                    DrawLongTap(offset, k, holds[k], pos[k], holdColorsHitpos[k]);
                    holdsInHitpos.SetColumn(k);
                }
                else
                {
                    DrawLongTap(offset, k, holds[k], pos[k], holdColors[k]);
                }
                holds[k] = pos[k];
                holdColors[k] = s.colors[k];
                holdMiddles.SetColumn(k);
            }
            foreach (byte k in s.ends.GetColumns())
            {
                if (holds[k] == 0)
                {
                    holdMiddles.SetColumn(k);
                    DrawLongTap(offset, k, holds[k], pos[k], holdColorsHitpos[k]);
                    holdsInHitpos.SetColumn(k);
                }
                else
                {
                    DrawLongTap(offset, k, holds[k], pos[k], holdColors[k]);
                }
                holds[k] = ScrollScheme.Height;
                holdMiddles.RemoveColumn(k);
            }
            foreach (byte k in s.holds.GetColumns())
            {
                holds[k] = pos[k];
                holdColors[k] = s.colors[k];
                if (!(holdsInHitpos.GetColumn(k) || bugFix.GetColumn(k)))
                {
                    holdColorsHitpos[k] = s.colors[k];
                    bugFix.SetColumn(k);
                }
            }
            foreach (byte k in s.taps.GetColumns())
            {
                Game.Options.Theme.DrawNote(ScrollScheme.PlaceObject(k, pos[k], IVisualMod.ObjectType.Note), k, Keys, s.colors[k], animation.cycles % 8);
            }
            foreach (byte k in s.mines.GetColumns())
            {
                Game.Options.Theme.DrawMine(ScrollScheme.PlaceObject(k, pos[k], IVisualMod.ObjectType.Mine), k, Keys, s.colors[k], animation.cycles % 8);
            }
            foreach (byte k in s.ends.GetColumns())
            {
                Game.Options.Theme.DrawTail(ScrollScheme.PlaceObject(k, pos[k], IVisualMod.ObjectType.Tail), k, Keys, s.colors[k], animation.cycles % 8);
            }
        }
    }
}
