using System;
using System.Drawing;
using OpenTK.Graphics.OpenGL;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay;
using Interlude.Interface;
using Interlude.Interface.Widgets.Gameplay;
using Interlude.Interface.Animations;
using Interlude.Graphics;

namespace Interlude.Gameplay
{
    public class NoteRenderer : Widget
    {
        readonly float[] holds;
        readonly BinarySwitcher holdMiddles, bugFix;
        readonly BinarySwitcher holdsInHitpos = new BinarySwitcher(0);
        readonly int[] holdColors, holdColorsHitpos, svindex;
        readonly float[] pos, time, sv;

        readonly ChartWithModifiers Chart;
        protected int HitPos { get { return Game.Options.Profile.HitPosition; } }
        protected int ColumnWidth { get { return Game.Options.Theme.ColumnWidth; } }
        readonly int Keys;
        readonly float ScrollSpeed;
        public readonly bool UseSV = true;

        protected FBO NoteRender, Composite;

        protected AnimationCounter animation;

        public NoteRenderer(ChartWithModifiers chart)
        {
            Chart = chart;
            Game.Options.Theme.LoadGameplayTextures();

            Composite = FBO.FromPool();
            NoteRender = FBO.FromPool();
            NoteRender.Unbind();
            Composite.Unbind();

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

            AddHitlights();
        }

        public void AddHitlights()
        {
            float o = ColumnWidth * Chart.Keys * -0.5f;
            for (int i = 0; i < Chart.Keys; i++)
            {
                AddChild(new HitLighting(Game.Options.Profile.KeyBinds[Chart.Keys - 3][i])
                    .Reposition(o + i * ColumnWidth, 0.5f, HitPos, 0, o + (i + 1) * ColumnWidth, 0.5f, -HitPos, 1));
            }
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            float Height = PlayfieldHeight();
            float now = (float)Game.Audio.Now(); //where are we in the song

            for (byte c = 0; c < Keys; c++)
            {
                pos[c] = 0;
                time[c] = now;
            }

            NoteRender.Bind();
            GL.Clear(ClearBufferMask.ColorBufferBit);
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
                DrawSnap(Chart.Notes.Points[i]);
                i++; //move on to next row of notes
            }

            if (holdsInHitpos.value > 0) //this has been updated by DrawSnapWithHolds
            {
                foreach (byte k in holdsInHitpos.GetColumns())
                {
                    SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetNoteSkinTexture("holdhead"), ObjectPosition(k, 0), Color.White,
                        AnimationFrame(), holdColorsHitpos[k]));
                }
                bugFix.value &= (ushort)~holdsInHitpos.value;
            }
            NoteRender.Unbind();
            Composite.Bind();
            GL.Clear(ClearBufferMask.ColorBufferBit);

            for (byte c = 0; c < Keys; c++) //draw columns and empty receptors
            {
                SpriteBatch.DrawRect(ColumnPosition(c), Game.Options.Theme.PlayfieldColor);
                SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetNoteSkinTexture("receptor"), ObjectPosition(c, 0), Color.White,
                    AnimationFrame(), Game.Options.Profile.KeyBinds[Keys - 3][c].Held() ? 1 : 0));
            }

            float sct = -ScreenUtils.ScreenHeight + ScreenUtils.ScreenHeight * 2 * Game.Options.Profile.ScreenCoverDown;
            float scb = -ScreenUtils.ScreenHeight + ScreenUtils.ScreenHeight * 2 * (1 - Game.Options.Profile.ScreenCoverUp);
            SpriteBatch.DrawTilingTexture(NoteRender, new Rect(-ScreenUtils.ScreenWidth, sct - Game.Options.Profile.ScreenCoverFadeLength, ScreenUtils.ScreenWidth, sct),
                ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, Color.Transparent, Color.Transparent, Color.White, Color.White);

            SpriteBatch.DrawTilingTexture(NoteRender, new Rect(-ScreenUtils.ScreenWidth, sct, ScreenUtils.ScreenWidth, scb),
                ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, Color.White, Color.White, Color.White, Color.White);

            SpriteBatch.DrawTilingTexture(NoteRender, new Rect(-ScreenUtils.ScreenWidth, scb, ScreenUtils.ScreenWidth, scb + Game.Options.Profile.ScreenCoverFadeLength),
                ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0.5f, 0.5f, Color.White, Color.White, Color.Transparent, Color.Transparent);
            DrawWidgets(bounds);

            Composite.Unbind();

            SpriteBatch.Enable3D();
            SpriteBatch.EnableTransform(false);
            SpriteBatch.Draw(new RenderTarget(Composite, bounds, Color.White));
            SpriteBatch.DisableTransform();
            SpriteBatch.Disable3D();
        }

        private void DrawLongTap(byte i, float start, float end, int color)
        {
            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetNoteSkinTexture("holdbody"), HoldPosition(i, start, end), Color.White, AnimationFrame(), color));
            if (holdMiddles.GetColumn(i)) //draw hold head if this isn't a middle section of a long note
            { holdMiddles.RemoveColumn(i); }
            else
            {
                SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetNoteSkinTexture("holdhead"), ObjectPosition(i, start), Color.White,
                    AnimationFrame(), color));
            }
        }
        private void DrawSnap(GameplaySnap s)
        {
            foreach (byte k in s.middles.GetColumns())
            {
                if (holds[k] == 0)
                {
                    holdMiddles.SetColumn(k);
                    DrawLongTap(k, holds[k], pos[k], holdColorsHitpos[k]);
                    holdsInHitpos.SetColumn(k);
                }
                else
                {
                    DrawLongTap(k, holds[k], pos[k], holdColors[k]);
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
                    DrawLongTap(k, holds[k], pos[k], holdColorsHitpos[k]);
                    holdsInHitpos.SetColumn(k);
                }
                else
                {
                    DrawLongTap(k, holds[k], pos[k], holdColors[k]);
                }
                holds[k] = PlayfieldHeight();
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
                SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetNoteSkinTexture("note"), ObjectPosition(k, pos[k]), Color.White,
                    AnimationFrame(), s.colors[k]).Rotate(NoteRotation(k)));
            }
            foreach (byte k in s.mines.GetColumns())
            {
                SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetNoteSkinTexture("mine"), ObjectPosition(k, pos[k]), Color.White,
                    AnimationFrame(), s.colors[k]));
            }
            foreach (byte k in s.ends.GetColumns())
            {
                bool FlipTail = (Game.Options.Themes.NoteSkins[Game.Options.Profile.NoteSkin].FlipHoldTail && !Game.Options.Themes.NoteSkins[Game.Options.Profile.NoteSkin].UseHoldTailTexture) ^ Game.Options.Profile.Upscroll;
                Sprite sprite = Game.Options.Themes.GetNoteSkinTexture(Game.Options.Themes.NoteSkins[Game.Options.Profile.NoteSkin].UseHoldTailTexture ? "holdtail" : "holdhead");
                Rect rect = ObjectPosition(k, pos[k]);
                if (FlipTail) rect = rect.FlipY();
                RenderTarget target = new RenderTarget(sprite, ObjectPosition(k, pos[k]), Color.White, AnimationFrame(), s.colors[k]);
                if (!Game.Options.Themes.NoteSkins[Game.Options.Profile.NoteSkin].UseHoldTailTexture) target = target.Rotate(NoteRotation(k));
                SpriteBatch.Draw(target);
            }
        }

        int NoteRotation(int column)
        {
            if (Game.Options.Themes.NoteSkins[Game.Options.Profile.NoteSkin].UseRotation)
            {
                if (Chart.Keys == 4)
                {
                    switch (column)
                    {
                        case 0: { return 3; }
                        case 1: { return 0; }
                        case 2: { return 2; }
                        case 3: { return 1; }
                    }
                }
                //?? rotation for other keymodes?
            }
            return 0;
        }

        int PlayfieldHeight()
        {
            return ScreenUtils.ScreenHeight * 2;
        }

        int AnimationFrame()
        {
            return animation.cycles % 8;
        }

        Rect ObjectPosition(int column, float position)
        {
            float offset = ColumnWidth * Chart.Keys * -0.5f;
            float bottom = PositionFunc(position);
            return new Rect(offset + column * ColumnWidth, bottom - ColumnWidth, offset + (column + 1) * ColumnWidth, bottom);
        }

        Rect HoldPosition(int column, float start, float end)
        {
            float offset = ColumnWidth * Chart.Keys * -0.5f;
            float bottom = PositionFunc(start) - Game.Options.Theme.ColumnWidth  * 0.5f;
            float top = PositionFunc(end) - Game.Options.Theme.ColumnWidth * 0.5f;
            return new Rect(offset + column * ColumnWidth, top, offset + (column + 1) * ColumnWidth, bottom);
        }

        float PositionFunc(float pos)
        {
            float p = PlayfieldHeight() / 2 - pos - Game.Options.Profile.HitPosition;
            if (Game.Options.Profile.Upscroll) p = -p + Game.Options.Theme.ColumnWidth;
            return p;
        }

        Rect ColumnPosition(int column)
        {
            float offset = ColumnWidth * Chart.Keys * -0.5f;
            return new Rect(offset + column * ColumnWidth, -PlayfieldHeight() / 2, offset + (column + 1) * ColumnWidth, PlayfieldHeight() / 2);
        }

        public override void Dispose()
        {
            base.Dispose();
            NoteRender.Dispose();
            Composite.Dispose();
        }
    }
}
