using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Gameplay.Watchers;

namespace YAVSRG.Interface
{
    class ScreenUtils
    {
        public static int ScreenWidth;

        public static int ScreenHeight;

        public static Rect Bounds
        {
            get { return new Rect(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight); }
        }

        public static void UpdateBounds(int Width, int Height)
        {
            ScreenWidth = Width / 2;
            ScreenHeight = Height / 2;
            if (ScreenWidth < 960 || ScreenHeight < 500)
            {
                float r = Math.Max(960f / ScreenWidth, 500f / ScreenHeight);
                ScreenWidth = (int)(ScreenWidth * r);
                ScreenHeight = (int)(ScreenHeight * r);
            }
        }

        public static bool MouseOver(Rect bounds)
        {
            int mx = Input.MouseX;
            int my = Input.MouseY;
            return (mx > bounds.Left && mx < bounds.Right && my > bounds.Top && my < bounds.Bottom);
        }

        public static bool CheckButtonClick(Rect bounds)
        {
            return MouseOver(bounds) && Input.MouseClick(OpenTK.Input.MouseButton.Left);
        }

        public static void DrawGraph(Rect bounds, IScoreSystem scoring, Gameplay.ScoreTracker.HitData[] data)
        {
            int snapcount = data.Length;
            SpriteBatch.DrawRect(bounds, Color.FromArgb(150, 0, 0, 0));
            float w = (bounds.Width - 10) / snapcount;
            float middle = bounds.CenterY;
            float scale = (bounds.Height - 20) * 0.5f / scoring.MissWindow;
            SpriteBatch.DrawRect(new Rect(bounds.Left, middle - 3, bounds.Right, middle + 3), Color.Green);
            int j;
            float o;
            for (int i = 0; i < snapcount; i++)
            {
                for (int k = 0; k < data[i].hit.Length; k++)
                {
                    if (data[i].hit[k] > 0)
                    {
                        o = data[i].delta[k];
                        j = scoring.JudgeHit(o);
                        if (j > 2)
                        {
                            SpriteBatch.DrawRect(new Rect(bounds.Left + i * w + 4, bounds.Top, bounds.Left + i * w + 6, bounds.Bottom), Color.FromArgb(80, Game.Options.Theme.JudgeColors[5]));
                        }
                        SpriteBatch.DrawRect(new Rect(bounds.Left + i * w + 3, middle - o * scale - 2, bounds.Left + i * w + 8, middle - o * scale + 2), Game.Options.Theme.JudgeColors[j]);
                    }
                }
            }
            DrawFrame(bounds, 30f, Color.White);
        }

        public static void DrawFrame(Rect bounds, float ignore, Color color, float shadow = 10f, float thickness = 3f)
        {/*
            //corners
            SpriteBatch.Draw("frame", new Rect(bounds.Left, bounds.Top, bounds.Left + scale, bounds.Top + scale), color, 0, 0);
            SpriteBatch.Draw("frame", new Rect(bounds.Right - scale, bounds.Top, bounds.Right, bounds.Top + scale), color, 2, 0);
            SpriteBatch.Draw("frame", new Rect(bounds.Left, bounds.Bottom - scale, bounds.Left + scale, bounds.Bottom), color, 0, 2);
            SpriteBatch.Draw("frame", new Rect(bounds.Right - scale, bounds.Bottom - scale, bounds.Right, bounds.Bottom), color, 2, 2);
            //edges
            SpriteBatch.Draw("frame", new Rect(bounds.Left + scale, bounds.Top, bounds.Right - scale, bounds.Top + scale), color, 1, 0);
            SpriteBatch.Draw("frame", new Rect(bounds.Left, bounds.Top + scale, bounds.Left + scale, bounds.Bottom - scale), color, 0, 1);
            SpriteBatch.Draw("frame", new Rect(bounds.Right - scale, bounds.Top + scale, bounds.Right, bounds.Bottom - scale), color, 2, 1);
            SpriteBatch.Draw("frame", new Rect(bounds.Left + scale, bounds.Bottom - scale, bounds.Right - scale, bounds.Bottom), color, 1, 2);*/

            SpriteBatch.Draw(bounds: bounds.SliceTop(shadow), colors: new[] { Color.Black, Color.Black, Color.FromArgb(0, Color.Black), Color.FromArgb(0, Color.Black) });
            SpriteBatch.Draw(bounds: bounds.SliceBottom(shadow), colors: new[] { Color.FromArgb(0, Color.Black), Color.FromArgb(0, Color.Black), Color.Black, Color.Black });
            SpriteBatch.Draw(bounds: bounds.SliceLeft(shadow), colors: new[] { Color.Black,  Color.FromArgb(0, Color.Black), Color.FromArgb(0, Color.Black), Color.Black });
            SpriteBatch.Draw(bounds: bounds.SliceRight(shadow), colors: new[] { Color.FromArgb(0, Color.Black), Color.Black, Color.Black, Color.FromArgb(0, Color.Black) });
            SpriteBatch.Draw(bounds: bounds.SliceTop(thickness), color: color);
            SpriteBatch.Draw(bounds: bounds.SliceBottom(thickness), color: color);
            SpriteBatch.Draw(bounds: bounds.SliceLeft(thickness), color: color);
            SpriteBatch.Draw(bounds: bounds.SliceRight(thickness), color: color);
        }
        /*
        public static void DrawArrowConfetti(float left, float top, float right, float bottom, float size, Color min, Color max, float value)
        {
            left -= size; right += size; top -= size; bottom += size;
            int amount = 100;
            float width = right - left;
            float height = bottom - top;
            float l, t, s;
            for (int i = 0; i < amount; i++)
            {
                s = (149 + i * 491) % (size / 2) + (size / 2);
                l = (461 + i * 397) % (width-s);
                t = (811 + i * 433 + value * s) % (height-s);
                SpriteBatch.Draw("arrow", left + l, top + t, left + l + s, top + t + s, Utils.ColorInterp(min, max, (float)Math.Abs(Math.Sin(value + i * 83))), 0, i % 8, 0);
            }
        }*/

        public static void DrawArc(float x, float y, float r1, float r2, float start, float end, Color c)
        {
            float s = (end - start) / 60;
            for (int i = 0; i < 60; i++)
            {
                SpriteBatch.Draw(coords: new OpenTK.Vector2[] {
                    new OpenTK.Vector2(x + r1 * (float)Math.Cos(start + s*i), y + r1 * (float)Math.Sin(start + s*i)),
                    new OpenTK.Vector2(x + r2 * (float)Math.Cos(start + s*i), y + r2 * (float)Math.Sin(start + s*i)),
                    new OpenTK.Vector2(x + r2 * (float)Math.Cos(start + s + s*i), y + r2 * (float)Math.Sin(start + s + s*i)),
                    new OpenTK.Vector2(x + r1 * (float)Math.Cos(start + s + s*i), y + r1 * (float)Math.Sin(start + s + s*i)),
                }, color: c);
            }
        }

        public static void DrawLoadingAnimation(float scale, float x, float y, float time, int alpha)
        {
            float tx, ty;
            for (int i = 0; i < 6; i++)
            {
                tx = x + scale * 1.2f * (float)Math.Cos(time + i * Math.PI / 3);
                ty = y + scale * 1.2f * (float)Math.Sin(time + i * Math.PI / 3);
                SpriteBatch.DrawRect(new Rect(tx - 10, ty - 10, tx + 10, ty + 10), Color.FromArgb(alpha,Color.Aqua));
            }

            for (int i = 0; i < 6; i++)
            {
                SpriteBatch.Draw(coords: new OpenTK.Vector2[] {
                    new OpenTK.Vector2(x - (0.8f*scale-10) * (float)Math.Cos(time + i * Math.PI / 3), y + (0.8f*scale-10) * (float)Math.Sin(time+i*Math.PI/3)),
                    new OpenTK.Vector2(x - (0.8f*scale) * (float)Math.Cos(time + i * Math.PI / 3) + 10 * (float)Math.Sin(time+i*Math.PI/3), y + (0.8f*scale) * (float)Math.Sin(time+i*Math.PI/3) + 10 * (float)Math.Cos(time+i*Math.PI/3)),
                    new OpenTK.Vector2(x - (0.8f*scale+10) * (float)Math.Cos(time + i * Math.PI / 3), y + (0.8f*scale+10) * (float)Math.Sin(time+i*Math.PI/3)),
                    new OpenTK.Vector2(x - (0.8f*scale) * (float)Math.Cos(time + i * Math.PI / 3) - 10 * (float)Math.Sin(time+i*Math.PI/3), y + (0.8f*scale) * (float)Math.Sin(time+i*Math.PI/3) - 10 * (float)Math.Cos(time+i*Math.PI/3)),
                }, color: Color.FromArgb(alpha, Color.Aqua));
            }

            DrawArc(x, y, scale * 0.5f, scale * 0.55f, time, time + 2 * (float)Math.Sin(time), Color.Aquamarine);
            DrawArc(x, y, scale * 0.5f, scale * 0.55f, time + 3.14f, 3.14f + time + 2 * (float)Math.Sin(time), Color.Aquamarine);

            DrawArc(x, y, scale * 0.95f, scale, -time + 1.57f, 1.57f - time + 2 * (float)Math.Cos(time), Color.Aquamarine);
            DrawArc(x, y, scale * 0.95f, scale, -time + 4.71f, 4.71f - time + 2 * (float)Math.Cos(time), Color.Aquamarine);
        }

        public static void DrawParallelogramWithBG(Rect bounds, float amount, Color fill, Color frame)
        {
            //todo: bounds shrink function
            float h = bounds.Height * 0.5f;
            float t = h * Math.Abs(amount);
            SpriteBatch.ParallelogramTransform(amount, bounds.Top + h);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Create);
            SpriteBatch.DrawRect(new Rect(bounds.Left-t, bounds.Top, bounds.Right+t, bounds.Bottom), Color.White);
            SpriteBatch.DisableTransform();
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Draw);
            Game.Screens.DrawChartBackground(new Rect(bounds.Left - h, bounds.Top, bounds.Right + h, bounds.Bottom), fill, 1.5f);
            SpriteBatch.Stencil(SpriteBatch.StencilMode.Disable);
            SpriteBatch.ParallelogramTransform(amount, bounds.Top + h);
            DrawFrame(new Rect(bounds.Left-t, bounds.Top, bounds.Right+t, bounds.Bottom), 30f, frame);
            SpriteBatch.DisableTransform();
        }
    }
}
