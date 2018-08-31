using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface
{
    class ScreenUtils
    {
        public static int ScreenWidth;

        public static int ScreenHeight;

        public static void UpdateBounds(int Width, int Height)
        {
            ScreenWidth = Width / 2;
            ScreenHeight = Height / 2;
            if (ScreenWidth < 800 || ScreenHeight < 450)
            {
                float r = Math.Max(800f / ScreenWidth, 450f / ScreenHeight);
                ScreenWidth = (int)(ScreenWidth * r);
                ScreenHeight = (int)(ScreenHeight * r);
            }
        }

        public static bool MouseOver(float left, float top, float right, float bottom)
        {
            int mx = Input.MouseX;
            int my = Input.MouseY;
            return (mx > left && mx < right && my > top && my < bottom);
        }

        public static bool CheckButtonClick(float left, float top, float right, float bottom)
        {
            return MouseOver(left, top, right, bottom) && Input.MouseClick(OpenTK.Input.MouseButton.Left);
        }

        public static void DrawGraph(float left, float top, float right, float bottom, Gameplay.ScoreSystem scoring, Gameplay.ScoreTracker.HitData[] data)
        {
            int snapcount = data.Length;
            SpriteBatch.DrawRect(left, top, right, bottom, Color.FromArgb(150, 0, 0, 0));
            float w = (right - left - 10) / snapcount;
            float middle = (top + bottom) * 0.5f;
            float scale = (bottom - top - 20) * 0.5f / scoring.MissWindow;
            SpriteBatch.DrawRect(left, middle - 3, right, middle + 3, Color.Green);
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
                            SpriteBatch.DrawRect(left + i * w + 4, top, left + i * w + 6, bottom, Color.FromArgb(80, Game.Options.Theme.JudgeColors[5]));
                        }
                        SpriteBatch.DrawRect(left + i * w + 3, middle - o * scale - 2, left + i * w + 8, middle - o * scale + 2, Game.Options.Theme.JudgeColors[j]);
                    }
                }
            }
            SpriteBatch.DrawFrame(left, top, right, bottom, 30f, Color.White);
        }

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
        }

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

        public static void DrawLoadingAnimation(float scale, float x, float y, float time)
        {
            float tx, ty;
            for (int i = 0; i < 6; i++)
            {
                tx = x + scale * 1.2f * (float)Math.Cos(time + i * Math.PI / 3);
                ty = y + scale * 1.2f * (float)Math.Sin(time + i * Math.PI / 3);
                SpriteBatch.DrawRect(tx - 10, ty - 10, tx + 10, ty + 10, Color.Aqua);
            }

            for (int i = 0; i < 6; i++)
            {
                SpriteBatch.Draw(coords: new OpenTK.Vector2[] {
                    new OpenTK.Vector2(x - (0.8f*scale-10) * (float)Math.Cos(time + i * Math.PI / 3), y + (0.8f*scale-10) * (float)Math.Sin(time+i*Math.PI/3)),
                    new OpenTK.Vector2(x - (0.8f*scale) * (float)Math.Cos(time + i * Math.PI / 3) + 10 * (float)Math.Sin(time+i*Math.PI/3), y + (0.8f*scale) * (float)Math.Sin(time+i*Math.PI/3) + 10 * (float)Math.Cos(time+i*Math.PI/3)),
                    new OpenTK.Vector2(x - (0.8f*scale+10) * (float)Math.Cos(time + i * Math.PI / 3), y + (0.8f*scale+10) * (float)Math.Sin(time+i*Math.PI/3)),
                    new OpenTK.Vector2(x - (0.8f*scale) * (float)Math.Cos(time + i * Math.PI / 3) - 10 * (float)Math.Sin(time+i*Math.PI/3), y + (0.8f*scale) * (float)Math.Sin(time+i*Math.PI/3) - 10 * (float)Math.Cos(time+i*Math.PI/3)),
                }, color: Color.Aqua);
            }

            DrawArc(x, y, scale * 0.5f, scale * 0.55f, time, time + 2 * (float)Math.Sin(time), Color.Aquamarine);
            DrawArc(x, y, scale * 0.5f, scale * 0.55f, time + 3.14f, 3.14f + time + 2 * (float)Math.Sin(time), Color.Aquamarine);

            DrawArc(x, y, scale * 0.95f, scale, -time + 1.57f, 1.57f - time + 2 * (float)Math.Cos(time), Color.Aquamarine);
            DrawArc(x, y, scale * 0.95f, scale, -time + 4.71f, 4.71f - time + 2 * (float)Math.Cos(time), Color.Aquamarine);
        }

        public static void DrawParallelogramWithBG(float left, float top, float right, float bottom, float amount, Color fill, Color frame)
        {
            float h = (bottom - top) * 0.5f;
            float t = h * Math.Abs(amount);
            SpriteBatch.ParallelogramTransform(amount, top + h);
            SpriteBatch.StencilMode(1);
            SpriteBatch.DrawRect(left-t, top, right+t, bottom, Color.White);
            SpriteBatch.DisableTransform();
            SpriteBatch.StencilMode(2);
            Game.Screens.DrawChartBackground(left - h, top, right + h, bottom, fill, 1.5f);
            SpriteBatch.StencilMode(0);
            SpriteBatch.ParallelogramTransform(amount, top + h);
            SpriteBatch.DrawFrame(left-t, top, right+t, bottom, 30f, frame);
            SpriteBatch.DisableTransform();
        }
    }
}
