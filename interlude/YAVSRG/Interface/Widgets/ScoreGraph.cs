using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using OpenTK;
using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    class ScoreGraph : Widget
    {
        FBO fbo;
        ScoreInfoProvider data;

        public ScoreGraph(ScoreInfoProvider scoreData)
        {
            data = scoreData;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (fbo == null)
            {
                Redraw(bounds.Width, bounds.Height);
            }
            float x = bounds.Width / ScreenUtils.ScreenWidth * 0.5f;
            float y = bounds.Height / ScreenUtils.ScreenHeight * 0.5f;
            SpriteBatch.Draw(new RenderTarget(fbo, bounds, Color.White, new Vector2(0, 0), new Vector2(x, 0), new Vector2(x, y), new Vector2(0, y)));
        }

        public void RequestRedraw()
        {
            fbo?.Dispose();
            fbo = null;
        }

        void Redraw(float width, float height)
        {
            fbo = FBO.FromPool();
            int snapcount = data.HitData.Length;
            var bounds = new Rect(-ScreenUtils.ScreenWidth, -ScreenUtils.ScreenHeight, -ScreenUtils.ScreenWidth + width, -ScreenUtils.ScreenHeight + height);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(150, 0, 0, 0));
            float w = (width - 10) / snapcount;
            float middle = -ScreenUtils.ScreenHeight + height * 0.5f;
            SpriteBatch.DrawRect(new Rect(-ScreenUtils.ScreenWidth, middle - 2, -ScreenUtils.ScreenWidth + width, middle + 2), Color.Green);
            int j;
            float o;
            float scale = (height - 20) * 0.5f / data.ScoreSystem.MissWindow;
            float x = -ScreenUtils.ScreenWidth + 5;
            for (int i = 0; i < snapcount; i++)
            {
                for (int k = 0; k < data.HitData[i].hit.Length; k++)
                {
                    if (data.HitData[i].hit[k] > 0)
                    {
                        o = data.HitData[i].delta[k];
                        j = data.ScoreSystem.JudgeHit(Math.Abs(o));
                        if (j >= data.ScoreSystem.ComboBreakingJudgement)
                        {
                            SpriteBatch.DrawRect(new Rect(x - 1, -ScreenUtils.ScreenHeight, x + 2, -ScreenUtils.ScreenHeight + height), Color.FromArgb(60, Game.Options.Theme.JudgeColors[5]));
                        }
                        SpriteBatch.DrawRect(new Rect(x - 2, middle - o * scale - 2, x + 3, middle - o * scale + 2), Game.Options.Theme.JudgeColors[j]);
                    }
                }
                x += w;
            }
            ScreenUtils.DrawFrame(bounds, Color.White);
            fbo.Unbind();
        }

        public override void OnResize()
        {
            base.OnResize();
            RequestRedraw();
        }
    }
}
