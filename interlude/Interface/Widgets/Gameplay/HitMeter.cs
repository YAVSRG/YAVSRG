using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class HitMeter : GameplayWidget
    {
        struct Hit
        {
            public float time;
            public float delta;
            public int tier;

            public Hit(float t, float d, int j)
            {
                time = t;
                delta = d;
                tier = j;
            }
        }

        class JudgementDisplay
        {
            Hit h;

            public JudgementDisplay()
            {
                h = new Hit(-10000, 0, 0);
            }

            public void Draw(Rect bounds, float now)
            {
                SpriteBatch.Draw("judgements", bounds, Color.FromArgb(Alpha((now - h.time) * 2), Color.White), h.delta < 0 ? 0 : 1, h.tier);
            }

            public void NewHit(Hit newhit)
            {
                if ((newhit.time - h.time >= 200 || newhit.tier > h.tier) && (newhit.tier > 0 || Game.Options.Theme.JudgementShowMarv))
                {
                    h = newhit;
                }
            }
        }

        JudgementDisplay[] disp;
        List<Hit> hits;
        float aspectRatio; //used to scale judgement text correctly
        float hScale, vScale;
        int thickness;

        public HitMeter(YAVSRG.Gameplay.ScoreTracker st, Options.WidgetPosition pos) : base(st, pos)
        {
            st.OnHit += AddHit;
            if (Game.Options.Theme.JudgementPerColumn)
            {
                disp = new JudgementDisplay[st.Chart.Keys];
                for (int k = 0; k < st.Chart.Keys; k++)
                {
                    disp[k] = new JudgementDisplay();
                }
            }
            else
            {
                disp = new JudgementDisplay[1];
                disp[0] = new JudgementDisplay();
            }
            hits = new List<Hit>();
            Sprite sprite = Content.GetTexture("judgements");
            aspectRatio = (float)sprite.Height / sprite.UV_Y / ((float)sprite.Width / sprite.UV_X);
            hScale = pos.GetValue("HitHorizontalScale", 3f) * Game.Options.Theme.ColumnWidth / st.Scoring.MissWindow;
            vScale = pos.GetValue("HitVerticalScale", 0.25f) * Game.Options.Theme.ColumnWidth;
            thickness = pos.GetValue("HitThickness", 4);
        }

        private void AddHit(int k, int tier, float delta)
        {
            float now = (float)Game.Audio.Now();
            Hit h = new Hit(now, delta, tier);
            if (Game.Options.Theme.JudgementPerColumn)
            {
                disp[k].NewHit(h);
            }
            else
            {
                disp[0].NewHit(h);
            }
            hits.Add(h);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            //todo: rewrite to fit to bounds

            float now = (float)Game.Audio.Now();

            if (Game.Options.Theme.JudgementPerColumn)
            {
                for (int i = 0; i < disp.Length; i++)
                {
                    disp[i].Draw(new Rect(bounds.Left + i * Game.Options.Theme.ColumnWidth, bounds.Top, bounds.Left + (i + 1) * Game.Options.Theme.ColumnWidth, bounds.Top + Game.Options.Theme.ColumnWidth * aspectRatio), now);
                }
            }
            else
            {
                //slice
                disp[0].Draw(new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + bounds.Width * aspectRatio), now);
            }

            float c = bounds.CenterX;
            foreach (Hit h in hits)
            {
                SpriteBatch.DrawRect(new Rect(c + h.delta * hScale - thickness, bounds.Bottom - vScale, c + h.delta * hScale + thickness, bounds.Bottom), Color.FromArgb(Alpha(now - h.time), Game.Options.Theme.JudgeColors[h.tier]));
            }
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            float now = (float)Game.Audio.Now();
            List<Hit> temp = new List<Hit>();
            foreach (Hit h in hits) //todo: optimise this
            {
                if (now - h.time > Game.Options.Theme.JudgementFadeTime)
                {
                    temp.Add(h);
                }
            }

            foreach (Hit h in temp)
            {
                hits.Remove(h);
            }
        }

        static int Alpha(float delta)
        {
            return (int)Math.Max(0,Math.Min(255,(255f*(1 - delta / Game.Options.Theme.JudgementFadeTime))));
        }
    }
}
