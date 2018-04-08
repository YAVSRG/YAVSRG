using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
            Sprite sprite;

            public JudgementDisplay()
            {
                h = new Hit(-10000, 0, 0);
                sprite = Content.LoadTextureFromAssets("judgements");
            }

            public void Draw(float left, float top, float right, float bottom, float now)
            {
                if (now-h.time < 200)
                {
                    float x = Math.Abs(1 - (now - h.time) * 0.01f) * (right-left)*0.2f;
                    //SpriteBatch.Draw(sprite, left, top, right, top + (right-left)*34f/256f, System.Drawing.Color.White, h.delta < 0 ? 1 : 0, h.tier);
                    SpriteBatch.DrawCentredTextToFill(Game.Options.Theme.Judges[h.tier], left + x, top, right - x, bottom, Game.Options.Theme.JudgeColors[h.tier]);
                }
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

        public HitMeter(YAVSRG.Gameplay.ScoreTracker st) : base(st)
        {
            st.OnHit += AddHit;
            if (Game.Options.Theme.JudgementPerColumn)
            {
                disp = new JudgementDisplay[st.c.Keys];
                for (int k = 0; k < st.c.Keys; k++)
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

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);

            float now = (float)Game.Audio.Now();

            if (Game.Options.Theme.JudgementPerColumn)
            {
                for (int i = 0; i < disp.Length; i++)
                {
                    disp[i].Draw(left + i * Game.Options.Theme.ColumnWidth, top, left + (i + 1) * Game.Options.Theme.ColumnWidth, bottom, now);
                }
            }
            else
            {
                disp[0].Draw(-Game.Options.Theme.ColumnWidth, top, Game.Options.Theme.ColumnWidth, bottom, now);
            }

            foreach (Hit h in hits)
            {
                int alpha = (int)((2500 + h.time - now) * 0.1f); //you do not understand how many cancerous crashes this has caused
                alpha = Math.Max(0, Math.Min(alpha, 255)); //so i did this
                SpriteBatch.DrawRect(h.delta * 4 - 4, -20, h.delta * 4 + 4, 20, System.Drawing.Color.FromArgb(alpha,Game.Options.Theme.JudgeColors[h.tier]));
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            float now = (float)Game.Audio.Now();
            List<Hit> temp = new List<Hit>();
            foreach (Hit h in hits)
            {
                if (now - h.time > 5000)
                {
                    temp.Add(h);
                }
            }

            foreach (Hit h in temp)
            {
                hits.Remove(h);
            }
        }
    }
}
