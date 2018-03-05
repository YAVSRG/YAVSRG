using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class HitMeter : Widget
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
            int tier;
            float when;

            public JudgementDisplay()
            {
                tier = 0;
                when = -10000;
            }

            public void Draw(float left, float top, float right, float bottom, float now)
            {
                if (now-when < 200)
                {
                    float x = Math.Abs(1 - (now - when) * 0.01f) * (right-left)*0.2f;
                    SpriteBatch.DrawCentredTextToFill(Game.Options.Theme.Judges[tier], left + x, top, right - x, bottom, Game.Options.Theme.JudgeColors[tier]);
                }
            }

            public void NewHit(int tier, float now)
            {
                if ((now - when >= 200 || tier > this.tier) && (tier > 0 || Game.Options.Theme.JudgementShowMarv))
                {
                    this.tier = tier;
                    when = now;
                }
            }
        }

        JudgementDisplay[] disp;
        List<Hit> hits;

        public HitMeter(int keys) : base()
        {
            if (Game.Options.Theme.JudgementPerColumn)
            {
                disp = new JudgementDisplay[keys];
                for (int k = 0; k < keys; k++)
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

        public void AddHit(int k, float delta, float now, int tier)
        {
            if (Game.Options.Theme.JudgementPerColumn)
            {
                disp[k].NewHit(tier, now);
            }
            else
            {
                disp[0].NewHit(tier, now);
            }
            hits.Add(new Hit(now,delta,tier));
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
                SpriteBatch.DrawRect(h.delta * 4 - 4, -20, h.delta * 4 + 4, 20, System.Drawing.Color.FromArgb((int)(4+(5000+h.time-now)*0.05f),Game.Options.Theme.JudgeColors[h.tier]));
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
