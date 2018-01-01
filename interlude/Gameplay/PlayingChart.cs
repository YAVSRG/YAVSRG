using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;

namespace YAVSRG.Gameplay
{
    public class PlayingChart
    {
        public class ChordCohesion : OffsetItem
        {
            int notes;
            public float[] delta;
            public bool[] hit;
            public int angery;

            public ChordCohesion(float offset, int keycount, int notecount)
            {
                notes = notecount;
                Offset = offset;
                hit = new bool[keycount];
                delta = new float[keycount];
            }
        }

        public Chart c;

        public ChordCohesion[] hitdata;
        private static int[] weight = new int[] { 10, 9, 5, 1, -10, 0 };
        public int[] judgement;
        private float maxscore = 0.001f;
        private float score = 0.001f;
        private int pos;

        public int combo;
        public int maxcombo = -1;

        public PlayingChart(Chart c)
        {
            this.c = c;
            judgement = new int[6];
            int to = c.States.Count;
            hitdata = new ChordCohesion[to];
            for (int i = 0; i < to; i++)
            {
                hitdata[i] = new ChordCohesion(c.States.Points[i].Offset, c.Keys, c.States.Points[i].Count);
            }
        }

        public void AddJudgement(int i)
        {
            judgement[i] += 1;
            score += weight[i];
            maxscore += 10;
        }

        public void ComboBreak()
        {
            if (combo > maxcombo)
            {
                maxcombo = combo;
            }
            combo = 0;
        }

        public float Accuracy()
        {
            return (float)Math.Round(score * 100f / maxscore, 2);
        }

        public void Update(float time, float hitwindow)
        {
            while (pos < hitdata.Length && hitdata[pos].Offset < time)
            {
                Snap s = c.States.Points[pos];
                float[] data = hitdata[pos].delta;
                float t = 0;
                int n = 0;
                foreach (int k in new Snap.BinarySwitcher(s.taps.value + s.holds.value + s.ends.value).GetColumns())
                {
                    if (!hitdata[pos].hit[k])
                    {
                        hitdata[pos].angery += 1;
                    }
                    else
                    {
                        t += Math.Abs(data[k]);
                        n += 1;
                    }
                }
                if (n > 0)
                {
                    float delta = t / n;
                    int score = hitdata[pos].angery;
                    score += Game.Options.Profile.JudgeHit(delta);
                    if (score > 4)
                    {
                        score = 4;
                    }
                    if (hitdata[pos].angery > 0)
                    {
                        ComboBreak();
                    }
                    else
                    {
                        combo += 1;
                    }
                    AddJudgement(score);
                }
                else
                {
                    AddJudgement(5);
                    ComboBreak();
                }
                pos++;
            }
        }
    }
}
