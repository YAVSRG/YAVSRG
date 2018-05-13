using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Beatmap.DifficultyRating
{
    public class RatingReport
    {
        public float[] physical,tech;
        public float[] breakdown;

        float[] fingers;

        public RatingReport(ChartWithModifiers map, float rate, float hitwindow)
        {
            KeyLayout layout = new KeyLayout(map.Keys);
            int hands = layout.hands.Count;
            fingers = new float[map.Keys];

            physical = new float[map.Notes.Points.Count];
            tech = new float[map.Notes.Points.Count];
            List<GameplaySnap> snaps = map.Notes.Points;
            Snap current;
            List<float> fingersOnHand = new List<float>();
            List<float> handsInSnap = new List<float>();
            BinarySwitcher s;

            for (int i = 0; i < snaps.Count; i++)
            {
                if (snaps[i].Count == 0) { continue; }
                handsInSnap.Clear();
                for (int h = 0; h < hands; h++)
                {
                    fingersOnHand.Clear();
                    current = snaps[i].Mask(layout.hands[h].Mask());
                    s = new BinarySwitcher(current.taps.value + current.holds.value);
                    foreach (int k in s.GetColumns())
                    {
                        fingersOnHand.Add(GetNoteDifficulty(k, current.Offset, layout.hands[h].Mask(), rate)); //collect together difficulty of each note on each hand
                    }
                    foreach (int k in s.GetColumns())
                    {
                        fingers[k] = current.Offset;
                    }
                    handsInSnap.Add(GetHandDifficulty(fingersOnHand)); //calculate difficulty for this hand
                }
                physical[i] = GetSnapDifficulty(handsInSnap); //calculate difficulty for hands overall (hand sync and shit idk)

                if (i > 1) //temp tech algorithm
                {
                    float delta1 = snaps[i].Offset - snaps[i - 1].Offset;
                    float delta2 = snaps[i - 1].Offset - snaps[i - 2].Offset;
                    tech[i] = (float)Math.Abs(Math.Log(delta1 / delta2, 2))*GetStreamCurve(delta1)*rate*20;
                }
            }
            breakdown = new float[] { GetOverallDifficulty(physical), GetOverallDifficulty(tech)}; //final values are meaned because your accuracy is a mean average of hits
            //difficulty of each snap is assumed to be a measure of how unlikely it is you will hit it well
        }

        protected float GetOverallDifficulty(float[] data)
        {
            int c = 0;
            float t = 0;
            for (int i = 0; i < data.Length; i++)
            {
                if (data[i] > 0)
                {
                    c += 1;
                }
                t += data[i];
            }
            return t / c;
        }

        protected float GetSnapDifficulty(List<float> data)
        {
            return DataSet.Mean(data)*9; //not actually that temp
        }

        protected float GetHandDifficulty(List<float> data)
        {
            return DataSet.Mean(data); //temp
        }

        protected float GetNoteDifficulty(int c, float offset, int h, float rate)
        {
            float val = 0;
            BinarySwitcher s = new BinarySwitcher(h);
            s.RemoveColumn(c);
            float delta1;
            if (fingers[c] > 0) //if this is not the first note in this column in the map
            {
                delta1 = (offset - fingers[c]) / rate;
                val += GetJackCurve(delta1); //add base jack value
            }
            else //if it is, this is some temp fix
            {
                delta1 = 10000;
            }
            foreach (int k in s.GetColumns()) //for all other columns on this hand
            {
                if (fingers[k] > 0) //if this is not the first note in this column in the map
                {
                    float delta2 = (offset - fingers[k]) / rate;
                    val += GetStreamCurve(delta2) * GetJackCompensation(delta1, delta2); //add trill part where applicable 
                }
            }
            return val;
        }

        protected float GetJackCompensation(float jackdelta, float streamdelta) //jumpjacks do not involve you swapping your fingers over to hit them, therefore ignore the stream part of a calc when there is a jack
        {
            float n = jackdelta / streamdelta;
            return (float)Math.Min(Math.Max(Math.Log(n, 2), 0), 1);
        }

        protected float GetStreamCurve(float delta) //how hard is it to hit these two adjacent notes? when they are VERY close together you can hit them at the same time so no difficulty added
        {
            float widthScale = 0.05f;
            float heightScale = 5f;
            float curveExponent = 1.1f;
            float cutoffExponent = 10f;
            return (float)Math.Max((1.1f * heightScale / Math.Pow(widthScale * delta, curveExponent) - 0.1 * heightScale / Math.Pow(widthScale * delta, curveExponent * cutoffExponent)), 0);
        }

        protected float GetJackCurve(float delta) //how hard is it to hit these two notes in the same column? closer = exponentially harder
        {
            float widthScale = 0.05f;
            float heightScale = 10f;
            float curveExponent = 1.1f;
            return (float)(heightScale / Math.Pow(widthScale * delta, curveExponent));
        }
    }
}