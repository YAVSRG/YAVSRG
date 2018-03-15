using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap.DifficultyRating
{
    public class RatingReport
    {
        static readonly float TIMEEXPONENT = -1.7f; //difficulty inversely proportional to time between each note
        static readonly float SMOOTHEXPONENT = 2f;
        static readonly float TIMESCALE = 1000;
        static readonly float BASESCALE = 50;

        public List<float> raw;
        public float[] breakdown;

        float[] fingers;

        public RatingReport(Chart map, float rate, float hitwindow)
        {
            KeyLayout layout = new KeyLayout(map.Keys);
            int hands = layout.hands.Count;
            fingers = new float[map.Keys];

            raw = new List<float>();
            Snap[] snaps = map.States.Points;
            Snap current;
            List<float> fingersOnHand = new List<float>();
            List<float> handsInSnap = new List<float>();
            Snap.BinarySwitcher s;

            for (int i = 0; i < snaps.Length; i++)
            {
                handsInSnap.Clear();
                for (int h = 0; h < hands; h++)
                {
                    fingersOnHand.Clear();
                    current = snaps[i].Mask(layout.hands[h].Mask());
                    s = new Snap.BinarySwitcher(current.taps.value + current.holds.value);
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
                raw.Add(GetSnapDifficulty(handsInSnap)); //calculate difficulty for hands overall (hand sync and shit idk)
            }
            breakdown = new float[] { DataSet.Mean(raw), 0, 0, 0, 0 }; //final values are meaned because your accuracy is a mean average of hits
            //difficulty of each snap is assumed to be a measure of how unlikely it is you will hit it well
        }

        protected float GetHandDifficulty(List<float> data)
        {
            return DataSet.Mean(data)*data.Count; //temp
        }

        protected float GetSnapDifficulty(List<float> data)
        {
            return DataSet.Mean(data)*6; //not actually that temp
        }

        protected float GetNoteDifficulty(int c, float offset, int h, float rate)
        {
            float val = 0;
            Snap.BinarySwitcher s = new Snap.BinarySwitcher(h);
            s.RemoveColumn(c);
            float delta1;
            if (fingers[c] > 0) //if this is not the first note in this column in the map
            {
                delta1 = (offset - fingers[c]) / rate;
                val += GetJackCurve(delta1); //add base jack value
            }
            else //if it is, this is some temp fix
            {
                delta1 = 1000;
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
            float heightScale = 10f;
            float curveExponent = 1.1f;
            float cutoffExponent = 10f;
            return (float)Math.Max((1.1f * heightScale / Math.Pow(widthScale * delta, curveExponent) - 0.1 * heightScale / Math.Pow(widthScale * delta, curveExponent * cutoffExponent)), 0);
        }

        protected float GetJackCurve(float delta) //how hard is it to hit these two notes in the same column? closer = exponentially harder
        {
            float widthScale = 0.05f;
            float heightScale = 10f;
            float curveExponent = 1.1f;
            return (float)(0.7f * heightScale / Math.Pow(widthScale * delta, curveExponent));
        }
    }
}