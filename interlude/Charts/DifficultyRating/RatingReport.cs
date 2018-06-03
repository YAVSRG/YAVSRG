using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Charts.DifficultyRating
{
    public class RatingReport
    {
        public float Physical;
        public float Technical;
        public float[] PhysicalData;
        public float[] TechnicalData;
        //public float[] physical, tech;
        //public float[] breakdown;

        float[] fingers;

        public RatingReport(ChartWithModifiers map, float rate)
        {
            KeyLayout layout = new KeyLayout(map.Keys);
            int hands = layout.hands.Count;
            fingers = new float[map.Keys];

            PhysicalData = new float[map.Notes.Points.Count];
            TechnicalData = new float[map.Notes.Points.Count];
            List<GameplaySnap> snaps = map.Notes.Points;
            Snap current;
            BinarySwitcher s;

            float CurrentStrain = 0;
            float now = 0;
            float delta;

            for (int i = 0; i < snaps.Count; i++)
            {
                if (snaps[i].taps.value + snaps[i].holds.value == 0) { continue; }

                //PHYSICAL ----
                for (int h = 0; h < hands; h++)
                {
                    current = snaps[i].Mask(layout.hands[h].Mask()); //bit mask to only look at notes corresponding to this hand
                    s = new BinarySwitcher(current.taps.value + current.holds.value); //s = consider button presses
                    delta = (current.Offset - now) / rate;
                    foreach (byte k in s.GetColumns()) //calculate value for each note and put it through overall algorithm
                    {
                        UpdateStrain(ref CurrentStrain, delta, GetNoteDifficulty(k, current.Offset, layout.hands[h].Mask(), rate));
                        delta = 0;
                    }
                    foreach (byte k in s.GetColumns()) //record times for calculation of the next snap
                    {
                        fingers[k] = current.Offset;
                    }
                }
                PhysicalData[i] = CurrentStrain; //calculate difficulty for hands overall (hand sync and shit idk)
                //         ----

                //TECH ----
                //temp algorithm
                TechnicalData[i] = GetStreamCurve((snaps[i].Offset - now) / rate);
                //    ----
                now = snaps[i].Offset; //record this time also, time between notes (used for both physical and technical)
            }
            Physical = GetOverallDifficulty(PhysicalData);
            Technical = GetOverallDifficulty(TechnicalData); //final values are meaned because your accuracy is a mean average of hits
            //difficulty of each snap is assumed to be a measure of how unlikely it is you will hit it well
        }

        protected void UpdateStrain(ref float result, float time, float value)
        {
            double decay = -0.005;
            float exponent = 10f; //this needs rewriting at SOON date
            result *= (float)Math.Exp(decay * time); //decay value over time
            result = (float)Math.Pow(Math.Pow(result, exponent) + Math.Pow(value, exponent), 1 / exponent); //bump up value according to new value
        }

        protected float GetOverallDifficulty(float[] data)
        { 
            //todo: stop ignoring 0s
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

        protected float GetNoteDifficulty(byte c, float offset, int h, float rate)
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
            foreach (byte k in s.GetColumns()) //for all other columns on this hand
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
            float widthScale = 0.02f;
            float heightScale = 10f;
            float curveExponent = 1f;
            float cutoffExponent = 10f;
            return (float)Math.Max((1f * heightScale / Math.Pow(widthScale * delta, curveExponent) - 0.1 * heightScale / Math.Pow(widthScale * delta, curveExponent * cutoffExponent)), 0);
        }

        protected float GetJackCurve(float delta) //how hard is it to hit these two notes in the same column? closer = exponentially harder
        {
            float widthScale = 0.02f;
            float heightScale = 10f;
            float curveExponent = 1f;
            return (float)Math.Min(1.7f * heightScale / Math.Pow(widthScale * delta, curveExponent), 20);
        }
    }
}