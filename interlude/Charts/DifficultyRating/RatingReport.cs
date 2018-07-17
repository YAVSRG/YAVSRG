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

        float[] fingers;

        //this code is subject to so much change that comments may not be complete or up to date, sorry about that

        public RatingReport(ChartWithModifiers map, float rate, string playstyle) //calculates difficulty of a chart to play - i.e how capable you must be to attain an S
        {
            KeyLayout layout = KeyLayout.GetLayout(playstyle, map.Keys);
            int hands = layout.hands.Count;
            fingers = new float[map.Keys];

            PhysicalData = new float[map.Notes.Points.Count];
            TechnicalData = new float[map.Notes.Points.Count];
            List<GameplaySnap> snaps = map.Notes.Points;
            Snap current;
            BinarySwitcher s;
            
            float[] currentStrain = new float[hands];
            float[] lastHandUse = new float[hands];
            List<float> handDiff = new List<float>();
            float delta;

            for (int i = 0; i < snaps.Count; i++)
            {
                if (snaps[i].taps.value + snaps[i].holds.value == 0) { continue; }
                //PHYSICAL ----
                for (int h = 0; h < hands; h++)
                {
                    delta = (snaps[i].Offset - lastHandUse[h]) / rate;
                    current = snaps[i].Mask(layout.hands[h].Mask()); //bit mask to only look at notes corresponding to this hand
                    if (current.IsEmpty()) { continue; }
                    s = new BinarySwitcher(current.taps.value + current.holds.value); //s = consider button presses
                    foreach (byte k in s.GetColumns()) //calculate value for each note and put it through overall algorithm
                    {
                        handDiff.Add(GetNoteDifficulty(k, current.Offset, layout.hands[h].Mask(), rate));
                    }
                    foreach (byte k in s.GetColumns()) //record times for calculation of the next snap
                    {
                        fingers[k] = current.Offset;
                    }
                    UpdateStrain(ref currentStrain[h], delta, GetHandDifficulty(handDiff));
                    handDiff.Clear();
                    lastHandUse[h] = snaps[i].Offset;
                }
                PhysicalData[i] = GetSnapDifficulty(currentStrain.ToList()); //calculate difficulty for hands overall
                //         ----

                //TECH ----
                //temp algorithm
                TechnicalData[i] = GetStreamCurve((snaps[i].Offset - lastHandUse[0]) / rate);
                //    ----
            }
            //Console.WriteLine("["+string.Join(", ",PhysicalData)+"]");
            Physical = GetOverallDifficulty(PhysicalData);
            Technical = GetOverallDifficulty(TechnicalData); //final values are meaned because your accuracy is a mean average of hits
            //difficulty of each snap is assumed to be a measure of how unlikely it is you will hit it well
        }

        protected void UpdateStrain(ref float result, float time, float value)
        {
            double decay = -0.004;
            double weight = Math.Exp(decay * time);
            result = (float)(result * weight + value * (1 - weight));
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
                t += data[i] * data[i];
            }
            return (float)Math.Pow(t / c, 0.5);
        }

        protected float GetHandDifficulty(List<float> data)
        {
            return Utils.RootMeanPower(data, 1);
        }

        protected float GetSnapDifficulty(List<float> data)
        {
            return Utils.RootMeanPower(data, 1);
        }

        protected float GetNoteDifficulty(byte c, float offset, int h, float rate)
        {
            float ohtnerf = 2f;
            double val = 0;
            BinarySwitcher s = new BinarySwitcher(h);
            s.RemoveColumn(c);
            float delta1;
            if (fingers[c] > 0) //if this is not the first note in this column in the map
            {
                delta1 = (offset - fingers[c]) / rate;
                val += Math.Pow(GetJackCurve(delta1),ohtnerf); //add base jack value
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
                    val += Math.Pow(GetStreamCurve(delta2) * GetJackCompensation(delta1, delta2), ohtnerf); //add trill part where applicable 
                }
            }
            return (float)Math.Pow(val, 1 / ohtnerf);
        }

        protected float GetJackCompensation(float jackdelta, float streamdelta) //jumpjacks do not involve you swapping your fingers over to hit them, therefore ignore the stream part of a calc when there is a jack
        {
            float n = jackdelta / streamdelta;
            return (float)Math.Min(Math.Pow(Math.Max(Math.Log(n, 2), 0), 2), 1);
        }

        protected float GetStreamCurve(float delta) //how hard is it to hit these two adjacent notes? when they are VERY close together you can hit them at the same time so no difficulty added
        {
            float widthScale = 0.02f;
            float heightScale = 10f * 1.4f;
            float curveExponent = 1f;
            float cutoffExponent = 10f;
            return (float)Math.Max((heightScale / Math.Pow(widthScale * delta, curveExponent) - 0.1f * heightScale / Math.Pow(widthScale * delta, curveExponent * cutoffExponent)), 0);
        }

        protected float GetJackCurve(float delta) //how hard is it to hit these two notes in the same column? closer = exponentially harder
        {
            float widthScale = 0.02f;
            float heightScale = 10f * 3f;
            float curveExponent = 1f;
            return (float)Math.Min(heightScale / Math.Pow(widthScale * delta, curveExponent), 20);
        }
    }
}