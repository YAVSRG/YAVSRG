using System;
using System.Collections.Generic;
using System.Drawing;

namespace Prelude.Gameplay.DifficultyRating
{
    public class CalcUtils
    {
        public static void StaminaAlgorithm(ref double currentValue, double input, float timedelta)
        {
            currentValue *= StaminaDecayFunc(timedelta);
            currentValue = Math.Max(currentValue, 0.001);
            double r = input / currentValue;
            currentValue *= StaminaBaseFunc(r);
        }

        public static double StaminaBaseFunc(double ratio)
        {
            return (1 + 0.1f * ratio);
        }

        public static double StaminaDecayFunc(float timedelta)
        {
            return Math.Exp(-0.0004 * timedelta);
        }

        public static void UpdateStrain(ref double result, double value, float timeDelta)
        {
            StaminaAlgorithm(ref result, value, timeDelta);
        }

        public static float GetOverallDifficulty(double[] data)
        {
            double v = 0.01;
            foreach (double d in data)
            {
                v *= Math.Exp(0.01 * Math.Max(0, Math.Log(d / v)));
            }
            return (float)Math.Pow(v,0.6)*2.5f;
        }

        public static double ConfidenceValue(double delta)
        {
            double deviation = Math.Max(2, Math.Abs(delta));
            return Phi((17.95 - deviation) / 15);
        }

        public static double Phi(double value) //en.wikipedia.org/wiki/Normal_distribution#Cumulative_distribution_function
        {
            value /= 1.414213562f;
            return Math.Max(0, Math.Min((0.5f + Math.Pow(Math.PI, -0.5) * (value - Math.Pow(value, 3) / 3 + Math.Pow(value, 5) / 10 - Math.Pow(value, 7) / 42 + Math.Pow(value, 9) / 216)), 1));
        }

        public static double RootMeanPower(List<double> data, float power) //powers items of a list by a power. finds the mean and then roots the mean by the power. used for calc.
        {
            if (data.Count == 0) { return 0; }
            if (data.Count == 1) { return data[0]; };
            double f = 0;
            foreach (float v in data)
            {
                f += Math.Pow(v, power);
            }
            return Math.Pow(f / data.Count, 1f / power);
        }

        public static Color PhysicalColor(float val)
        {
            try
            {
                float a = Math.Min(1, val * 0.1f);
                float b = Math.Max(1, val * 0.1f) - 1;
                return Color.FromArgb((int)(255 * a), (int)(255 * (1 - a)), (int)(255 * b));
            }
            catch
            {
                return Color.Red;
            }
        }

        public static Color TechnicalColor(float val)
        {
            try
            {
                float a = Math.Min(1, val * 0.1f);
                float b = Math.Max(1, val * 0.1f) - 1;
                return Color.FromArgb((int)(255 * (1 - a)), (int)(255 * b), (int)(255 * a));
            }
            catch
            {
                return Color.Blue;
            }
        }
    }
}
