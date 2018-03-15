using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap.DifficultyRating
{
    public class DataSet
    {
        public List<float> data = new List<float>();
        private int n;

        public DataSet()
        {
            n = 0;
        }

        public void AddValue(float v)
        {
            data.Add(v);
        }

        public void CountEmptyValue()
        {
            n += 1;
        }

        public float GetMean()
        {
            int c = data.Count;
            if (c < 3) { return 0; }
            float s = 0;
            for (int i = 0; i < c - 2; i++)
            {
                s += CalcThing(data[i], data[i + 1], data[i + 2]);
            }
            return s/(c-2);
        }

        private static float CalcThing(float a, float b, float c)
        {
            float pow = 0.5f;
            return (float)Math.Pow((Math.Pow(a, pow) + Math.Pow(b, pow) + Math.Pow(c, pow))/3f, 1f / pow);

        }

        public float GetEffectiveMean()
        {
            return Sum() / (data.Count - n + 1);
        }

        private float Sum()
        {
            float total = 0f;
            foreach (float f in data)
            {
                total += f;
            }
            return total;
        }

        public static float Mean(List<float> data)
        {
            float t = 0;
            if (data.Count == 0) { return 0; }
            foreach (float f in data)
            {
                t += f;
            }
            return t / (data.Count);
        }

        public static List<float> Smooth(List<float> data, float smooth)
        {
            int c = data.Count;
            if (c < 3) { return data; }
            List<float> result = new List<float>();
            for (int i = 0; i < c - 2; i++)
            {
                result.Add(CalcThing(data[i], data[i + 1], data[i + 2]));
            }
            return result;
        }

        public static List<float> Combine(List<float>[] data, float smooth)
        {
            List<float> result = new List<float>(); 
            int c = data[0].Count;
            float x;
            for (int i = 0; i < c; i++)
            {
                x = 0;
                for (int h = 0; h < data.Length; h++)
                {
                    x += data[h][i];
                }
                result.Add((float)Math.Pow(x, smooth));
            }
            return result;
        }
        public static List<float> Combine(List<float>[,] data, int index, float smooth)
        {
            List<float> result = new List<float>();
            int c = data[0,index].Count;
            float x;
            for (int i = 0; i < c; i++)
            {
                x = 0;
                for (int h = 0; h < data.GetLength(0); h++)
                {
                    x += data[h,index][i];
                }
                result.Add((float)Math.Pow(x, smooth));
            }
            return result;
        }

        public static List<float> Combine(List<float> a, List<float> b,  List<float> c, float smooth)
        {
            List<float> result = new List<float>();
            int t = a.Count;
            for (int i = 0; i < t; i++)
            {
                result.Add((float)Math.Pow(a[i] + b[i] + c[i], smooth));
            }
            return result;
        }
    }
}
