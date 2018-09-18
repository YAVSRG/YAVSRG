using System;
using System.Collections.Generic;

namespace YAVSRG.Charts.YAVSRG
{
    public class PointManager<P> where P : OffsetItem
    {
        public List<P> Points;
        public int Count;
        public PointManager() : this(new List<P>()) { }

        public PointManager(List<P> data) { Points = data; Count = data.Count; }

        public P GetPointAt(float offset, bool interpolate)
        {
            float x = GetInterpolatedIndex(offset);
            int i = (int)x;
            if (interpolate && i != x)
            {
                return (P)Points[i].Interpolate(offset);
            }
            return Points[i];
        }

        public int GetLastIndex(float offset) //or current
        {
            return (int)GetInterpolatedIndex(offset);
        }

        public int GetNextIndex(float offset)
        {
            return (int)Math.Ceiling(GetInterpolatedIndex(offset) + 0.1f);
        }

        public void AppendPoint(P point)
        {
            Points.Add(point);
            Count += 1;
        }
        
        //keep this
        public float GetInterpolatedIndex(float offset)
        {
            if (Count == 0)
            {
                return -1;
            }
            else if (Count == 1) //fix for edge case
            {
                return Points[0].Offset < offset ? 0.5f : 0;
            }
            int low = 0;
            int high = Count - 1;
            int mid = -1;
            float o = 0f;
            while (low <= high)
            {
                mid = (high + low) / 2;
                if (Points[mid].Offset == offset)
                {
                    return mid;
                }
                else if (Points[mid].Offset < offset)
                {
                    low = mid + 1;
                    o = -0.5f;
                }
                else
                {
                    high = mid - 1;
                    o = 0.5f;
                }
            }
            return mid - o;
        }
    }
}
