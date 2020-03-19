using System;
using System.Collections.Generic;

namespace Prelude.Gameplay.Charts.YAVSRG
{
    //Manages an ordered list of objects that contain a time into the chart when they occur
    public class PointManager<P> where P : OffsetItem
    {
        public List<P> Points;
        public int Count;
        public PointManager() : this(new List<P>()) { }

        public PointManager(List<P> data) { Points = data; Count = data.Count; }

        //Indexer for points by index
        public P this[int index]
        {
            get { return Points[index]; }
        }

        //Indexer for points by time
        public P this[float offset]
        {
            get { return GetPointAt(offset, true); }
        }

        //Gets the most recent point from a given time
        //If interpolate is true and the time does not exactly match a point, generate a new one with inheriting properties from the previous
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

        //Gets the index of the last point that appeared before a given time (including a point exactly at this time)
        public int GetLastIndex(float offset)
        {
            return (int)(GetInterpolatedIndex(offset));
        }

        //Gets the index of the next point that appears after a given time (ignores a point exactly at this time)
        public int GetNextIndex(float offset)
        {
            return (int)Math.Ceiling(GetInterpolatedIndex(offset) + 0.1f);
        }

        //Adds a new point to the end of the 
        public void AppendPoint(P point)
        {
            Points.Add(point);
            Count += 1;
        }

        //Inserts a timing point in its correct place
        public void InsertPoint(P point)
        {
            float x = GetInterpolatedIndex(point.Offset);
            int i = (int)x;
            if (i != x)
            {
                Points.Insert(i + 1, point);
                Count += 1;
            }
            Points[i] = point;
        }

        //Gets the index at a particular time, or the index of the most recent point + 0.5 if not exactly matching a point's time
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
