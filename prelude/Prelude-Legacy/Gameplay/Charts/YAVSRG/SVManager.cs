using System.Collections.Generic;

namespace Prelude.Gameplay.Charts.YAVSRG
{
    public class SVManager
    {
        public PointManager<SVPoint>[] SV;
        public PointManager<BPMPoint> BPM;

        public SVManager(int keys)
        {
            SV = new PointManager<SVPoint>[keys + 1];
            SetBlankSVData();
        }

        public SVManager(SVManager toClone) //todo: FULL CLONE NOT SOFT CLONE FOR EASIER MOD CODE
        {
            SV = new PointManager<SVPoint>[toClone.SV.Length];
            SetTimingData(toClone.BPM.Points);
            for (int i = 0; i < SV.Length; i++)
            {
                SV[i] = new PointManager<SVPoint>(toClone.SV[i].Points);
            }
        }

        public void SetBlankSVData()
        {
            for (int i = 0; i < SV.Length; i++)
            {
                SV[i] = new PointManager<SVPoint>();
            }
        }

        public void SetTimingData(List<BPMPoint> data)
        {
            BPM = new PointManager<BPMPoint>(data);
        }

        public void SetSVData(int channel, List<SVPoint> data)
        {
            SV[channel + 1] = new PointManager<SVPoint>(data);
        }

        public bool ContainsSV()
        {
            for (int i = 0; i < SV.Length; i++)
            {
                if (SV[i].Count > 0)
                {
                    return true;
                }
            }
            return false;
        }

        public PointManager<SVPoint> this[int index] //-1 is the "main" SV channel, 0 is leftmost column multiplier etc
        {
            get
            {
                return SV[index + 1];
            }
        }
    }
}
