using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;

namespace YAVSRG.Gameplay.Mods
{
    public class Mod
    {
        public bool Enable;
        public virtual bool IsActive(ChartWithModifiers c)
        {
            //can check if applicable to the chart even if enabled
            return Enable;
        }

        public virtual void Apply(ChartWithModifiers c)
        {

        }

        public virtual string GetName()
        {
            return "?";
        }
    }
}
