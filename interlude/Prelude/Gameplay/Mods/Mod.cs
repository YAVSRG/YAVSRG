using System;

namespace Prelude.Gameplay.Mods
{
    public class Mod
    {
        public virtual bool IsApplicable(ChartWithModifiers c, string data)
        {
            //can check if applicable to the chart even if enabled
            return true;
        }

        public virtual void Apply(ChartWithModifiers c, string data)
        {
            c.Mods = GetName(data);
        }

        public virtual void ApplyToHitData(ChartWithModifiers c, ref HitData[] hitdata, string data)
        {

        }

        public virtual string GetName(string data) { return "?"; }

        public virtual string GetDescription(string data) { return "No description set"; }

        public virtual int GetStatus(string data) { return 0; } //0 = ranked, 1 = save but not ranked, 2 = do not save

        public virtual string[] Settings { get { return new string[] { }; } }
    }
}
