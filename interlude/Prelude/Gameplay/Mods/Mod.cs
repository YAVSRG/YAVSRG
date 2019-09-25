using System;
using System.Reflection;
using System.Linq;
using System.Collections.Generic;
using Prelude.Utilities;

namespace Prelude.Gameplay.Mods
{
    //Represents a modifier that can be applied to a chart
    //Important: It is assumed that the Data object passed is never null - when the config of a mod is null it is not enabled
    public class Mod
    {
        public static Dictionary<string, Mod> AvailableMods = new Dictionary<string, Mod>();

        //Returns true if this mod is applicable to the current chart
        //It can perform checks on the chart itself e.g. check if there are long notes before applying long note mod
        public virtual bool IsApplicable(ChartWithModifiers Chart, DataGroup Data) => true;

        //Applies the mod to the chart (by editing the note data)
        //Also marks the chart as having this mod applied to it
        public virtual void Apply(ChartWithModifiers Chart, DataGroup Data) { }

        //Applies the mod to the score data before the chart is played
        //The score data begins with markers for notes you should hit and the timing you hit a note with
        //Used in auto: sets all notes as hit with perfect accuracy
        //Can be used to disable the need to hit certain notes (e.g. releasing lns or having to avoid mines)
        public virtual void ApplyToHitData(ChartWithModifiers Chart, ref HitData[] HitData, DataGroup Data) { }

        //Gets the name the mod should go by
        public virtual string Name => "Unknown Modifier";

        //Gets the description the mod should have
        public virtual string Description => "No description set";

        //Gets the behaviour the mod should have in terms of score saving
        //0 = save score as usual, 1 = save score but its not suitable for pbs or online uploads or whatever, 2 = dont save
        public virtual int Status => 0;

        //Marks if this mod should appear in the mod select menu
        public virtual bool Visible => false;

        //Creates the default setup for the mod when it is enabled.
        //How this configuration can be modified is given by the attributes of the class
        //todo: once done ill put a specific example of the attribute thing here
        public virtual DataGroup DefaultSettings => new DataGroup();

        static Mod()
        {
            foreach (var modType in Assembly.GetAssembly(typeof(Mod)).GetTypes().Where(t => t.Namespace == "Prelude.Gameplay.Mods"))
            {
                if (modType.Name == "Mod" || modType.Name == "<>c") { continue; }
                //Logging.Log("Loading mod: " + modType.Name, "", Logging.LogType.Debug);
                try
                {
                    AvailableMods.Add(modType.Name, (Mod)Activator.CreateInstance(modType));
                }
                catch (Exception e)
                {
                    Logging.Log("Failed to load mod: " + modType.Name, e.ToString(), Logging.LogType.Error);
                }
            }
        }
    }
}
