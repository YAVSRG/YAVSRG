using System;
using Prelude.Gameplay.Watchers;
using Prelude.Utilities;

namespace Interlude.Interface.Widgets
{
    public class AccuracySystemSelector : FlowContainer
    {
        public struct AccuracySystem
        {
            public ScoreSystem.ScoreType Type;
            public DataGroup Data;

            public AccuracySystem(ScoreSystem.ScoreType type, DataGroup data)
            {
                Data = data;
                Type = type;
            }
        }

        class SelectableCard : Widget
        {
            int i;
        }

        public AccuracySystemSelector(Func<int> GetSelected, Action<int> OnSelect, Action OnCreate, Action<int> OnModify, Action<int> OnDelete)
        {
            AddChild(new SpriteButton("buttonimport", "Add", () => { }));
            for (int i = 0; i < Game.Options.Profile.ScoreSystems.Count; i++)
            {
                //AddChild(new SelectableCard(Game.Options.Profile.GetScoreSystem(i).Name, Game.Options.Profile.ScoreSystems[i]));
            }
        }
    }
}
