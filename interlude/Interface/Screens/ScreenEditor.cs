using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Screens
{
    public class ScreenEditor : Screen
    {
        public ScreenEditor()
        {
            AddChild(new Widgets.Gameplay.Playfield(new Gameplay.ScoreTracker(Game.Gameplay.ModifiedChart)).PositionTopLeft(-400, 0, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(1000, 0, AnchorType.CENTER, AnchorType.MAX));
            AddChild(new Widgets.Editor.Timeline().PositionTopLeft(0, 50, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Screens.Toolbar.Collapse();
            Game.Audio.SetRate(1.0);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Screens.Toolbar.Expand();
        }
    }
}
