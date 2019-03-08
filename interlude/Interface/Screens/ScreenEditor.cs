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
            AddChild(new Gameplay.NoteRenderer(Game.Gameplay.ModifiedChart, new Gameplay.Mods.Visual.ManipScroll(ScreenUtils.Bounds,Game.Gameplay.ModifiedChart.Keys)).PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(500, 0, AnchorType.MAX, AnchorType.MAX));
            AddChild(new Widgets.Editor.Timeline().PositionTopLeft(0, 50, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(500, 0, AnchorType.MAX, AnchorType.MAX));
            AddChild(new Widgets.SpriteButton("buttonclose", "Exit", () => { Game.Screens.PopScreen(); }).PositionTopLeft(500, 0, AnchorType.MAX, AnchorType.MIN));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Screens.Toolbar.SetState(WidgetState.DISABLED);
            Game.Audio.SetRate(1.0);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Screens.Toolbar.SetState(WidgetState.ACTIVE);
        }
    }
}
