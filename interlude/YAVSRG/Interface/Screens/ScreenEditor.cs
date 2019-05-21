using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Screens
{
    public class ScreenEditor : Screen
    {
        public ScreenEditor()
        {
            AddChild(new Gameplay.NoteRenderer(Game.Gameplay.ModifiedChart, new Gameplay.Mods.Visual.DownScroll(ScreenUtils.Bounds,Game.Gameplay.ModifiedChart.Keys)).TL_DeprecateMe(0, 0, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(500, 0, AnchorType.MAX, AnchorType.MAX));
            AddChild(new Widgets.Editor.Timeline().TL_DeprecateMe(0, 50, AnchorType.MIN, AnchorType.MAX).BR_DeprecateMe(500, 0, AnchorType.MAX, AnchorType.MAX));
            AddChild(new Widgets.SpriteButton("buttonclose", "Exit", () => { Game.Screens.PopScreen(); }).TL_DeprecateMe(500, 0, AnchorType.MAX, AnchorType.MIN));
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
