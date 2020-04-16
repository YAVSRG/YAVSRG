using Interlude.Gameplay;
using Interlude.Interface.Widgets;
using Interlude.Interface.Widgets.Editor;

namespace Interlude.Interface.Screens
{
    public class ScreenEditor : Screen
    {
        public ScreenEditor()
        {
            AddChild(new NoteRenderer(Game.Gameplay.ModifiedChart).Reposition(0, 0, 0, 0, -100, 1, 0, 1));
            AddChild(new Timeline().Reposition(0, 0, -20, 1, 0, 1, 0, 1));
            AddChild(new SpriteButton("buttonclose", () => { Game.Screens.PopScreen(); }, () => Game.Options.General.Hotkeys.Exit) { Tooltip = "Exit editor" }.Reposition(-80, 1, 0, 0, 0, 1, 80, 0));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Screens.Toolbar.SetState(WidgetState.DISABLED);
            Game.Audio.OnPlaybackFinish = Game.Audio.Stop;
            Game.Screens.Toolbar.Icons.Filter(0b00000001);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Screens.Toolbar.SetState(WidgetState.ACTIVE);
        }
    }
}
