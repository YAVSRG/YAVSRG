using Interlude.Interface.Widgets;

namespace Interlude.Interface.Screens
{
    class ScreenOptions : Screen
    {
        private LayoutPanel lp;
        
        public ScreenOptions()
        {
            OnResize();
        }

        public override void OnResize()
        {
            Children.Clear();
            var ib = new InfoBox();
            FlowContainer tabs = new FlowContainer();
            lp = new LayoutPanel(ib);
            tabs.AddChild(new GeneralPanel(ib, lp).BR_DeprecateMe(0, 900, AnchorType.MAX, AnchorType.MIN));
            tabs.AddChild(new GameplayPanel(ib, lp).BR_DeprecateMe(0, 900, AnchorType.MAX, AnchorType.MIN));
            tabs.AddChild(lp.BR_DeprecateMe(0, 900, AnchorType.MAX, AnchorType.MIN));
            tabs.AddChild(new CreditsPanel(ib).BR_DeprecateMe(0, 900, AnchorType.MAX, AnchorType.MIN));
            lp.Refresh();

            AddChild(tabs.TL_DeprecateMe(200, 0, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(200, 0, AnchorType.MAX, AnchorType.MAX));

            AddChild(ScrollButton("General", 0, tabs));
            AddChild(ScrollButton("Gameplay", 1, tabs));
            AddChild(ScrollButton("Layout", 2, tabs));
            AddChild(ScrollButton("???", 3, tabs));
            AddChild(ScrollButton("Credits", 4, tabs));

            AddChild(ib.TL_DeprecateMe(0, 200, AnchorType.MIN, AnchorType.MAX).BR_DeprecateMe(0, 0, AnchorType.MAX, AnchorType.MAX));

        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Screens.Toolbar.Icons.Filter(0b00011011);
        }

        private Widget ScrollButton(string name, int id, FlowContainer container)
        {
            return new FramedButton(name, () => { container.ScrollTo(id); }) { Highlight = () => container.VisibleIndexBottom == id, Frame = 170, HorizontalFade = 50 }.TL_DeprecateMe(id * 0.2f, 80, AnchorType.LERP, AnchorType.MAX).BR_DeprecateMe(0.2f + id * 0.2f, 0, AnchorType.LERP, AnchorType.MAX);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Gameplay.UpdateChart(); //recolor notes based on settings if they've changed
        }
    }
}
