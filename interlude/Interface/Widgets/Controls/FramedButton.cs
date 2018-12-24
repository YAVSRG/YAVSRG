using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    class FramedButton : FrameContainer
    {
        Action OnClick;
        Animations.AnimationColorMixer Fill, Border;
        public Func<bool> Highlight = () => false;

        public FramedButton(string label, Action onClick)
        {
            OnClick = onClick;
            Animation.Add(Fill = new Animations.AnimationColorMixer(Game.Screens.DarkColor));
            Animation.Add(Border = new Animations.AnimationColorMixer(Game.Screens.HighlightColor));
            FrameColor = () => Border;
            BackColor = () => Fill;
            AddChild(new TextBox(() => label, AnchorType.CENTER, 0, true, () => Game.Options.Theme.MenuFont, BackColor));
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (ScreenUtils.MouseOver(GetBounds(bounds)))
            {
                Fill.Target(Game.Screens.BaseColor);
                Border.Target(Game.Options.Theme.MenuFont);
                if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                {
                    Game.Audio.PlaySFX("click");
                    OnClick();
                }
            }
            else
            {
                Fill.Target(Highlight() ? Game.Screens.BaseColor : Game.Screens.DarkColor);
                Border.Target(Game.Screens.HighlightColor);
            }
        }
    }
}
