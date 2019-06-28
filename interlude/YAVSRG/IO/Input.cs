using System.Collections.Generic;
using OpenTK.Input;
using Interlude.Interface;

namespace Interlude.IO
{
    public class Input
    {
        private static List<Key> k;
        private static List<Key> ok;
        private static List<MouseButton> m;
        private static List<MouseButton> om;
        private static InputMethod im;

        public static int MouseX;
        public static int MouseY;
        private static int _MouseScroll;
        public static bool ClickHandled;

        public static void Init()
        {
            k = new List<Key>();
            ok = new List<Key>();
            m = new List<MouseButton>();
            om = new List<MouseButton>();
            
            Game.Instance.MouseDown += MouseDown;
            Game.Instance.MouseUp += MouseUp;
            Game.Instance.KeyDown += KeyDown;
            Game.Instance.KeyUp += KeyUp;
            Game.Instance.MouseMove += MouseMove;
            Game.Instance.MouseWheel += MouseWheel;
        }

        private static void MouseWheel(object sender, MouseWheelEventArgs e)
        {
            _MouseScroll = e.Delta;
        }

        public static int MouseScroll
        {
            get
            {
                var m = _MouseScroll;
                _MouseScroll = 0;
                return m;
            }
        }

        private static void MouseMove(object sender, MouseMoveEventArgs e)
        {
            MouseX = (e.X - Game.Instance.Width / 2) * ScreenUtils.ScreenWidth / (Game.Instance.Width / 2);
            MouseY = (e.Y - Game.Instance.Height / 2) * ScreenUtils.ScreenHeight / (Game.Instance.Height / 2);
        }

        private static void KeyUp(object sender, KeyboardKeyEventArgs e)
        {
            while (k.Contains(e.Key))
            {
                k.Remove(e.Key);
            }
        }

        private static void KeyDown(object sender, KeyboardKeyEventArgs e)
        {
            k.Add(e.Key);
        }

        private static void MouseUp(object sender, MouseButtonEventArgs e)
        {
            while (m.Contains(e.Button))
            {
                m.Remove(e.Button);
            }
        }

        private static void MouseDown(object sender, MouseButtonEventArgs e)
        {
            m.Add(e.Button);
        }

        public static bool MouseClick(MouseButton b)
        {
            if (!ClickHandled && m.Contains(b) && !om.Contains(b))
            {
                ClickHandled = true;
                return true;
            }
            return false;
        }

        public static bool MouseRelease(MouseButton b)
        {
            return (!m.Contains(b) && om.Contains(b));
        }

        public static bool MousePress(MouseButton b)
        {
            return (m.Contains(b));
        }

        public static bool KeyTap(Key b, bool imoverride = false)
        {
            return (k.Contains(b) && !ok.Contains(b) && CheckIMOverride(imoverride));
        }

        public static bool KeyRelease(Key b)
        {
            return (!k.Contains(b) && ok.Contains(b));
        }

        public static bool KeyPress(Key b, bool imoverride = false)
        {
            return (k.Contains(b)) && CheckIMOverride(imoverride);
        }

        static bool CheckIMOverride(bool o)
        {
            return o || (im == null);
        }

        public static void ChangeIM(InputMethod im)
        {
            Input.im?.Dispose();
            Input.im = im;
        }

        public static bool HasIM()
        {
            return im != null;
        }

        public static void Update()
        {
            im?.Update();
            ok = new List<Key>(k);
            om = new List<MouseButton>(m);
            _MouseScroll = 0;
            ClickHandled = false;
        }
    }

    public abstract class Bind
    {
        public abstract bool Tapped(bool overrideTextBox = false);

        public abstract bool Held(bool overrideTextBox = false);
    }

    public class KeyBind : Bind
    {
        public readonly Key key;

        public KeyBind(Key key)
        {
            this.key = key;
        }

        public override bool Held(bool overrideTextBox = false)
        {
            return Input.KeyPress(key, overrideTextBox);
        }

        public override bool Tapped(bool overrideTextBox = false)
        {
            return Input.KeyTap(key, overrideTextBox);
        }

        public override string ToString()
        {
            return key.ToString();
        }
    }

    public class MouseBind : Bind
    {
        public readonly MouseButton button;

        public MouseBind(MouseButton button)
        {
            this.button = button;
        }

        public override bool Held(bool overrideTextBox = false)
        {
            return Input.MousePress(button);
        }

        public override bool Tapped(bool overrideTextBox = false)
        {
            return Input.MouseClick(button);
        }

        public override string ToString()
        {
            return "Mouse"+button.ToString();
        }
    }

    public class AltBind : Bind
    {
        public readonly Bind bind;
        public readonly bool shift;
        public readonly bool ctrl;

        public AltBind(Bind bind, bool shift, bool ctrl)
        {
            this.bind = bind;
            this.shift = shift;
            this.ctrl = ctrl;
        }

        public override bool Held(bool overrideTextBox = false)
        {
            return CheckAltButtons() && bind.Held(overrideTextBox);
        }

        public override bool Tapped(bool overrideTextBox = false)
        {
            return CheckAltButtons() && bind.Tapped(overrideTextBox);
        }

        bool CheckAltButtons()
        {
            return (!ctrl || Input.KeyPress(Key.ControlLeft) || Input.KeyPress(Key.ControlRight)) && (!shift || Input.KeyPress(Key.ShiftLeft) || Input.KeyPress(Key.ShiftRight));
        }

        public override string ToString()
        {
            return (ctrl ? "Ctrl + " : "") + (shift ? "Shift + " : "") + bind.ToString();
        }
    }
}
