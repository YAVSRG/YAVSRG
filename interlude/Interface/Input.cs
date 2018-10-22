using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Input;

namespace YAVSRG.Interface
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
}
