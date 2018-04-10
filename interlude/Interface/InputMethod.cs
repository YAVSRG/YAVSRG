using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface
{
    public class InputMethod : IDisposable
    {
        Action<string> setter;
        Func<string> getter;
        Action onUpdate;

        public InputMethod(Action<string> setter, Func<string> getter, Action update)
        {
            this.setter = setter;
            this.getter = getter;
            onUpdate = update;
            Game.Instance.KeyPress += HandleKey;
        }

        public void HandleKey(object sender, OpenTK.KeyPressEventArgs e)
        {
            setter(getter() + e.KeyChar);
            onUpdate();
        }

        public void Update()
        {
            if (Input.KeyTap(OpenTK.Input.Key.BackSpace, true) && getter().Length > 0)
            {
                setter(getter().Remove(getter().Length - 1));
                onUpdate();
            }
        }

        public void Dispose()
        {
            Game.Instance.KeyPress -= HandleKey;
        }
    }
}
