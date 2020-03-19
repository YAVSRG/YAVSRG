using System;
using System.Reflection;

namespace Prelude.Utilities
{
    public class SetterGetter<T>
    {
        private Action<T> _set;
        private Func<T> _get;

        public SetterGetter(object obj, string propertyName)
        {
            var prop = obj.GetType().GetField(propertyName);

            _set = (v) => { prop.SetValue(obj, v); };
            _get = () => (T)prop.GetValue(obj);
        }

        public SetterGetter(Action<T> set, Func<T> get)
        {
            _set = set;
            _get = get;
        }

        public void Set(T value)
        {
            _set(value);
            
        }

        public static implicit operator T(SetterGetter<T> s)
        {
            return s._get();
        }
    }
}
