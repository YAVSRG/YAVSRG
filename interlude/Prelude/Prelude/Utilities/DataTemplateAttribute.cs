using System;
using System.Reflection;

namespace Prelude.Utilities
{
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = true, Inherited = true)]
    public class DataTemplateAttribute : Attribute
    {
        public readonly string Name;
        public readonly DataGroup Properties;

        public DataTemplateAttribute(string name, params object[] data)
        {
            Name = name;
            Properties = new DataGroup();
            for (int i = 0; i < data.Length / 2; i++)
            {
                Properties.Add((string)data[i * 2], data[i * 2 + 1]);
            }
        }

        public static DataTemplateAttribute[] GetAttributes(Type t)
        {
            return (DataTemplateAttribute[])GetCustomAttributes(t, typeof(DataTemplateAttribute));
        }
    }
}
