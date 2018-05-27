using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Utilities
{
    public class Logging
    {
        static Action<string, LogType> LogAction;

        public enum LogType
        {
            Info,
            Warning,
            Error,
            Critical
        }

        public static void SetLogAction(Action<string, LogType> a)
        {
            LogAction = a;
        }

        public static void Log(string s, LogType type = LogType.Info)
        {
            LogAction(s, type);
        }
    }
}
