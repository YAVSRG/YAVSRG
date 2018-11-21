using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Utilities
{
    public class Logging
    {
        public static event Action<string, string, LogType> OnLog;

        public enum LogType
        {
            Debug,
            Info,
            Warning,
            Error,
            Critical
        }

        public static void Log(string Main, string Details, LogType Type = LogType.Info)
        {
            OnLog(Main, Details, Type);
        }
    }
}
