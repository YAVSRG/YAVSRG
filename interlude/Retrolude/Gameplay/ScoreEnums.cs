using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Gameplay
{
    public enum ScoreSavingPreference
    {
        ALWAYS,
        PASS,
        PERSONALBEST,
        PACEMAKER
    }

    public enum HPFailType
    {
        NOFAIL,
        INSTANT,
        END_OF_SONG
    }
}
