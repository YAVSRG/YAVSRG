using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Threading;

namespace YAVSRG.Utilities
{
    public class TaskManager
    {
        CancellationTokenSource token;

        public class NamedTask
        {
            Task t;
            string name;

            public NamedTask(Task t, string name)
            {
                this.t = t;
                this.name = name;
            }

            public void Start()
            {
                t.Start();
            }

            public TaskStatus Status
            {
                
                get { return t.Status; }
            }
        }

        public List<NamedTask> tasks;

        public TaskManager()
        {
            tasks = new List<NamedTask>();
            token = new CancellationTokenSource();
        }

        public void Stop()
        {
            token.Cancel();
        }

        public NamedTask AddTask(Action a, string name)
        {
            NamedTask t = new NamedTask(new Task(a, token.Token), name);
            tasks.Add(t);
            t.Start();
            return t;
        }
    }
}
