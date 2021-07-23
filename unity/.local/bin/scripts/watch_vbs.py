#!/usr/bin/env python3
import psutil
import datetime
import time
from pprint import pprint
from datetime import datetime

badProcName = "VBCSCompiler"
badProcAllowedSecs = 60 * 2.5
processes = {}
for p in psutil.process_iter(['name', 'username']):
    if p.name() in processes:
        processes[p.name()].append(p)
    else:
        processes[p.name()] = [p]

if badProcName in processes:
    procs = processes[badProcName]
    for proc in procs:
        if time.time() - proc.create_time() > badProcAllowedSecs:
            print("Killing {}, seconds up: {}, pid: {}".format(proc.name(),int(time.time() - proc.create_time()),proc.pid))
            proc.kill()
