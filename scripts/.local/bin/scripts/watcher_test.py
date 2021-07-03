#!/usr/bin/env python3
import psutil
import datetime
import time
from pprint import pprint
from datetime import datetime

badProcName = "say-hello"
# badProcName = "say-hello"
badProcAllowedSecs = 10
processes = {}

for p in psutil.process_iter(['name', 'username']):
    if p.name() in processes:
        processes[p.name()].append(p)
    else:
        processes[p.name()] = [p]

for p in processes:
    pprint(processes[p][0].name())




# badProcName = "VBCSCompiler"
# badProcAllowedSecs = 60

badProcName = "say-hello"
badProcAllowedSecs = 60
processes = {}
for p in psutil.process_iter(['name', 'username']):
    if p.name() in processes:
        processes[p.name()].append(p)
    else:
        processes[p.name()] = [p]

# if badProcName in processes:
#     procs = processes[badProcName]
#     for proc in procs:
#         pprint(proc)
#         pprint("seconds up: {}".format(int(time.time() - proc.create_time())))
#         if time.time() - proc.create_time() > badProcAllowedSecs:
#             print("Killing {}, seconds up: {}, pid: {}".format(proc.name(),int(time.time() - proc.create_time()),proc.pid))
#             proc.kill()




# for name in processes:
#     count = 0
#     for proc in processes[name]:
#         count = count + 1
#         if count > 1:
#             print("duplicate proc:")
#             pprint(proc.name())
