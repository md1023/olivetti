from mod_python import apache
import sys
import datetime
import subprocess

current_time = datetime.datetime.now()

def handler(req):
    req.content_type = 'text/plain'
    pwd = subprocess.check_output(["pwd"])
    req.write("hello.py: Hello, World! >\%s< ~%s~\n%s\n%s" %
	      (sys.version_info, current_time, req.status, pwd))
    return apache.OK


