#!/usr/bin/python

import os
import subprocess


repodef = 'repos'

try:
    frepos = open(repodef, 'r')
except:
    print "no repos"
    sys.exit (0)

repos = frepos.readlines()
for repo in repos:
    repo = repo.rstrip()
    print "repo: "+repo
    subprocess.call (["pwd"])

    homedir = os.path.expanduser('~')
    targetdir = homedir+'/'+repo
    os.chdir (targetdir)
    print "now at: "+os.getcwd()
    subprocess.call (["git", "status", "-s"])
    subprocess.call (["git", "fetch"])
        
