#!/usr/bin/env python

from distutils.core import setup, Extension
from os import system, mkdir, path, popen
import sys

build = 'build'
options = {'perl':'perl'}

def parse_args():
    global options
    args = sys.argv[1:]
    for arg in args:
        if arg.find("--with-sigar") != -1:
            sys.argv.remove(arg)
            value = arg.split('=')[1]
            options['sigar'] = value
        elif arg.find("--with-perl") != -1:
            sys.argv.remove(arg)
            value = arg.split('=')[1]
            options['perl'] = value

def sbuild(cmd):
    return popen(options['perl'] + ' -Mlib=.. -MSigarBuild -e ' + cmd).readline()

def sargs(cmd):
    res = sbuild(cmd)
    if len(res) > 0:
        return res.split(' ')
    else :
        return None

parse_args()

if sys.argv[-1] != 'clean':
    if not path.exists(build):
        mkdir(build)
    system(options['perl'] + ' -Mlib=.. -MSigarWrapper -e generate Python ' + build)

if 'sigar' in options:
    sigar = options['sigar']
    print "Linking againt libsigar in " + sigar
    _sigar = Extension(
        "_sigar",
        ["_sigar.c"],
        include_dirs = [sigar + '/include', build],
        extra_compile_args = ['-Wall'],
        libraries=['sigar'],
        library_dirs=[sigar + '/lib'])
else :
    print "Inlining libsigar sources"
    src = sargs('inline_src -- ' + build)
    src.append('_sigar.c')
    cppflags = sargs('cppflags')
    cppflags.append('-Wall')
    _sigar = Extension(
        "_sigar",
        src,
        include_dirs = [build],
        extra_compile_args = cppflags,
        extra_link_args = sargs('ldflags'),
        libraries=sargs('libs'))

setup(name="pysigar", version="0.1",
      py_modules = ['sigar'],
      ext_modules=[_sigar])
