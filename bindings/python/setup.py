#!/usr/bin/env python
#
# Copyright (c) 2007, 2009 Hyperic, Inc.
# Copyright (c) 2009 VMware, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

from distutils.core import setup, Extension
from distutils.util import get_platform
from distutils.sysconfig import get_config_var, get_python_version
from os import system, mkdir, path, popen, rename
import sys

build = 'build'
options = {'perl':'perl'}
deps = [build + '/_sigar_generated.c']

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
        depends = deps,
        include_dirs = [sigar + '/include', build],
        extra_compile_args = ['-Wall'],
        libraries=['sigar'],
        library_dirs=[sigar + '/lib'])
else :
    print "Inlining libsigar sources"
    src = sargs('inline_src -- ' + build)
    src.append('_sigar.c')

    version_file = "sigar_version.c"
    build_version_file = build + "/" + version_file
    libname = "_sigar" + get_config_var("SO")
    filters = \
        'ARCHNAME=' + get_platform() + '-' + get_python_version() + ' ' + \
        'ARCHLIB=' + libname + ' ' + \
        'BINNAME=' + libname
    system(options['perl'] + ' -Mlib=.. -MSigarBuild -e version_file ' + filters)
    rename(version_file, build_version_file)
    src.append(build_version_file)

    cppflags = sargs('cppflags')
    cppflags.append('-Wall')
    _sigar = Extension(
        "_sigar",
        src,
        depends = deps,
        include_dirs = [build],
        extra_compile_args = cppflags,
        extra_link_args = sargs('ldflags'),
        libraries=sargs('libs'))

setup(name="pysigar", version="0.1",
      py_modules = ['sigar'],
      ext_modules=[_sigar])
