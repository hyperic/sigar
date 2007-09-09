#!/usr/bin/env python

from distutils.core import setup, Extension

_sigar = Extension(
    "_sigar",
    ["_sigar.c"],
    include_dirs = ['../java/sigar-bin/include'],
    extra_compile_args = ['-Wall'],
    libraries=['sigar-universal-macosx'],
    library_dirs=['../java/sigar-bin/lib'],
    extra_link_args=[],
    define_macros=[],
    undef_macros=[])

setup(name="pysigar", version="0.1",
      py_modules = ['sigar'],
      ext_modules=[_sigar])
