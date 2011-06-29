#!/usr/bin/env python
#
# Copyright (c) 2011 VMware, Inc.
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

import unittest
import os
import sys

#prepend PYTHONPATH to find sigar in build/
from distutils.util import get_platform
from distutils.sysconfig import get_python_version
sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)) + \
                                   "/../build/lib." + get_platform() + "-" + get_python_version())

import sigar

class SigarTest(unittest.TestCase):

    def setUp(self):
        self.sigar = sigar.open()

    def tearDown(self):
        self.sigar.close()
        self.sigar = None

    def is_type(self, obj, cls):
        return obj.__class__.__name__ == cls

    def testCpuList(self):
        retval = self.sigar.cpu_list()
        self.assertTrue(isinstance(retval, tuple))
        for cpu in retval:
            self.assertTrue(self.is_type(cpu, "Cpu"))

    def testCpuInfoList(self):
        retval = self.sigar.cpu_info_list()
        self.assertTrue(isinstance(retval, tuple))
        for cpu in retval:
            self.assertTrue(self.is_type(cpu, "CpuInfo"))

    def testWhoList(self):
        retval = self.sigar.who_list()
        self.assertTrue(isinstance(retval, tuple))
        for who in retval:
            self.assertTrue(self.is_type(who, "Who"))

    def testNetRouteList(self):
        retval = self.sigar.net_connection_list(0)
        self.assertTrue(isinstance(retval, tuple))
        for conn in retval:
            self.assertTrue(self.is_type(conn, "NetConnection"))

    def testNetStat(self):
        retval = self.sigar.net_stat(0)
        self.assertTrue(self.is_type(retval, "NetStat"))

    def testNetRoute(self):
        retval = self.sigar.net_route_list()
        self.assertTrue(isinstance(retval, tuple))
        for route in retval:
            self.assertTrue(self.is_type(route, "NetRoute"))

    def testProcArgs(self):
        retval = self.sigar.proc_args(os.getpid())
        self.assertTrue(isinstance(retval, tuple))
        self.assertTrue(retval > 1)
        for arg in retval:
            self.assertTrue(isinstance(arg, str))

        self.assertEquals(retval, self.sigar.proc_args(str(os.getpid())))

        self.assertEquals(retval, self.sigar.proc_args("$$"))

        retval = self.sigar.proc_args("Pid.Pid.eq=" + str(os.getpid()))
        self.assertTrue(isinstance(retval, tuple))

    def testProcEnv(self):
        retval = self.sigar.proc_env(os.getpid())
        for k, v in os.environ.items():
            self.assertEquals(v, retval[k])
        for pid in self.sigar.proc_list("State.Name.eq=python"):
            self.sigar.proc_env(pid)

    def testProcList(self):
        retval = self.sigar.proc_list()
        self.assertTrue(isinstance(retval, tuple))
        self.assertTrue(len(retval) > 1)
        self.assertTrue(os.getpid() in retval)
        self.assertTrue(os.getpid() in self.sigar.proc_list("State.Name.eq=python"))
        self.assertTrue(os.getpid() in self.sigar.proc_list("State.Name.re=.*ython"))
        self.assertFalse(os.getpid() in self.sigar.proc_list("State.Name.eq=ruby"))
        try:
            self.sigar.proc_list("Invalid.Query.xe=.*ython")
            self.assertTrue(False)
        except ValueError:
            self.assertTrue(True)

    def testPtql(self):
        try:
            self.sigar.proc_args("Invalid.Query.xe=.*ython")
            self.assertTrue(False)
        except ValueError:
            self.assertTrue(True)

    def testConstants(self):
        self.assertEquals(sigar.IFF_UP, 0x1)
        self.assertTrue(len(sigar.VERSION) >= 7)

if __name__ == '__main__':
    unittest.main()
