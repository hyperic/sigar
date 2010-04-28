#
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

$LOAD_PATH.unshift File.dirname(__FILE__)
require 'helper'

class CpuTest < Test::Unit::TestCase
  def check_cpu(cpu)
    assert_gt_eq_zero cpu.user, "user"
    assert_gt_eq_zero cpu.sys, "sys"
    assert_gt_eq_zero cpu.idle, "idle"
    assert_gt_eq_zero cpu.wait, "wait"
    assert_gt_eq_zero cpu.irq, "irq"
    assert_gt_eq_zero cpu.soft_irq, "soft_irq"
    assert_gt_eq_zero cpu.stolen, "stolen"
    assert_gt_zero cpu.total, "total"
  end

  def test_cpu
    sigar = Sigar.new
    check_cpu sigar.cpu

    sigar.cpu_list.each do |cpu|
      check_cpu cpu
    end
  end
end
