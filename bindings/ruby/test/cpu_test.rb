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
