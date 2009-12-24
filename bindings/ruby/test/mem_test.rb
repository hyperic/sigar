$LOAD_PATH.unshift File.dirname(__FILE__)
require 'helper'

class MemTest < Test::Unit::TestCase

  def test_mem
    sigar = Sigar.new
    mem = sigar.mem
    assert_gt_zero mem.total, "total"
    assert_gt_zero mem.used, "used"

    assert_gt_zero mem.used_percent, "used_percent"
    assert mem.used_percent <= 100, "used_percent <= 100"

    assert_gt_eq_zero mem.free_percent, "free_percent"
    assert mem.free_percent < 100, "free_percent < 100"

    assert_gt_zero mem.free, "free"

    assert_gt_zero mem.actual_used, "actual_used"

    assert_gt_zero mem.actual_free, "actual_free"

    assert_gt_zero mem.ram, "ram"

    assert (mem.ram % 8) == 0
  end

end
