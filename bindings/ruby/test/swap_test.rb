require 'test/unit'
require 'rbsigar'

class SwapTest < Test::Unit::TestCase

  def test_swap
    sigar = Sigar.new
    swap = sigar.swap

    assert_gt_eq_zero swap.total, "total"
    assert_gt_eq_zero swap.used, "used"
    assert_gt_eq_zero swap.free, "free"

    assert_eq swap.total - swap.used, swap.free, "total-used==free"

    assert_any swap.page_in, "page_in"
    assert_any swap.page_out, "page_out"
  end

end
