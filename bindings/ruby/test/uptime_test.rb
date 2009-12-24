$LOAD_PATH.unshift File.dirname(__FILE__)
require 'helper'

class UptimeTest < Test::Unit::TestCase

  def test_uptime
    uptime = Sigar.new.uptime
    assert_gt_zero uptime.uptime, "uptime"
  end
end
