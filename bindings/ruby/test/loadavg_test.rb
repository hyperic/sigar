$LOAD_PATH.unshift File.dirname(__FILE__)
require 'helper'

class LoadAvgTest < Test::Unit::TestCase

  def test_loadavg
    begin
      loadavg = Sigar.new.loadavg
    rescue #XXX SigarNotImplemented (win32)
      return
    end
    assert loadavg.length == 3
  end
end
