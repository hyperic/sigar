#
# Copyright (c) 2009-2010 VMware, Inc.
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
