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

require 'test/unit'
require 'sigar'

module Test::Unit::Assertions
  def assert_gt_eq_zero(value, message)
    message = build_message message, '<?> is not >= 0.', value
    assert_block message do
      value >= 0
    end
  end

  def assert_gt_zero(value, message)
    message = build_message message, '<?> is not > 0.', value
    assert_block message do
      value > 0
    end
  end

  def assert_eq(expected, actual, message)
    message = build_message message, '<?> != <?>.', expected, actual
    assert_block message do
      expected == actual
    end
  end

  def assert_length(value, message)
    message = build_message message, '<?>.length > 0.', value
    assert_block message do
      value.length > 0
    end
  end

  def assert_any(value, message)
    message = build_message message, '<?> is anything.', value
    assert_block message do
      true
    end
  end

end

