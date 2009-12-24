require 'test/unit'
require 'rbsigar'

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

