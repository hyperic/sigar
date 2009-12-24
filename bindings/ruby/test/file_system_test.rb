$LOAD_PATH.unshift File.dirname(__FILE__)
require 'helper'

class FileSystemTest < Test::Unit::TestCase

  def test_file_system
    sigar = Sigar.new
    sigar.file_system_list.each do |fs|
      assert_length fs.dev_name, "dev_name"
      assert_length fs.dir_name, "dir_name"
      assert_length fs.type_name, "type_name"
      assert_length fs.sys_type_name, "sys_type_name"
      assert fs.options.length >= 0, "options"

      begin
        usage = sigar.file_system_usage fs.dir_name
      rescue err
        if fs.type == Sigar::FSTYPE_LOCAL_DISK
          raise err
        end
        # else ok, e.g. floppy drive on windows
        next
      end
    end
  end

end
