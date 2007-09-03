require 'mkmf'

extension_name = 'rbsigar'

installdir = '../java/sigar-bin'
#XXX hardwired for the moment
libname = 'sigar-universal-macosx'

$CPPFLAGS += ' -I' + installdir + '/include'
$LOCAL_LIBS += '-L' + installdir + '/lib -l' + libname

dir_config(extension_name)

system('perl -Mlib=.. -MSigarWrapper -e generate Ruby .')

create_makefile(extension_name)
