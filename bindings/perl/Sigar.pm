package Sigar;

use 5.003;
use strict;
use vars qw($VERSION @ISA);

$VERSION = '0.01';

eval {
    require XSLoader;
};

if ($@) {
    #$] < 5.6
    require DynaLoader;
    @ISA = qw(DynaLoader);
    __PACKAGE__->bootstrap($VERSION);
}
else {
    XSLoader::load(__PACKAGE__, $VERSION);
}

1;
__END__
