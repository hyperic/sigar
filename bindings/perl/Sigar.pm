#/*
# * Copyright (C) [2004, 2005, 2006], Hyperic, Inc.
# * This file is part of SIGAR.
# * 
# * SIGAR is free software; you can redistribute it and/or modify
# * it under the terms version 2 of the GNU General Public License as
# * published by the Free Software Foundation. This program is distributed
# * in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# * even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# * PARTICULAR PURPOSE. See the GNU General Public License for more
# * details.
# * 
# * You should have received a copy of the GNU General Public License
# * along with this program; if not, write to the Free Software
# * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
# * USA.
# */

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
