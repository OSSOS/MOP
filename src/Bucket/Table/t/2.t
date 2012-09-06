# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use strict;
use Test::More tests => 5;
BEGIN { use_ok('Bucket::Control') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.


### check MOP_execute with forcing
ok(-e "2.t.FAILED", "Control created the FAILED file");
ok (MOP_execute("t/t.pl","none","force")=~/none/, "MOP_execute succeeed");
ok (MOP_execute("t/t.pl","none")=~/none/, "MOP_execute with no force");
is ($Bucket::Control::err, 0, "Error flag set correct for success");
