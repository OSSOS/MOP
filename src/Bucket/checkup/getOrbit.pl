#!/usr/bin/env perl

my $object = shift;

my $query = "mysql -u cfhls cfeps -s -e 'select mpc from measure m JOIN object o on m.provisional like o.provisional where official like \"$object%\"' | fit_radec ";

#print "$query";
system("$query > $object.abg");
