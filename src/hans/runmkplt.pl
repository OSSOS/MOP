#!/usr/bin/perl

use Getopt::Long;

GetOptions('f|file:s');

chomp($myimage = $opt_f);

$pltfail = $myimage.".mkpltsol.FAILED";
#print $pltfail;
if (-e $pltfail)  {
    print "Removing mkpltsol files for $myimage\n";
    $result = `/bin/rm $myimage.mkpltsol.*`; 
}
$hansfiles = $myimage.".hansastone.FAILED";
if (-e $hansfiles) {
    print "Removing hansastone files for $myimage\n";
    $result = `/bin/rm $hansfiles`;
}

print ("Running stepZjmp and step1jmp\n");
$result = `stepZjmp -f $myimage`;
$result = `step1jmp -f $myimage -w 4 -t 4 -m 30000`;

$jmpobj = $myimage.".obj.jmp";

print ("Sorting and shortening obj.jmp file for mkpltsol\n");
$result = `grep "#" $jmpobj > tmp`;
$result = `grep -v "#" $jmpobj | sort -k 3 -rn | head -500 >> tmp`;
$result = `/bin/mv $jmpobj $jmpobj."orig"`;
$result = `/bin/mv tmp $jmpobj`; 

$result = `mkpltsol $myimage`;
$pltok = $myimage.".mkpltsol.OK";
if (-e $pltok) {
    $result = `/bin/rm $pltfail`;
    printf("Mkpltsol ran successfully\n");
}
$result = `/bin/rm stepZjmp* step1jmp*`;

exit(-1);
