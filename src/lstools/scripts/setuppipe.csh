#!/bin/csh

#Need list of chips (01 02 03 .. 36) == chips
#Need field list (image root names + name of object directories) == fields

# for jmpmakepsf (to start pipeline, need this before find.pl)
# give jmpmakepsf name of directory & image & yes/no (whether to plant)

# give us an argument for plant or not (YES or NO)

set plant = $1

if (-e jmpmakepsf.input) /bin/rm jmpmakepsf.input
if (-e find.input) /bin/rm find.input
if (-e allpipeline.go) /bin/rm allpipeline.go
touch jmpmakepsf.input
touch find.input
foreach f (`cat fieldsonly`)
 foreach i (`cat chips`)
   foreach k (`ls chip$i/$f`)
  echo jmpmakepsf.csh chip$i/$f $k $plant  >> jmpmakepsf.input
end  
end
end
if (-e ttt) /bin/rm ttt
<jmpmakepsf.input grep .fits| grep -v psf.fits > ttt
/bin/mv ttt jmpmakepsf.input
/bin/cp jmpmakepsf.input allpipeline.go

if (-e WARNING) /bin/rm WARNING
<allpipeline.go grep .fits | grep fk > WARNING
echo "You have fake images already if there are lines in this file above!" >> WARNING


if ($plant == "YES") then 
 if (-e plant.input) /bin/rm plant.input
 touch plant.input
 foreach f (`cat fieldsonly`)
 	foreach i (`cat chips`)
#warning- plant.csh now requires rates, BUT didn't used to
	echo plant.csh chip$i/$f -rn ratemin -rx ratemax -a ang -aw awidth >> plant.input
	echo plant.csh chip$i/$f -rn ratemin -rx ratemax -a ang -aw awidth >> allpipeline.go
    end
 end
endif

if ($plant == "YES") then 
   set prefix = fk
else 
    set prefix = "''"
endif 

if (-e find.input) /bin/rm find.input
touch find.input
foreach f (`cat fieldsonly`)
 foreach i (`cat chips`)
       echo find.pl -p $prefix -d chip$i/$f -rn ratemin -rx ratemax -a ang -aw awidth >> find.input
       echo find.pl -p $prefix -d chip$i/$f -rn ratemin -rx ratemax -a ang -aw awidth >> allpipeline.go
end
end


