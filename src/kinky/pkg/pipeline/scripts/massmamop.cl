## (MA) Loops over mamop for all fields
##
## procedure massmamop(ccdnumber)
##
##  INPUT:
##    On command line:
##	ccdnumber - the number of the ccd to analyse
##    Also requires input files:
##	fields.txt - file containing the names of all the fields
##
## This is intended to run lsmop for all folders for a given chip, 
## both for scrampled &/ fake planted images as well as "real" data,
## while keeping the human inspecting the images in the dark as to the nature
## of the detections seen. 
##
## MODIFICATIONS:
## 2011 Nov 02 : M.Alexandersen, created.
## 2012 Nov 14 : M.A. changed order of chip/field to field/ccd in dir tree. 
## 2012 Nov 27 : M.A. changed back <_< Thanks JJ... 
##
## To ensure photometry part works, 
## before running script, run:
## kinky
## deep
## digip
## daophot
##--------------------------------------------
procedure massmamop(ccdnumber)
	int ccdnumber {0, prompt=" CCD number "}
	string pwork {"./", prompt=" Path to working directory "}
	string preal {"./real", prompt=" Path to real triplets "}
	string pscram {"./scramble", prompt=" Path to scramplanted triplets"}
	string ffields {"./fields.txt", prompt=" File containing field names"}
##--------------------------------------------
begin
	string pathw,pathr,paths,fieldsf
	string nffile
	string dummy, fn
	string ccdns,fields[20],folderu[40],folders[40]
	int i,j,k
	int ccdn,start,nfields,nfieldn,stop
##--------------------------------------------

kinky		#Packages that need to be loaded. 
deep
digip
daophot

pathw = pwork
pathr = preal
paths = pscram
ccdn = ccdnumber
fieldsf = ffields
cd(pathw)

##--------------------------------------------
## Make foldernames
#if (ccdn < 10) { ccdns="chip0"//radix(ccdn,10) } else { ccdns="chip"//radix(ccdn,10) }
if (ccdn < 10) { ccdns="ccd0"//radix(ccdn,10) } else { ccdns="ccd"//radix(ccdn,10) }
print(" Working on ",ccdns)

##--------------------------------------------
## Find out how many fields there are to work on
nffile = 'nfield.foo'
wc(fieldsf, >> nffile)
list = nffile
dummy = fscan(list,nfields)
delete(nffile,verify-)

##--------------------------------------------
## Read the pointings that should be worked on.
list = fieldsf			# Don't ask, this doesn't make sense
for (j=1; j<=nfields; j+=1){
   dummy=fscan(list,fn)
   fields[j]=fn
}

##--------------------------------------------
## Make array of all the folders, scrambled and unscrambled, mixed together
for (j=1; j<=nfields; j+=1){
   i=2*j-1
#   folderu[i]=pathr//"/"//ccdns//"/"//fields[j]
#   folderu[i+1]=paths//"/"//ccdns//"/"//fields[j]
   folderu[i]=pathr//"/"//fields[j]//"/"//ccdns
   folderu[i+1]=paths//"/"//fields[j]//"/"//ccdns
}


urand(1,1,ndigits=2,seed=ccdn,scale_factor=2*nfields-1.1) | scan start
start=start+2			#2 to 40
stop = 2*nfields-start+1	#1 to 39
for (j=1; j<=stop; j+=1){
   folders[j]=folderu[j+start-1]
   print(j)
   k=j+start-1
   print(k)
   print(stop)
}
for (j=1; j<=2*nfields-stop; j+=1){
   folders[stop+j]=folderu[j]
   k=stop+j
   print(k)
   print(j)
   k=start-1
   print(k)
}
print("UnScrambled "//folderu[1])
print("Scrambled "//folders[1])

##--------------------------------------------
## Define ln procedures within iraf (rather than print "! ln -s ?" | cl)
task $lns = "$ln -s $(1) $(2)"
print "here i am"
cd(pathw)
for (j=1; j<=2*nfields; j+=1){
   cd(pathw)
   cd(folders[j])		# Didn't work. Iraf too smart 
   lns("*.cands.astrom","astrom.cands")
   if ( !access("astrom.cands") ) {
   #if ( !access("check_me.cl") ) { 	#Better (as not always has astrom)
      if ( !access("mamop.checked") ) {
         touch("massmamop.nocands")
      } 
   } else {
      if ( !access("mamop.checked") ) {
         mamop(ccdnr=ccdn)
      } 
   }
   cd(pathw)
}
##--------------------------------------------
print("Job's done!")

end
