#
#  testu.cl  
#

procedure testu(pseed)

	int pseed {"", prompt="seed?"}
begin 
	real   rseed, rseedtmp, rnum, mag
	real   x,y
	int    i,nobjs
	
	cache("utilities.urand")

	print(" ") 
	print(" testing urand ")
	rseed = pseed
	print(" number of objects to generate?")
	scan(nobjs)

	for(i=1; i<=nobjs; i+=1)
	{
	urand(1,1,ndigits=5,seed=rseed) | scan rnum
	mag = 24.2 + rnum
	urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
	urand(1,1,ndigits=5,seed=rseed,scale_factor=2000.) | scan x
	urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
	urand(1,1,ndigits=5,seed=rseed,scale_factor=2000.) | scan y
	urand(1,1,ndigits=5,seed=rseed,scale_factor=10000.) | scan rseed
	print(" x  y   mag ", x,y,mag)
	}

	print(" ")
	print(" Done ")
end

	
