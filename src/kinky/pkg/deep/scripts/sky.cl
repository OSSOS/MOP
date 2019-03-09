#
#  sky.cl  
#
#  compute the sky flux and stdev for a given frame using a list of sections
#

procedure sky(image,sections)

	string image {"", prompt="Image to measure the sky values of"}
	file sections {"", prompt="name of file with image sections to use"}
        real sky
        real std
        bool verbose {"no", prompt="Print results"}
begin 
	string t_image
        string frame
        int    npix, nsections
        real   t_sky, t_stdev, t_min, t_max 
        real   sum_sky, sum_std
   
	t_image = image
 	list = sections

        # aSUPA0134204p_2_s[341:345,298:302]  25  -15.31872  5.057913  -25.57022  -5.529946

        nsections = 0
        sum_sky = 0
        sum_std = 0
        while(fscan(list, s1)!=EOF) {
            imstat(t_image//s1, format-) | scan(frame, npix, t_sky, t_stdev, t_min, t_max)
             sum_sky = sum_sky + t_sky
             sum_std = sum_std + t_stdev
            nsections = nsections + 1
        }
        sky = sum_sky/nsections
        std = sum_std/nsections
        if (verbose) print("SKY: "//sky, "STD: "//std)

end

	
