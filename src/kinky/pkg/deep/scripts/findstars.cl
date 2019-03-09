#
#  findstars.cl  
#
#  compute the sky flux and stdev for a given frame using a list of sections
#

procedure findstars(image, sky, std, fwhm)

	string image {"", prompt="Image to run daofind on"}
        real sky {"", prompt="Sky value"}
        real std {"", prompt="Sky STDDEV"}
        real fwhm {"", prompt="FWHM" }

begin 
	string t_image
        real t_sky, t_std, t_fwhm
   
	t_image = image
        t_sky = sky
        t_std = std
        t_fwhm = fwhm

        datapars.fwhmpsf = t_fwhm
        datapars.sigma = t_std
        datapars.datamin = t_sky - 10*t_std
        datapars.datamax = 20000

        fitskypars.salgorithm = 'mode'
        fitskypars.annulus = 5*t_fwhm+1
        fitskypars.dannulus = t_fwhm
        fitskypars.skyvalue = t_sky
        findpars.threshold = 2.5

        daofind(t_image, "default", veri-, verb-)

end

	
