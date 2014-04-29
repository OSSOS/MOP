C -*-compile-command: "cf77 -O6 -o ../bin/LINUX/psfjmp psfjmp.f -L../lib/LINUX -ljmppsf -lcfitsio; /bin/rm psfjmp.o"; -*-
C When actually using g77, add the option -fno-automatic

      implicit none

      include 'match-mov.h'

      integer*4
     $  image_size_max, n_ap_max, max_par, max_psf, max_exp

      parameter
     $  (image_size_max = 2100*4100, n_ap_max = 12, max_par = 6,
     $  max_psf = 500, max_exp = 6)

      integer*4
     $  iargc, narg, i, i1, i2, lun_b, lun_i, lun_o, idstar(n_br_max),
     $  naxis1, naxis2, nx, ny, y_offset, j, n, n_star, n_ap, n_ei,
     $  worki(n_br_max), nbpsf, id_k(n_br_max), n_tot, n_par, anal_mod,
     $  iexpand, ifrac, npass, npsf, nexp, k, lun_p

      real*4
     $  echelle, fwhm, e_sky, m_sky, image(image_size_max), exptime,
     $  x_br(n_br_max), y_br(n_br_max), flux(n_br_max), size(n_br_max),
     $  inten(n_br_max), el(n_br_max), lsky(n_br_max), lsfl(n_br_max),
     $  x2(n_br_max), sharp(n_br_max), phpadu, rdnoise, rms_sky, lobad,
     $  hibad, is, os, psfrad, fitrad, aper(n_ap_max), s_sky(n_br_max),
     $  vs(n_br_max), sk(n_br_max), ap_mag(n_ap_max,n_br_max), zero_p,
     $  mag_err(n_ap_max,n_br_max), workr(n_br_max), mn_sky, sig_sky,
     $  msig_sky, ssig_sky, m_skew, s_skew, thresh, x_k(n_br_max),
     $  y_k(n_br_max), mag_k(n_br_max), sky_k(n_br_max), par(max_par),
     $  psf(max_psf,max_psf,max_exp), x_sig, y_sig, perr, pkerr, n_sig

      character
     $  image_name*100, base_name*100, arg*100, line*100, psf_name*100,
     $  bright_name*100, header(6)*80, psf_head(2)*80, proc_name*80,
     $  plant*3, proc_line(10)*29

      logical
     $  finished, help, no_f, no_p, no_a


c Create a file for later error handling

      open (unit=1, file='psfjmp.FAILED', status='unknown')
      write (1, *) ' '
      close (1)

c Get input parameters

      do i = 1, 80
         base_name(i:i) = char(0)
      end do

      narg = iargc()
      i = 1
      no_f = .true.
      no_p = .true.
      no_a = .true.
      help = (narg .ne. 6)

 90   continue
      if (i .le. narg) then
         call getarg (i, arg)
         if (arg(1:2) .eq. '-f') then
            i = i + 1
            call getarg (i, base_name)
            no_f = .false.
         else if (arg(1:2) .eq. '-p') then
            i = i + 1
            call getarg (i, proc_name)
            no_p = .false.
         else if (arg(1:2) .eq. '-a') then
            i = i + 1
            call getarg (i, plant)
            no_a = .false.
         else if ((arg(1:2) .eq. '-h') .or. (arg(1:2) .eq. '-?')) then
            i = narg + 1
            help = .true.
         end if
         i = i + 1
         goto 90
      end if

      j = 1
      do i = 0, narg
         call getarg (i, arg)
         call read_file_name (arg, i1, i2, finished, 100)
         line(j:j+i2-i1) = arg(i1:i2)
         j = j + i2 - i1 + 1
         line(j:j) = ' '
         j = j + 1
      end do
      write (6, *) line(1:j-1)

      open (unit=1, file='psfjmp.FAILED', status='unknown')
      write (1, *) line(1:j-1)
      close (1)

      help =  help .or. no_f .or. no_p .or. no_a

      if (help) then
         write (6, *)
     $     'Usage: psfjmp -f <image file> -p <proc-these-file>'
     $     //' -a <YES|NO>'
         write (6, *) 'where:'
         write (6, *) '-f <image file>: image to process (no extension)'
         write (6, *)
     $     '-p <proc-these-file>: file containing list of images'
         stop
      end if

      if ((plant .eq. 'YES') .or. (plant .eq. 'YEs')
     $  .or. (plant .eq. 'Yes') .or. (plant .eq. 'yes')
     $  .or. (plant .eq. 'yeS') .or. (plant. eq. 'yES')
     $  .or. (plant .eq. 'yEs') .or. (plant .eq. 'YeS')) then
         plant = 'YES'
      else
         plant = 'NO '
      end if

 80   continue

      call read_file_name (base_name, i1, i2, finished, 80)
      if (finished) stop

      image_name = base_name(i1:i2)//'.fits'
      psf_name = base_name(i1:i2)//'.psf.fits'
      bright_name = base_name(i1:i2)//'.bright.psf'
      lun_i = 20
      lun_o = 21
      lun_b = 22
      lun_p = 23

c Reads in the bright stars for PSF fitting.

      open (unit=lun_b, file=bright_name, status='old')
      read (lun_b, '(a)') header(1)
      if (header(1)(15:16) .ne. ' 1') then
         write (6, '(a)') 'File '//base_name(i1:i2)//'.bright.psf'
     $     //' written by '//header(1)(4:18)
         write (6, '(a)') 'Should be MOP version 1.x'
         stop
      end if
      read (lun_b, '(a)') header(2)
      read (lun_b, '(a)') header(3)
      read (header(3), 104) exptime, echelle, hibad
      read (lun_b, '(a)') header(4)
      read (lun_b, '(a)') header(5)
      read (header(5), 105) nx, ny, phpadu, rdnoise
      read (lun_b, '(a)') header(6)
 104  format (18x, f8.2, 6x, f6.2, f9.1)
 105  format (32x, 2i6, 20x, 2f6.2)

      fwhm = echelle
      is = 5.*echelle
      os = 8.*echelle
      psfrad = 5.*echelle
      fitrad = .85*echelle
      aper(1) = 5.*echelle
      aper(2) = 1.2*echelle
      n_ap = 2
      n_sig = 3.
      zero_p = 26. + 2.5*log10(exptime)

      n = 1
      e_sky = 0.
 200  continue
         read (lun_b, *, end=210, err=200) x_br(n), y_br(n), flux(n),
     $     size(n), inten(n), el(n), x2(n), sharp(n), lsky(n), lsfl(n)
         idstar(n) = n
         id_k(n) = n
         x_k(n) = x_br(n)
         y_k(n) = y_br(n)
         e_sky = e_sky + lsky(n)
         n = n + 1
         if (n .gt. n_br_max) then
            write (6, *) 'Too many bright stars in '//bright_name
            write (6, *) 'Truncating file and continuing ...'
            goto 210
         end if
         goto 200
 210  continue
      n_star = n - 1
      n_tot = n_star
      if (n_star .gt. 0) then
         e_sky = e_sky/float(n_star)
      else
         write (6, *) 'Error: read ', n_star, ' bright stars.'
         stop
      end if
      close (lun_b)

c Open image file

      call open_image (image_name, lun_i, naxis1, naxis2)

      if ((nx .ne. naxis1) .or. (ny .ne. naxis2)) then
         write (6, *) 'Error: incompatible image size (', nx, ',', ny,
     $     ') != (', naxis1, ',', naxis2, ').'
         stop
      end if

      y_offset = 0
      nx = naxis1
      ny = naxis2

c Read in image

      call read_seg (image, y_offset, lun_i, nx, ny)

c Estimate sky background value. Needed for low value rejection.

      call mean_sky (image, nx, ny, m_sky, e_sky)
      rms_sky = sqrt(rdnoise**2 + m_sky/phpadu)

 500  continue
      lobad = m_sky - n_sig*rms_sky
      if (lobad .lt. 0.) then
         write (6, *) 'Negative value of LOBAD.'
         stop 'Exiting.'
      end if
      write (6, *) 'm_sky = ', m_sky
      write (6, *) 'rms_sky = ', rms_sky
      write (6, *) 'lobad = ', lobad
      write (6, *) 'hibad = ', hibad

c Now, compute aperture photometry on the bright stars. Used also to
c get rid of stars with bad surrounding background.

      call phot (image, nx, ny, phpadu, lobad, hibad, aper, n_ap, is,
     $  os, idstar, x_br, y_br, ap_mag, mag_err, s_sky, vs, sk, n_star)
      do i = 1, n_star
         mag_k(i) = ap_mag(2,i)
         sky_k(i) = s_sky(i)
      end do

      write (60, *)
      write (60, *) n_star
      do i = 1, n_star
         write (60, *) i, idstar(i), x_br(i), y_br(i), ap_mag(1,i),
     $     ap_mag(2,i), s_sky(i), vs(i), sk(i)
      end do

c Eliminate star with bad background: undefined magnitude with large
c aperture.

      call clean_mag (idstar, x_br, y_br, s_sky, vs, sk, ap_mag,
     $  mag_err, n_star, n_ap, n_ap_max, 1)

      if (2*n_star .lt. n_tot) then
         n_sig = n_sig + 1.
         n_star = n_tot
         goto 500
      end if
      write (60, *)
      write (60, *) n_star
      do i = 1, n_star
         write (60, *) i, idstar(i), x_br(i), y_br(i), ap_mag(1,i),
     $     ap_mag(2,i), s_sky(i), vs(i), sk(i)
      end do

c Eliminate stars with undifined small aperture magnitude.

      call clean_mag (idstar, x_br, y_br, s_sky, vs, sk, ap_mag,
     $  mag_err, n_star, n_ap, n_ap_max, 2)

      if (2*n_star .lt. n_tot) then
         n_sig = n_sig + 1.
         n_star = n_tot
         goto 500
      end if
      write (60, *)
      write (60, *) n_star
      do i = 1, n_star
         write (60, *) i, idstar(i), x_br(i), y_br(i), ap_mag(1,i),
     $     ap_mag(2,i), s_sky(i), vs(i), sk(i)
      end do

c Compute the mean and rms of sky background, variance and skewness.

      call mnsky (mn_sky, sig_sky, msig_sky, ssig_sky, m_skew, s_skew,
     $  s_sky, vs, sk, n_star, workr)

      write (60, *)
      write (60, *) mn_sky, sig_sky, msig_sky, ssig_sky, m_skew, s_skew

c      mn_sky = 3535.25317
c      sig_sky = 30.5377216
c      msig_sky = 40.1134109
c      ssig_sky = 12.922802
c      m_skew = .0686168224
c      s_skew = .0686168224

      write (60, *)
      write (60, *) mn_sky, sig_sky, msig_sky, ssig_sky, m_skew, s_skew

c Now select stars far from edges of chip, and far from eachother.

      nbpsf = 45
      call pckpsf (x_br, y_br, ap_mag, n_star, fitrad, psfrad, nx, ny,
     $  nbpsf, n_ap, n_ap_max, workr, worki)
      call clean_mag (idstar, x_br, y_br, s_sky, vs, sk, ap_mag,
     $  mag_err, n_star, n_ap, n_ap_max, n_ap)

      write (60, *)
      do i = 1, n_star
         write (60, *) i, idstar(i), x_br(i), y_br(i), ap_mag(2,i),
     $     vs(i), sk(i)
      end do

c Retain only stars with well behaved sky annulus.

      thresh = 3.
      call pselect (ap_mag, s_sky, vs, sk, n_star, mn_sky,
     $  sig_sky, msig_sky, ssig_sky, m_skew, s_skew, thresh, n_ap,
     $  n_ap_max, idstar)
      call clean_mag (idstar, x_br, y_br, s_sky, vs, sk, ap_mag,
     $  mag_err, n_star, n_ap, n_ap_max, n_ap)

      write (60, *)
      do i = 1, n_star
         write (60, *) i, idstar(i), x_br(i), y_br(i), ap_mag(2,i),
     $     vs(i), sk(i)
      end do

c Now call the PSF fitting subroutine. First, need to open the output
c file.

      anal_mod = 1
      iexpand = 0
      ifrac = 0
      npass = 0
      call getpsf (image, nx, ny, psfrad, fitrad, lobad, hibad, phpadu,
     $  rdnoise, fwhm, anal_mod, iexpand, ifrac, npass, id_k, x_k, y_k,
     $  mag_k, sky_k, n_tot, idstar, n_star, n_ei, par, n_par, psf,
     $  npsf, nexp, psf_head)

c      call open_psf_output_file (psf_name, lun_o, ???)
      open (unit=lun_o, file=psf_name, status='unknown')
      write (lun_o, '(a)') psf_head(1)
      write (lun_o, '(a)') psf_head(2)
      do k = 1, nexp
         write (lun_o, 204) ((psf(i,j,k), i=1,npsf), j=1,npsf)
      end do
 204  format (1x, 1p, 6e13.6)
      close (lun_o)

      write (60, *)
      do i = 1, n_star
         write (60, *) i, id_k(i), x_k(i), y_k(i), mag_k(i), sky_k(i)
      end do

      write (60, *)
      do i = n_star+1, n_ei
         write (60, *) i, id_k(i), x_k(i), y_k(i), mag_k(i), sky_k(i)
      end do

c From the PSF fit parameter, determine the actual FWHM

      x_sig = par(1)
      y_sig = par(2)
      fwhm = sqrt(x_sig**2 + y_sig**2)
      if (abs((x_sig - y_sig)/fwhm) .gt. 0.2) then
         open (unit=lun_o, file=base_name(i1:i2)//'psf.warning',
     $     status='unknown')
         write (lun_o, '(a)')
     $     'ERROR: The PSF is too elliptical for planting. image'
     $     //base_name(i1:i2)
         close (lun_o)
         fwhm = echelle
      else
         fwhm = 1.05*fwhm*sqrt(2.*log(2.))
      end if

      write (6, *) base_name(i1:i2)
      open (unit=lun_p, file=proc_name, status='new', err = 1000)
      write (6, *) 'Openned new '//proc_name
      write (lun_p, '(a)') '# Files to be planted and searched'
      write (lun_p, '(a)') '#             image  fwhm plant'

 1000 continue
      close (lun_p)
      open (unit=lun_p, file=proc_name, status='old', err = 2000)
      write (6, *) 'Openned old '//proc_name

 1010 continue
         read (lun_p, '(a)', end=1020) line
         write (6, '(a)') line
         goto 1010
 1020 continue
      j = 19
      do i = i2, i1, -1
         line(j:j) = base_name(i:i)
         j = j - 1
      end do
      do i = j, 1, -1
         line(i:i) = ' '
      end do
      write (lun_p, '(a19, 1x, f5.1, 1x, a3)') line(1:19), fwhm, plant
      close (lun_p)

      write (6, *) 'FWHM: ', fwhm

c Call nstar to fit the profil to multiple stars. Needed to get the
c aperture correction.

      perr = 0.75
      pkerr = 5.
c      call nstar (par, max_par, psf, max_psf, max_exp, image, nx, ny,
c     $  fitrad, perr, pkerr, psf_head, id_k, x_k, y_x, n_ei)

      call close_image (lun_i)

c Apparently things went right

      open (unit=1, file='psfjmp.OK', status='unknown')
      write (1, *) ' '
      close (1)

      stop

 2000 continue
      write (6, '(a)') 'Could not open file: '//proc_name
      stop 'Exiting.'

      end

      include 'read_file_name.f'
      include 'open_image.f'
      include 'read_seg.f'
      include 'fit_util.f'
      include 'phot.f'
      include 'sort.f'
      include 'psf_util.f'
      include 'getpsf.f'
      include 'close_image.f'
