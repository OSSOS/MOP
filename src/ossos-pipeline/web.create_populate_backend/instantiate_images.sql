-- instantiates the table 'images' within the db ossuary for OSSOS data
-- images: a table for the image .fits header info and a bit of quality info
--
-- MTB 29 April 2013  NRC-Herzberg


CREATE TABLE images (
  image_id     BIGINT PRIMARY KEY, -- EXPNUM: CFHT odometer number (TAP:collectionID)
  cfht_field   TEXT                     NOT NULL, -- OBJECT the field it thinks it's looking at. eg 'O+0-1   '
  block        TEXT                     NULL, -- semester + on/off ecliptic
  filename     TEXT                     NOT NULL, -- FILENAME '1616934o' / Base filename at acquisition: header has a .head suffix
  obs_start    TIMESTAMP WITH TIME ZONE NOT NULL, -- from DATE-OBS and UTIME: at start of observation (UTC)
  obs_end      TIMESTAMP WITH TIME ZONE NOT NULL, -- DATE: UTC Date of file creation
  mjd_start    DOUBLE PRECISION         NOT NULL, -- MJDATE / Modified Julian Date at start of observation
  mjd_end      DOUBLE PRECISION         NOT NULL, -- MJDEND / Modified Julian Date at end of observation
  exptime      INTERVAL                 NOT NULL, -- EXPTIME / Measured integration time (seconds)
  qrunid       VARCHAR(8)               NOT NULL, -- when the data are taken: use for identifying runs in the same lunation
  airmass      REAL                     NOT NULL, -- AIRMASS / Airmass at start of obs
  moon_angle   REAL                     NOT NULL, -- MOONANGL / Angle from object to moon at start in degrees
  moon_phase   NUMERIC                  NOT NULL, -- MOONPHAS / Moon phase @ 0 HST, 0..1 new>full, -1..0 >new
  moon_up      BOOLEAN                  NOT NULL, -- MOONUP / Moon up? True or False
-- and Stephen informs me that the updated WCS can be found in:
  crval_ra     DOUBLE PRECISION         NULL, -- CRVAL1 / WCS Ref value (RA in decimal degrees)
  crval_dec    DOUBLE PRECISION         NULL, -- CRVAL2 / WCS Ref value (DEC in decimal degrees)
  crpix_ra     DOUBLE PRECISION         NULL, -- CRPIX1
  crpix_dec    DOUBLE PRECISION         NULL, -- CRPIX2
-- these are for the higher-order correction terms 
-- CD[1..2]_[1..2]
  cd_1_1       DOUBLE PRECISION         NULL,
  cd_1_2       DOUBLE PRECISION         NULL,
  cd_2_1       DOUBLE PRECISION         NULL,
  cd_2_2       DOUBLE PRECISION         NULL,
-- PV[1..2]_[1..10]  -- okay there's an awful lot of these!	
  iq_ossos     REAL                     NULL, -- fwhm x pixel scale modulated by airmass
  iq_elixir    REAL                     NULL, -- elixir does its own thing: for comparison
  zeropt       REAL                     NULL, -- zeropoint of image
  snr          REAL                     NULL, -- SNR on chip 13, computed on aperture/sky level/Gwyn zeropoint
  proc_status  TEXT                     NULL, -- storage for VOSpace tags
  blink_status TEXT                     NULL, -- storage for VOSpace tags
  comment      TEXT                     NULL
);

-- pixel scale is symmetric, 0.1850 arcsec/pixel. Dectector is [1:23219,1:19354] pixels.
-- saturates at/max linearity is at 65535 ADU.
-- LATITUDE = 19.825252 / Latitude (degrees N) LONGITUD= -155.468876 / Longitude (degrees E)   

-- for date searching
CREATE INDEX images_obsdt_index ON images (obs_end ASC);
-- for the q3c spatial searching: requires q3c to be installed!
--CREATE INDEX images_q3c_index ON images(q3c_ang2ipix(crval_ra, crval_dec));
--CLUSTER images USING images_q3c_index;
ANALYZE images;
