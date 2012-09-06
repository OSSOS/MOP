      IMPLICIT
     *       NONE
      INTEGER
     *       nzmax, nacc, nchunk, nhist, nspdzone
      DOUBLE PRECISION
     *       convert
      PARAMETER
     *       (nzmax = 10)
      PARAMETER
     *       (nacc = 96)
      PARAMETER
     *       (nchunk = 1*1000*1000)
      PARAMETER
     *       (convert = 3600.0D00*100.0D00)
      PARAMETER
     *       (nhist = 25)
      PARAMETER
     *       (nspdzone = 24)
      INTEGER
     *       nra, ndec, dzone(NZMAX), frec(NACC), nrec(NACC),
     *       oldzone, buf(3,NCHUNK), nsav, bhist(NHIST),
     *       rhist(NHIST), nfn(NSPDZONE)
      DOUBLE PRECISION
     *       rfrst(NZMAX), rlast(NZMAX), dfrst(NZMAX), dlast(NZMAX),
     *       rcent, dcent, scale
      CHARACTER*64
     *       fn(NSPDZONE)
      COMMON
     *       rfrst, rlast, dfrst, dlast, rcent, dcent, scale,
     *       nra, ndec, dzone, frec, nrec, oldzone, buf, nsav,
     *       bhist, rhist, nfn,
     *       fn

