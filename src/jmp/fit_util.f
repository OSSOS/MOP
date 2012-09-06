C -*-compile-command: "make_lib"; -*-
c
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine fits the X's and Y's of a set of star to another set.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine fit_xy (x1, y1, x2, y2, sig, n_p, coeff, n_c, func)

      include 'match-mov.h'

      integer*4
     $  ncvm

      parameter
     $  (ncvm = 20)

      integer*4
     $  n_p, n_c, i, lista(ncvm), n_fit

      real*8
     $  x1(*), y1(*), x2(*), y2(*), coeff(*), data(2,n_br_max),
     $  sig(*), covar(ncvm,ncvm), chisq

      external func

      n_fit = n_c
      do i = 1, n_fit
         lista(i) = i
      end do

      do i = 1, n_p
         data(1,i) = x2(i)
         data(2,i) = y2(i)
      end do

      call lfit2 (data, x1, sig, n_p, coeff(1), n_c, lista, n_fit,
     $  covar, ncvm, chisq, func)
      call lfit2 (data, y1, sig, n_p, coeff(n_c+1), n_c, lista, n_fit,
     $  covar, ncvm, chisq, func)

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine fits one dimentional set of data.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine fit_1d (x, y, sig, n_p, coeff, n_c, func)

      implicit none

      integer*4
     $  ncvm

      parameter
     $  (ncvm = 20)

      integer*4
     $  n_p, n_c, i, lista(ncvm), n_fit

      real*8
     $  x(*), y(*), coeff(*), sig(*), covar(ncvm,ncvm), chisq

      external func

      n_fit = n_c
      do i = 1, n_fit
         lista(i) = i
      end do

      call lfit (x, y, sig, n_p, coeff, n_c, lista, n_fit,
     $  covar, ncvm, chisq, func)

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c Routine LFIT from Numerical Recipes.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      SUBROUTINE LFIT(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,COVAR,
     $     NCVM,CHISQ,FUNCS)
      integer*4 mmax, ndata, ma, mfit, ncvm
      PARAMETER (MMAX=50)
      integer*4 lista(ma), kk, j, k, ihit, i
      real*8 X(NDATA),Y(NDATA),SIG(NDATA),A(MA),
     $     covar(NCVM,NCVM),BETA(MMAX),AFUNC(MMAX),
     $     ym, sig2i, wt, sum, chisq
      external funcs

	KK=MFIT+1
	DO J=1,MA
		IHIT=0
		DO K=1,MFIT
			IF (LISTA(K).EQ.J) IHIT=IHIT+1
		ENDDO
		IF (IHIT.EQ.0) THEN
			LISTA(KK)=J
			KK=KK+1
		ELSE IF (IHIT.GT.1) THEN
			PAUSE 'Improper set in LISTA'
		ENDIF
	ENDDO
	IF (KK.NE.(MA+1)) PAUSE 'Improper set in LISTA'
	DO J=1,MFIT
		DO K=1,MFIT
			COVAR(J,K)=0.
		ENDDO
		BETA(J)=0.
	ENDDO
	DO I=1,NDATA
		CALL FUNCS(X(I),AFUNC,MA)
		YM=Y(I)
		IF(MFIT.LT.MA) THEN
			DO J=MFIT+1,MA
				YM=YM-A(LISTA(J))*AFUNC(LISTA(J))
			ENDDO
		ENDIF
		SIG2I=1./SIG(I)**2
		DO J=1,MFIT
			WT=AFUNC(LISTA(J))*SIG2I
			DO K=1,J
				COVAR(J,K)=COVAR(J,K)+WT*AFUNC(LISTA(K))
			ENDDO
			BETA(J)=BETA(J)+YM*WT
		ENDDO
	ENDDO
	IF (MFIT.GT.1) THEN
		DO J=2,MFIT
			DO K=1,J-1
				COVAR(K,J)=COVAR(J,K)
			ENDDO
		ENDDO
	ENDIF
	CALL GAUSSJ(COVAR,MFIT,NCVM,BETA,1,1)
	DO J=1,MFIT
		A(LISTA(J))=BETA(J)
	ENDDO
	CHISQ=0.
	DO I=1,NDATA
		CALL FUNCS(X(I),AFUNC,MA)
		SUM=0.
		DO J=1,MA
			SUM=SUM+A(J)*AFUNC(J)
		ENDDO
		CHISQ=CHISQ+((Y(I)-SUM)/SIG(I))**2
	ENDDO
	CALL COVSRT(COVAR,NCVM,MA,LISTA,MFIT)
	RETURN
	END
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c Routine LFIT2 from LFIT from Numerical Recipes.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      SUBROUTINE LFIT2(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,COVAR,
     $     NCVM,CHISQ,FUNCS)
      integer*4 mmax, ndata, ma, mfit, ncvm
      PARAMETER (MMAX=50)
      integer*4 lista(ma), kk, j, k, ihit, i
      real*8 X(2,NDATA),Y(NDATA),SIG(NDATA),A(MA),
     $     covar(NCVM,NCVM),BETA(MMAX),AFUNC(MMAX),
     $     ym, sig2i, wt, sum, chisq
      external funcs

	KK=MFIT+1
	DO J=1,MA
		IHIT=0
		DO K=1,MFIT
			IF (LISTA(K).EQ.J) IHIT=IHIT+1
		ENDDO
		IF (IHIT.EQ.0) THEN
			LISTA(KK)=J
			KK=KK+1
		ELSE IF (IHIT.GT.1) THEN
			PAUSE 'Improper set in LISTA'
		ENDIF
	ENDDO
	IF (KK.NE.(MA+1)) PAUSE 'Improper set in LISTA'
	DO J=1,MFIT
		DO K=1,MFIT
			COVAR(J,K)=0.
		ENDDO
		BETA(J)=0.
	ENDDO
	DO I=1,NDATA
		CALL FUNCS(X(1,I),AFUNC,MA)
		YM=Y(I)
		IF(MFIT.LT.MA) THEN
			DO J=MFIT+1,MA
				YM=YM-A(LISTA(J))*AFUNC(LISTA(J))
			ENDDO
		ENDIF
		SIG2I=1./SIG(I)**2
		DO J=1,MFIT
			WT=AFUNC(LISTA(J))*SIG2I
			DO K=1,J
				COVAR(J,K)=COVAR(J,K)+WT*AFUNC(LISTA(K))
			ENDDO
			BETA(J)=BETA(J)+YM*WT
		ENDDO
	ENDDO
	IF (MFIT.GT.1) THEN
		DO J=2,MFIT
			DO K=1,J-1
				COVAR(K,J)=COVAR(J,K)
			ENDDO
		ENDDO
	ENDIF
	CALL GAUSSJ(COVAR,MFIT,NCVM,BETA,1,1)
	DO J=1,MFIT
		A(LISTA(J))=BETA(J)
	ENDDO
	CHISQ=0.
	DO I=1,NDATA
		CALL FUNCS(X(1,I),AFUNC,MA)
		SUM=0.
		DO J=1,MA
			SUM=SUM+A(J)*AFUNC(J)
		ENDDO
		CHISQ=CHISQ+((Y(I)-SUM)/SIG(I))**2
	ENDDO
	CALL COVSRT(COVAR,NCVM,MA,LISTA,MFIT)
	RETURN
	END
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c Routine LFITM from LFIT from Numerical Recipes.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      SUBROUTINE LFITM(X,Y,SIG,NDATA,ndim, deg,A,MA,LISTA,MFIT,COVAR,
     $     NCVM,CHISQ,FUNCS)
      integer*4 mmax, ndata, ma, mfit, ncvm, ndim, deg
      PARAMETER (MMAX=50)
      integer*4 lista(ma), kk, j, k, ihit, i
      real*8 X(ndim,NDATA),Y(NDATA),SIG(NDATA),A(MA),
     $     covar(NCVM,NCVM),BETA(MMAX),AFUNC(MMAX),
     $     ym, sig2i, wt, sum, chisq
      external funcs

	KK=MFIT+1
	DO J=1,MA
		IHIT=0
		DO K=1,MFIT
			IF (LISTA(K).EQ.J) IHIT=IHIT+1
		ENDDO
		IF (IHIT.EQ.0) THEN
			LISTA(KK)=J
			KK=KK+1
		ELSE IF (IHIT.GT.1) THEN
			PAUSE 'Improper set in LISTA'
		ENDIF
	ENDDO
	IF (KK.NE.(MA+1)) PAUSE 'Improper set in LISTA'
	DO J=1,MFIT
		DO K=1,MFIT
			COVAR(J,K)=0.
		ENDDO
		BETA(J)=0.
	ENDDO
	DO I=1,NDATA
		CALL FUNCS(X(1,I),AFUNC,MA, ndim, deg)
		YM=Y(I)
		IF(MFIT.LT.MA) THEN
			DO J=MFIT+1,MA
				YM=YM-A(LISTA(J))*AFUNC(LISTA(J))
			ENDDO
		ENDIF
		SIG2I=1./SIG(I)**2
		DO J=1,MFIT
			WT=AFUNC(LISTA(J))*SIG2I
			DO K=1,J
				COVAR(J,K)=COVAR(J,K)+WT*AFUNC(LISTA(K))
			ENDDO
			BETA(J)=BETA(J)+YM*WT
		ENDDO
	ENDDO
	IF (MFIT.GT.1) THEN
		DO J=2,MFIT
			DO K=1,J-1
				COVAR(K,J)=COVAR(J,K)
			ENDDO
		ENDDO
	ENDIF
	CALL GAUSSJ(COVAR,MFIT,NCVM,BETA,1,1)
	DO J=1,MFIT
		A(LISTA(J))=BETA(J)
	ENDDO
	CHISQ=0.
	DO I=1,NDATA
		CALL FUNCS(X(1,I),AFUNC,MA, ndim, deg)
		SUM=0.
		DO J=1,MA
			SUM=SUM+A(J)*AFUNC(J)
		ENDDO
		CHISQ=CHISQ+((Y(I)-SUM)/SIG(I))**2
	ENDDO
	CALL COVSRT(COVAR,NCVM,MA,LISTA,MFIT)
	RETURN
	END
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c Routine COVSRT from Numerical Recipes.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      SUBROUTINE COVSRT(COVAR,NCVM,MA,LISTA,MFIT)
      integer*4 ncvm, ma, mfit, lista(mfit), j, i
      real*8 COVAR(NCVM,NCVM), swap

	DO 20 J=1,MA-1
		DO 10 I=J+1,MA
			COVAR(I,J)=0.
  10		continue
  20	continue
	DO 40 I=1,MFIT-1
		DO 30 J=I+1,MFIT
			IF(LISTA(J).GT.LISTA(I)) THEN
				COVAR(LISTA(J),LISTA(I))=COVAR(I,J)
			ELSE
				COVAR(LISTA(I),LISTA(J))=COVAR(I,J)
			ENDIF
   30		continue
   40	continue
	SWAP=COVAR(1,1)
	DO 50 J=1,MA
		COVAR(1,J)=COVAR(J,J)
		COVAR(J,J)=0.
   50	continue
	COVAR(LISTA(1),LISTA(1))=SWAP
	DO 60 J=2,MFIT
		COVAR(LISTA(J),LISTA(J))=COVAR(1,J)
   60	continue
	DO 80 J=2,MA
		DO 70 I=1,J-1
			COVAR(I,J)=COVAR(J,I)
   70		continue
   80	continue
	RETURN
	END
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c Routine GAUSSJ from Numerical Recipes.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
      integer*4 n, np, m, mp, nmax, j, i, k, icol, irow, l, ll
      PARAMETER (NMAX=50)
      integer*4 IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      real*8 A(NP,NP),B(NP,MP), big, dum, pivinv

      DO 10  J=1,N
         IPIV(J)=0
 10   CONTINUE
	DO 110 I=1,N
           BIG=0.
           DO 30 J=1,N
              IF(IPIV(J).NE.1)THEN
                 DO 20 K=1,N
                    IF (IPIV(K).EQ.0) THEN
                       IF (ABS(A(J,K)).GE.BIG)THEN
                          BIG=ABS(A(J,K))
                          IROW=J
                          ICOL=K
                       ENDIF
                    ELSE IF (IPIV(K).GT.1) THEN
                       PAUSE 'Singular matrix'
                    ENDIF
 20              CONTINUE
              ENDIF
 30        CONTINUE
           IPIV(ICOL)=IPIV(ICOL)+1
           IF (IROW.NE.ICOL) THEN
              DO 40 L=1,N
                 DUM=A(IROW,L)
                 A(IROW,L)=A(ICOL,L)
                 A(ICOL,L)=DUM
 40           CONTINUE
              DO 50 L=1,M
                 DUM=B(IROW,L)
                 B(IROW,L)=B(ICOL,L)
                 B(ICOL,L)=DUM
 50           CONTINUE
           ENDIF
           INDXR(I)=IROW
           INDXC(I)=ICOL
           IF (A(ICOL,ICOL).EQ.0.) PAUSE 'Singular matrix.'
           PIVINV=1./A(ICOL,ICOL)
           A(ICOL,ICOL)=1.
           DO 60 L=1,N
              A(ICOL,L)=A(ICOL,L)*PIVINV
 60        CONTINUE
           DO 70 L=1,M
              B(ICOL,L)=B(ICOL,L)*PIVINV
 70        CONTINUE
           DO 100 LL=1,N
              IF(LL.NE.ICOL)THEN
                 DUM=A(LL,ICOL)
                 A(LL,ICOL)=0.
                 DO 80 L=1,N
                    A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
 80              CONTINUE
                 DO 90 L=1,M
                    B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
 90              CONTINUE
              ENDIF
 100       CONTINUE
 110    CONTINUE
	DO 130 L=N,1,-1
           IF(INDXR(L).NE.INDXC(L))THEN
              DO 120 K=1,N
                 DUM=A(K,INDXR(L))
                 A(K,INDXR(L))=A(K,INDXC(L))
                 A(K,INDXC(L))=DUM
 120          CONTINUE
           ENDIF
 130    CONTINUE
	RETURN
	END
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine computes the terms of each coefficient.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine trans (x, mat, n_c)

      implicit none

      integer*4
     $  n_c

      real*8
     $  x(2), mat(*)

      mat(1) = 1.
      if (n_c .gt. 1) then
         mat(2) = x(1)
         mat(3) = x(2)
      end if

      if (n_c .gt. 3) then
         mat(4) = x(1)**2
         mat(5) = x(1)*x(2)
         mat(6) = x(2)**2
      end if

      if (n_c .gt. 6) then
         mat(7) = mat(4)*x(1)
         mat(8) = mat(4)*x(2)
         mat(9) = mat(6)*x(1)
         mat(10) = mat(6)*x(2)
      end if

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine computes the values of the monomes at x.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine polyno (x, afunc, ma, ndim, deg)

      integer*4
     $  ma, ndim, deg, i, p, in, out, stpt,
     $  ist(100), pst(100), inst(100), outst(0:100),
     $  ppst(0:100), iinst(0:100), iist(0:100)

      real*8
     $  x(ndim), afunc(ma)

      afunc(1) = 1.d0
      i = 1
      p = 0
      in = 1
      out = 2
      stpt = 1
      ist(1) = i
      pst(1) = p
      inst(1) = in
      outst(1) = out

 1001 continue
         ppst(stpt) = pst(stpt) + 1
         iinst(stpt) = outst(stpt)
         afunc(outst(stpt)) = afunc(inst(stpt))*x(ist(stpt))
         outst(stpt) = outst(stpt) + 1
         iist(stpt) = ist(stpt)
 1002    continue
            if (ppst(stpt) .lt. deg) then
               stpt = stpt + 1
               ist(stpt) = iist(stpt-1)
               pst(stpt) = ppst(stpt-1)
               inst(stpt) = iinst(stpt-1)
               outst(stpt) = outst(stpt-1)
               goto 1001
            else
               iist(stpt-1) = ist(stpt)
               ppst(stpt-1) = pst(stpt)
               iinst(stpt-1) = inst(stpt)
               outst(stpt-1) = outst(stpt)
               stpt = stpt - 1
            end if
 1003       continue
            iist(stpt) = iist(stpt ) + 1
         if (iist(stpt) .le. ndim) goto 1002
         if (stpt .ge. 0) then
            iist(stpt-1) = ist(stpt)
            ppst(stpt-1) = pst(stpt)
            iinst(stpt-1) = inst(stpt)
            outst(stpt-1) = outst(stpt)
            stpt = stpt - 1
            goto 1003
         end if
 1004    continue

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine computes the X and Y transformations.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine trans_xy (x, y, n, xt, yt, coeff, n_c, func)

      implicit none

      integer*4
     $  i, j, n, n_c

      real*4
     $  x(*), y(*), xt(*), yt(*)

      real*8
     $  coeff(*), pt(2), afunc(20), x1, y1

      external func

      do i = 1, n
         pt(1) = x(i)
         pt(2) = y(i)
         call func (pt, afunc, n_c)
         x1 = 0.
         y1 = 0.
         do j = 1, n_c
            x1 = x1 + coeff(j)*afunc(j)
            y1 = y1 + coeff(n_c+j)*afunc(j)
         end do
         xt(i) = x1
         yt(i) = y1
      end do

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine computes the terms of each coefficient.
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
      subroutine monomes (x, mat, n_c)

      implicit none

      integer*4
     $  n_c, i

      real*8
     $  x, mat(*)

      mat(1) = 1.d0
      do i = 2, n_c
         mat(i) = mat(i-1)*x
      end do

      return
      end
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c This routine computes the 
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
c-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
