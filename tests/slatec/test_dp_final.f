C     TEST_DP_FINAL - FINAL DOUBLE PRECISION SLATEC TESTS
C     DRC, DHVNRM, DVNRMS ON IBM SYSTEM/360 FORTRAN G
C
      DOUBLE PRECISION DRC, DHVNRM, DVNRMS
      DOUBLE PRECISION V(10), W(10), RESULT, EXPECT, ERR
      DOUBLE PRECISION PI, LN2, PIFOUR
      INTEGER I, IER
C
      WRITE(6,100)
  100 FORMAT(' FINAL DOUBLE PRECISION SLATEC TESTS')
      WRITE(6,110)
  110 FORMAT(' ===================================='/)
C
C     HIGH PRECISION CONSTANTS
      PI = 3.14159265358979324D0
      LN2 = 0.69314718055994531D0
      PIFOUR = 0.78539816339744831D0
C
C     ============================================================
C     TEST DHVNRM - DOUBLE PRECISION MAXIMUM NORM
C     ============================================================
      WRITE(6,200)
  200 FORMAT(' DHVNRM TESTS (DP MAXIMUM NORM):')
C
C     DHVNRM([1,2,3,4,5]) = 5
      DO 210 I = 1, 5
         V(I) = I
  210 CONTINUE
      RESULT = DHVNRM(V, 5)
      EXPECT = 5.0D0
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,220) RESULT, EXPECT, ERR
  220 FORMAT('   DHVNRM([1,2,3,4,5])=',D18.10/
     1       '                  EXP=',D18.10,' ERR=',D10.2)
C
C     DHVNRM([-7,3,5,-2,1]) = 7
      V(1) = -7.0D0
      V(2) = 3.0D0
      V(3) = 5.0D0
      V(4) = -2.0D0
      V(5) = 1.0D0
      RESULT = DHVNRM(V, 5)
      EXPECT = 7.0D0
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,230) RESULT, EXPECT, ERR
  230 FORMAT('   DHVNRM([-7,3,5,-2,1])=',D18.10/
     1       '                    EXP=',D18.10,' ERR=',D10.2)
C
      WRITE(6,120)
  120 FORMAT(' ')
C
C     ============================================================
C     TEST DVNRMS - DOUBLE PRECISION WEIGHTED RMS NORM
C     ============================================================
      WRITE(6,300)
  300 FORMAT(' DVNRMS TESTS (DP WEIGHTED RMS NORM):')
C
C     DVNRMS([1,1,1,1,1], [1,1,1,1,1]) = 1
      DO 310 I = 1, 5
         V(I) = 1.0D0
         W(I) = 1.0D0
  310 CONTINUE
      RESULT = DVNRMS(5, V, W)
      EXPECT = 1.0D0
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,320) RESULT, EXPECT, ERR
  320 FORMAT('   DVNRMS([1,1,1,1,1],[1,1,1,1,1])=',D18.10/
     1       '                              EXP=',D18.10,' ERR=',D10.2)
C
C     DVNRMS([3,4], [1,1]) = SQRT(12.5) = 3.5355339...
      V(1) = 3.0D0
      V(2) = 4.0D0
      W(1) = 1.0D0
      W(2) = 1.0D0
      RESULT = DVNRMS(2, V, W)
      EXPECT = 3.5355339059327376D0
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,330) RESULT, EXPECT, ERR
  330 FORMAT('   DVNRMS([3,4],[1,1])=',D18.10/
     1       '                  EXP=',D18.10,' ERR=',D10.2)
C
      WRITE(6,120)
C
C     ============================================================
C     TEST DRC - DOUBLE PRECISION CARLSON ELLIPTIC INTEGRAL
C     ============================================================
      WRITE(6,400)
  400 FORMAT(' DRC TESTS (DP CARLSON ELLIPTIC INTEGRAL):')
C
C     DRC(0, 0.25) = PI
      RESULT = DRC(0.0D0, 0.25D0, IER)
      EXPECT = PI
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,410) RESULT, EXPECT, ERR, IER
  410 FORMAT('   DRC(0, 0.25)=',D22.15,' (PI!)'/
     1       '           EXP=',D22.15/
     2       '           ERR=',D10.2,' IER=',I2)
C
C     DRC(1, 2) = ARCTAN(1) = PI/4
      RESULT = DRC(1.0D0, 2.0D0, IER)
      EXPECT = PIFOUR
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,420) RESULT, EXPECT, ERR, IER
  420 FORMAT('   DRC(1, 2)=',D22.15,' (PI/4)'/
     1       '        EXP=',D22.15/
     2       '        ERR=',D10.2,' IER=',I2)
C
C     DRC(1/16, 1/8) = PI (ALTERNATE FORM)
      RESULT = DRC(0.0625D0, 0.125D0, IER)
      EXPECT = PI
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,430) RESULT, EXPECT, ERR, IER
  430 FORMAT('   DRC(1/16, 1/8)=',D22.15,' (PI!)'/
     1       '             EXP=',D22.15/
     2       '             ERR=',D10.2,' IER=',I2)
C
C     DRC(9/4, 2) = LN(2)
      RESULT = DRC(2.25D0, 2.0D0, IER)
      EXPECT = LN2
      ERR = DABS(RESULT - EXPECT)
      WRITE(6,440) RESULT, EXPECT, ERR, IER
  440 FORMAT('   DRC(9/4, 2)=',D22.15,' (LN2)'/
     1       '          EXP=',D22.15/
     2       '          ERR=',D10.2,' IER=',I2)
C
      WRITE(6,120)
      WRITE(6,500)
  500 FORMAT(' FINAL DOUBLE PRECISION TESTS COMPLETE')
      WRITE(6,510)
  510 FORMAT(' ALL SLATEC VERIFICATION TESTS COMPLETE!')
      STOP
      END
C
C     ============================================================
C     DHVNRM - DOUBLE PRECISION MAXIMUM NORM
C     ============================================================
C     DECK DHVNRM
      DOUBLE PRECISION FUNCTION DHVNRM (V, NCOMP)
C***BEGIN PROLOGUE  DHVNRM
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DDEABM, DDEBDF and DDERKF
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (HVNRM-S, DHVNRM-D)
C***AUTHOR  Watts, H. A., (SNLA)
C***END PROLOGUE  DHVNRM
C
      INTEGER K, NCOMP
      DOUBLE PRECISION V
      DIMENSION V(NCOMP)
C***FIRST EXECUTABLE STATEMENT  DHVNRM
      DHVNRM = 0.0D0
      DO 10 K = 1, NCOMP
         DHVNRM = DMAX1(DHVNRM,DABS(V(K)))
   10 CONTINUE
      RETURN
      END
C
C     ============================================================
C     DVNRMS - DOUBLE PRECISION WEIGHTED RMS NORM
C     ============================================================
C     DECK DVNRMS
      DOUBLE PRECISION FUNCTION DVNRMS (N, V, W)
C***BEGIN PROLOGUE  DVNRMS
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DDEBDF
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (VNWRMS-S, DVNRMS-D)
C***AUTHOR  (UNKNOWN)
C***END PROLOGUE  DVNRMS
      INTEGER I, N
      DOUBLE PRECISION SUM, V, W
      DIMENSION V(N),W(N)
C***FIRST EXECUTABLE STATEMENT  DVNRMS
      SUM = 0.0D0
      DO 10 I = 1, N
         SUM = SUM + (V(I)/W(I))**2
   10 CONTINUE
      DVNRMS = DSQRT(SUM/N)
      RETURN
      END
C
C     ============================================================
C     DRC - DOUBLE PRECISION CARLSON ELLIPTIC INTEGRAL
C     ============================================================
C     DECK DRC
      DOUBLE PRECISION FUNCTION DRC (X, Y, IER)
C***BEGIN PROLOGUE  DRC
C***PURPOSE  Calculate a double precision approximation to
C             DRC(X,Y) = Integral from zero to infinity of
C                              -1/2     -1
C                    (1/2)(t+X)    (t+Y)  dt,
C            where X is nonnegative and Y is positive.
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (RC-S, DRC-D)
C***AUTHOR  Carlson, B. C., Ames Laboratory-DOE, Iowa State University
C***END PROLOGUE  DRC
      INTEGER IER
      DOUBLE PRECISION C1, C2, ERRTOL, LAMDA, LOLIM
      DOUBLE PRECISION MU, S, SN, UPLIM, X, XN, Y, YN
C
C     IBM 360/370 CONSTANTS (DOUBLE PRECISION)
      DATA ERRTOL / 0.00028D0 /
      DATA LOLIM  / 3.0D-78 /
      DATA UPLIM  / 1.0D+75 /
      DATA C1 / 0.14285714285714286D0 /
      DATA C2 / 0.40909090909090909D0 /
C
C***FIRST EXECUTABLE STATEMENT  DRC
C
C     CHECK FOR VALID ARGUMENTS
C
      DRC = 0.0D0
      IF (X.LT.0.0D0.OR.Y.LE.0.0D0) GO TO 90
      IF (DMAX1(X,Y).GT.UPLIM) GO TO 92
      IF (X+Y.LT.LOLIM) GO TO 91
C
      IER = 0
      XN = X
      YN = Y
C
C     DUPLICATION THEOREM ITERATION
C
   30 MU = (XN+YN+YN)/3.0D0
      SN = (YN+MU)/MU - 2.0D0
      IF (DABS(SN).LT.ERRTOL) GO TO 40
      LAMDA = 2.0D0*DSQRT(XN)*DSQRT(YN) + YN
      XN = (XN+LAMDA)*0.250D0
      YN = (YN+LAMDA)*0.250D0
      GO TO 30
C
C     TAYLOR SERIES EXPANSION
C
   40 S = SN*SN*(0.30D0+SN*(C1+SN*(0.3750D0+SN*C2)))
      DRC = (1.0D0+S)/DSQRT(MU)
      RETURN
C
C     ERROR RETURNS
C
   90 IER = 1
      RETURN
   91 IER = 2
      RETURN
   92 IER = 3
      RETURN
      END
