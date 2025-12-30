C     TEST_INTRV_HVNRM - TEST SLATEC INTRV AND HVNRM
C     ON IBM SYSTEM/360 FORTRAN G
C
      REAL HVNRM, RC
      REAL V(10), XT(10), X, RESULT, EXPECT, ERR
      INTEGER I, ILO, ILEFT, MFLAG, IER
C
      WRITE(6,100)
  100 FORMAT(' SLATEC INTRV, HVNRM, AND RC TEST')
      WRITE(6,110)
  110 FORMAT(' ================================='/)
C
C     ============================================================
C     TEST HVNRM - MAXIMUM NORM (INFINITY NORM)
C     ============================================================
      WRITE(6,200)
  200 FORMAT(' HVNRM TESTS (MAXIMUM/INFINITY NORM):')
C
C     HVNRM([1,2,3,4,5]) = 5
      DO 210 I = 1, 5
         V(I) = I
  210 CONTINUE
      RESULT = HVNRM(V, 5)
      EXPECT = 5.0
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,220) RESULT, EXPECT, ERR
  220 FORMAT('   HVNRM([1,2,3,4,5])=',F10.4/
     1       '                 EXP=',F10.4,' ERR=',E10.2)
C
C     HVNRM([-7,3,5,-2,1]) = 7 (ABSOLUTE VALUE)
      V(1) = -7.0
      V(2) = 3.0
      V(3) = 5.0
      V(4) = -2.0
      V(5) = 1.0
      RESULT = HVNRM(V, 5)
      EXPECT = 7.0
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,230) RESULT, EXPECT, ERR
  230 FORMAT('   HVNRM([-7,3,5,-2,1])=',F10.4/
     1       '                   EXP=',F10.4,' ERR=',E10.2)
C
C     HVNRM([0.1,0.2,0.3]) = 0.3
      V(1) = 0.1
      V(2) = 0.2
      V(3) = 0.3
      RESULT = HVNRM(V, 3)
      EXPECT = 0.3
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,240) RESULT, EXPECT, ERR
  240 FORMAT('   HVNRM([0.1,0.2,0.3])=',F10.4/
     1       '                   EXP=',F10.4,' ERR=',E10.2)
C
      WRITE(6,120)
  120 FORMAT(' ')
C
C     ============================================================
C     TEST INTRV - INTERVAL SEARCH FOR B-SPLINES
C     ============================================================
      WRITE(6,300)
  300 FORMAT(' INTRV TESTS (INTERVAL SEARCH):')
C
C     SET UP KNOT VECTOR: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
      DO 310 I = 1, 10
         XT(I) = I
  310 CONTINUE
C
C     FIND INTERVAL FOR X = 3.5 (SHOULD BE ILEFT=3, MFLAG=0)
      X = 3.5
      ILO = 1
      CALL INTRV(XT, 10, X, ILO, ILEFT, MFLAG)
      WRITE(6,320) X, ILEFT, MFLAG
  320 FORMAT('   X=',F5.1,' ILEFT=',I2,' MFLAG=',I2,
     1       ' (EXPECT ILEFT=3, MFLAG=0)')
C
C     FIND INTERVAL FOR X = 7.0 (SHOULD BE ILEFT=7, MFLAG=0)
      X = 7.0
      ILO = 1
      CALL INTRV(XT, 10, X, ILO, ILEFT, MFLAG)
      WRITE(6,330) X, ILEFT, MFLAG
  330 FORMAT('   X=',F5.1,' ILEFT=',I2,' MFLAG=',I2,
     1       ' (EXPECT ILEFT=7, MFLAG=0)')
C
C     FIND INTERVAL FOR X = 0.5 (BELOW RANGE, MFLAG=-1)
      X = 0.5
      ILO = 1
      CALL INTRV(XT, 10, X, ILO, ILEFT, MFLAG)
      WRITE(6,340) X, ILEFT, MFLAG
  340 FORMAT('   X=',F5.1,' ILEFT=',I2,' MFLAG=',I2,
     1       ' (EXPECT ILEFT=1, MFLAG=-1)')
C
C     FIND INTERVAL FOR X = 10.5 (ABOVE RANGE, MFLAG=1)
      X = 10.5
      ILO = 1
      CALL INTRV(XT, 10, X, ILO, ILEFT, MFLAG)
      WRITE(6,350) X, ILEFT, MFLAG
  350 FORMAT('   X=',F5.1,' ILEFT=',I2,' MFLAG=',I2,
     1       ' (EXPECT ILEFT=10, MFLAG=1)')
C
C     TEST SEQUENTIAL SEARCH (EXPLOIT ILO CACHING)
      WRITE(6,360)
  360 FORMAT('   SEQUENTIAL SEARCH TEST:')
      ILO = 1
      DO 370 I = 1, 5
         X = I + 0.5
         CALL INTRV(XT, 10, X, ILO, ILEFT, MFLAG)
         WRITE(6,380) X, ILEFT, ILO
  370 CONTINUE
  380 FORMAT('     X=',F5.1,' ILEFT=',I2,' (ILO NOW=',I2,')')
C
      WRITE(6,120)
C
C     ============================================================
C     TEST RC - CARLSON ELLIPTIC INTEGRAL
C     ============================================================
      WRITE(6,400)
  400 FORMAT(' RC TESTS (CARLSON ELLIPTIC INTEGRAL):')
C
C     RC(0, 0.25) = PI (SPECIAL IDENTITY!)
      RESULT = RC(0.0, 0.25, IER)
      EXPECT = 3.1415927
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,410) RESULT, EXPECT, ERR, IER
  410 FORMAT('   RC(0, 0.25)=',F12.7,' (THIS IS PI!)'/
     1       '          EXP=',F12.7,' ERR=',E10.2,' IER=',I2)
C
C     ARCTAN(1) = 1 * RC(1, 1+1) = RC(1,2) = PI/4
      RESULT = RC(1.0, 2.0, IER)
      EXPECT = 0.7853982
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,420) RESULT, EXPECT, ERR, IER
  420 FORMAT('   RC(1, 2)=',F12.7,' (ARCTAN(1)=PI/4)'/
     1       '       EXP=',F12.7,' ERR=',E10.2,' IER=',I2)
C
C     RC(1/16, 1/8) = PI (ANOTHER IDENTITY)
      RESULT = RC(0.0625, 0.125, IER)
      EXPECT = 3.1415927
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,430) RESULT, EXPECT, ERR, IER
  430 FORMAT('   RC(1/16, 1/8)=',F12.7,' (ALSO PI!)'/
     1       '            EXP=',F12.7,' ERR=',E10.2,' IER=',I2)
C
C     LN(2) VIA RC: (2-1)*RC(((1+2)/2)^2, 2) = RC(2.25, 2)
      RESULT = RC(2.25, 2.0, IER)
      EXPECT = 0.6931472
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,440) RESULT, EXPECT, ERR, IER
  440 FORMAT('   RC(9/4, 2)=',F12.7,' (LN(2))'/
     1       '          EXP=',F12.7,' ERR=',E10.2,' IER=',I2)
C
      WRITE(6,120)
      WRITE(6,500)
  500 FORMAT(' SLATEC INTRV, HVNRM, AND RC TEST COMPLETE')
      STOP
      END
C
C     ============================================================
C     HVNRM - MAXIMUM NORM (INFINITY NORM)
C     ============================================================
C     DECK HVNRM
      FUNCTION HVNRM (V, NCOMP)
C***BEGIN PROLOGUE  HVNRM
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DEABM, DEBDF and DERKF
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (HVNRM-S, DHVNRM-D)
C***AUTHOR  Watts, H. A., (SNLA)
C***END PROLOGUE  HVNRM
      DIMENSION V(NCOMP)
C***FIRST EXECUTABLE STATEMENT  HVNRM
      HVNRM=0.
      DO 10 K=1,NCOMP
   10   HVNRM=AMAX1(HVNRM,ABS(V(K)))
      RETURN
      END
C
C     ============================================================
C     INTRV - INTERVAL SEARCH FOR B-SPLINES
C     ============================================================
C     DECK INTRV
      SUBROUTINE INTRV (XT, LXT, X, ILO, ILEFT, MFLAG)
C***BEGIN PROLOGUE  INTRV
C***PURPOSE  Compute the largest integer ILEFT in 1 .LE. ILEFT .LE. LXT
C            such that XT(ILEFT) .LE. X where XT(*) is a subdivision
C            of the X interval.
C***LIBRARY   SLATEC
C***AUTHOR  Amos, D. E., (SNLA)
C            Written by Carl de Boor and modified by D. E. Amos
C***END PROLOGUE  INTRV
C
      INTEGER IHI, ILEFT, ILO, ISTEP, LXT, MFLAG, MIDDLE
      REAL X, XT
      DIMENSION XT(LXT)
C***FIRST EXECUTABLE STATEMENT  INTRV
      IHI = ILO + 1
      IF (IHI.LT.LXT) GO TO 10
      IF (X.GE.XT(LXT)) GO TO 110
      IF (LXT.LE.1) GO TO 90
      ILO = LXT - 1
      IHI = LXT
C
   10 IF (X.GE.XT(IHI)) GO TO 40
      IF (X.GE.XT(ILO)) GO TO 100
C
C *** NOW X .LT. XT(IHI) . FIND LOWER BOUND
      ISTEP = 1
   20 IHI = ILO
      ILO = IHI - ISTEP
      IF (ILO.LE.1) GO TO 30
      IF (X.GE.XT(ILO)) GO TO 70
      ISTEP = ISTEP*2
      GO TO 20
   30 ILO = 1
      IF (X.LT.XT(1)) GO TO 90
      GO TO 70
C *** NOW X .GE. XT(ILO) . FIND UPPER BOUND
   40 ISTEP = 1
   50 ILO = IHI
      IHI = ILO + ISTEP
      IF (IHI.GE.LXT) GO TO 60
      IF (X.LT.XT(IHI)) GO TO 70
      ISTEP = ISTEP*2
      GO TO 50
   60 IF (X.GE.XT(LXT)) GO TO 110
      IHI = LXT
C
C *** NOW XT(ILO) .LE. X .LT. XT(IHI) . NARROW THE INTERVAL
   70 MIDDLE = (ILO+IHI)/2
      IF (MIDDLE.EQ.ILO) GO TO 100
C     NOTE. IT IS ASSUMED THAT MIDDLE = ILO IN CASE IHI = ILO+1
      IF (X.LT.XT(MIDDLE)) GO TO 80
      ILO = MIDDLE
      GO TO 70
   80 IHI = MIDDLE
      GO TO 70
C *** SET OUTPUT AND RETURN
   90 MFLAG = -1
      ILEFT = 1
      RETURN
  100 MFLAG = 0
      ILEFT = ILO
      RETURN
  110 MFLAG = 1
      ILEFT = LXT
      RETURN
      END
C
C     ============================================================
C     RC - CARLSON ELLIPTIC INTEGRAL (SIMPLIFIED FOR FORTRAN IV)
C     ============================================================
C     DECK RC
      REAL FUNCTION RC (X, Y, IER)
C***BEGIN PROLOGUE  RC
C***PURPOSE  Calculate an approximation to
C             RC(X,Y) = Integral from zero to infinity of
C                              -1/2     -1
C                    (1/2)(t+X)    (t+Y)  dt,
C            where X is nonnegative and Y is positive.
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (RC-S, DRC-D)
C***AUTHOR  Carlson, B. C.
C             Ames Laboratory-DOE, Iowa State University
C***END PROLOGUE  RC
      INTEGER IER
      REAL C1, C2, ERRTOL, LAMDA, LOLIM
      REAL MU, S, SN, UPLIM, X, XN, Y, YN
C
C     IBM 360/370 CONSTANTS
      DATA ERRTOL / 0.0012E0 /
      DATA LOLIM  / 3.0E-78 /
      DATA UPLIM  / 1.0E+75 /
      DATA C1 / 0.142857143E0 /
      DATA C2 / 0.409090909E0 /
C
C***FIRST EXECUTABLE STATEMENT  RC
C
C     CHECK FOR VALID ARGUMENTS
C
      RC = 0.0E0
      IF (X.LT.0.0E0.OR.Y.LE.0.0E0) GO TO 90
      IF (AMAX1(X,Y).GT.UPLIM) GO TO 92
      IF (X+Y.LT.LOLIM) GO TO 91
C
      IER = 0
      XN = X
      YN = Y
C
C     DUPLICATION THEOREM ITERATION
C
   30 MU = (XN+YN+YN)/3.0E0
      SN = (YN+MU)/MU - 2.0E0
      IF (ABS(SN).LT.ERRTOL) GO TO 40
      LAMDA = 2.0E0*SQRT(XN)*SQRT(YN) + YN
      XN = (XN+LAMDA)*0.250E0
      YN = (YN+LAMDA)*0.250E0
      GO TO 30
C
C     TAYLOR SERIES EXPANSION
C
   40 S = SN*SN*(0.30E0+SN*(C1+SN*(0.3750E0+SN*C2)))
      RC = (1.0E0+S)/SQRT(MU)
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
