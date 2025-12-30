C     TEST_COMPLEX - TEST SLATEC COMPLEX UTILITIES
C     CDIV, CSROOT, AND DENORM ON IBM SYSTEM/360 FORTRAN G
C
      REAL PYTHAG
      DOUBLE PRECISION DENORM
      REAL AR, AI, BR, BI, CR, CI
      REAL XR, XI, YR, YI
      REAL ER, EI, ERRR, ERRI
      DOUBLE PRECISION DX(5), DRESLT, DEXPCT, DERR
      INTEGER I
C
      WRITE(6,100)
  100 FORMAT(' SLATEC COMPLEX UTILITIES TEST')
      WRITE(6,110)
  110 FORMAT(' =============================='/)
C
C     ============================================================
C     TEST CDIV - COMPLEX DIVISION
C     ============================================================
      WRITE(6,200)
  200 FORMAT(' CDIV TESTS (COMPLEX DIVISION):')
C
C     (2+3I)/(1+1I) = (2*1+3*1 + I*(3*1-2*1))/(1+1) = (5+I)/2
C     = 2.5 + 0.5I
      AR = 2.0
      AI = 3.0
      BR = 1.0
      BI = 1.0
      CALL CDIV(AR, AI, BR, BI, CR, CI)
      ER = 2.5
      EI = 0.5
      ERRR = ABS(CR - ER)
      ERRI = ABS(CI - EI)
      WRITE(6,210) AR, AI, BR, BI, CR, CI, ER, EI, ERRR, ERRI
  210 FORMAT('   (',F5.1,'+',F5.1,'I)/(',F5.1,'+',F5.1,'I)'/
     1       '     = (',F8.4,'+',F8.4,'I)'/
     2       '     EXP=(',F8.4,'+',F8.4,'I)'/
     3       '     ERR=',E10.2,' +',E10.2,'I')
C
C     (4+0I)/(2+0I) = 2+0I (REAL DIVISION)
      AR = 4.0
      AI = 0.0
      BR = 2.0
      BI = 0.0
      CALL CDIV(AR, AI, BR, BI, CR, CI)
      ER = 2.0
      EI = 0.0
      ERRR = ABS(CR - ER)
      ERRI = ABS(CI - EI)
      WRITE(6,210) AR, AI, BR, BI, CR, CI, ER, EI, ERRR, ERRI
C
C     (1+1I)/(0+1I) = (1+1I)*(-I)/(1) = -I+1 = 1-I
      AR = 1.0
      AI = 1.0
      BR = 0.0
      BI = 1.0
      CALL CDIV(AR, AI, BR, BI, CR, CI)
      ER = 1.0
      EI = -1.0
      ERRR = ABS(CR - ER)
      ERRI = ABS(CI - EI)
      WRITE(6,210) AR, AI, BR, BI, CR, CI, ER, EI, ERRR, ERRI
C
C     (3+4I)/(3-4I) = (9-16+24I)/(9+16) = (-7+24I)/25
      AR = 3.0
      AI = 4.0
      BR = 3.0
      BI = -4.0
      CALL CDIV(AR, AI, BR, BI, CR, CI)
      ER = -0.28
      EI = 0.96
      ERRR = ABS(CR - ER)
      ERRI = ABS(CI - EI)
      WRITE(6,210) AR, AI, BR, BI, CR, CI, ER, EI, ERRR, ERRI
C
      WRITE(6,120)
  120 FORMAT(' ')
C
C     ============================================================
C     TEST CSROOT - COMPLEX SQUARE ROOT
C     ============================================================
      WRITE(6,300)
  300 FORMAT(' CSROOT TESTS (COMPLEX SQUARE ROOT):')
C
C     SQRT(4+0I) = 2+0I
      XR = 4.0
      XI = 0.0
      CALL CSROOT(XR, XI, YR, YI)
      ER = 2.0
      EI = 0.0
      ERRR = ABS(YR - ER)
      ERRI = ABS(YI - EI)
      WRITE(6,310) XR, XI, YR, YI, ER, EI, ERRR, ERRI
  310 FORMAT('   SQRT(',F7.2,'+',F7.2,'I)'/
     1       '     = (',F10.6,'+',F10.6,'I)'/
     2       '     EXP=(',F10.6,'+',F10.6,'I)'/
     3       '     ERR=',E10.2,' +',E10.2,'I')
C
C     SQRT(-1+0I) = 0+1I
      XR = -1.0
      XI = 0.0
      CALL CSROOT(XR, XI, YR, YI)
      ER = 0.0
      EI = 1.0
      ERRR = ABS(YR - ER)
      ERRI = ABS(YI - EI)
      WRITE(6,310) XR, XI, YR, YI, ER, EI, ERRR, ERRI
C
C     SQRT(0+4I) = SQRT(2)+SQRT(2)I = 1.4142...+1.4142...I
      XR = 0.0
      XI = 4.0
      CALL CSROOT(XR, XI, YR, YI)
      ER = 1.4142136
      EI = 1.4142136
      ERRR = ABS(YR - ER)
      ERRI = ABS(YI - EI)
      WRITE(6,310) XR, XI, YR, YI, ER, EI, ERRR, ERRI
C
C     SQRT(3+4I) = 2+1I (SINCE (2+I)^2 = 4+4I-1 = 3+4I)
      XR = 3.0
      XI = 4.0
      CALL CSROOT(XR, XI, YR, YI)
      ER = 2.0
      EI = 1.0
      ERRR = ABS(YR - ER)
      ERRI = ABS(YI - EI)
      WRITE(6,310) XR, XI, YR, YI, ER, EI, ERRR, ERRI
C
C     SQRT(0-4I) = SQRT(2)-SQRT(2)I (NEGATIVE IMAGINARY)
      XR = 0.0
      XI = -4.0
      CALL CSROOT(XR, XI, YR, YI)
      ER = 1.4142136
      EI = -1.4142136
      ERRR = ABS(YR - ER)
      ERRI = ABS(YI - EI)
      WRITE(6,310) XR, XI, YR, YI, ER, EI, ERRR, ERRI
C
      WRITE(6,120)
C
C     ============================================================
C     TEST DENORM - DOUBLE PRECISION EUCLIDEAN NORM
C     ============================================================
      WRITE(6,400)
  400 FORMAT(' DENORM TESTS (DOUBLE PRECISION EUCLIDEAN NORM):')
C
C     DENORM([3,4]) = 5
      DX(1) = 3.0D0
      DX(2) = 4.0D0
      DRESLT = DENORM(2, DX)
      DEXPCT = 5.0D0
      DERR = DABS(DRESLT - DEXPCT)
      WRITE(6,410) DRESLT, DEXPCT, DERR
  410 FORMAT('   DENORM([3,4])=',D18.10/
     1       '            EXP=',D18.10,' ERR=',D10.2)
C
C     DENORM([1,2,2]) = 3
      DX(1) = 1.0D0
      DX(2) = 2.0D0
      DX(3) = 2.0D0
      DRESLT = DENORM(3, DX)
      DEXPCT = 3.0D0
      DERR = DABS(DRESLT - DEXPCT)
      WRITE(6,420) DRESLT, DEXPCT, DERR
  420 FORMAT('   DENORM([1,2,2])=',D18.10/
     1       '              EXP=',D18.10,' ERR=',D10.2)
C
C     DENORM([1,1,1,1,1]) = SQRT(5) = 2.2360679774997896
      DO 430 I = 1, 5
         DX(I) = 1.0D0
  430 CONTINUE
      DRESLT = DENORM(5, DX)
      DEXPCT = 2.2360679774997896D0
      DERR = DABS(DRESLT - DEXPCT)
      WRITE(6,440) DRESLT, DEXPCT, DERR
  440 FORMAT('   DENORM([1,1,1,1,1])=',D18.10/
     1       '                  EXP=',D18.10,' ERR=',D10.2)
C
C     LARGE VALUES - TEST OVERFLOW PROTECTION
      DX(1) = 1.0D30
      DX(2) = 1.0D30
      DRESLT = DENORM(2, DX)
      DEXPCT = 1.4142135623730951D30
      DERR = DABS(DRESLT - DEXPCT) / DEXPCT
      WRITE(6,450) DRESLT, DERR
  450 FORMAT('   DENORM([1E30,1E30])=',D18.10/
     1       '                REL.ERR=',D10.2)
C
      WRITE(6,120)
      WRITE(6,500)
  500 FORMAT(' SLATEC COMPLEX UTILITIES TEST COMPLETE')
      STOP
      END
C
C     ============================================================
C     CDIV - COMPLEX DIVISION
C     ============================================================
C     DECK CDIV
      SUBROUTINE CDIV (AR, AI, BR, BI, CR, CI)
C***BEGIN PROLOGUE  CDIV
C***SUBSIDIARY
C***PURPOSE  Compute the complex quotient of two complex numbers.
C***LIBRARY   SLATEC
C***TYPE      COMPLEX (CDIV-C)
C***AUTHOR  (UNKNOWN)
C***END PROLOGUE  CDIV
      REAL AR,AI,BR,BI,CR,CI
C
      REAL S,ARS,AIS,BRS,BIS
C***FIRST EXECUTABLE STATEMENT  CDIV
      S = ABS(BR) + ABS(BI)
      ARS = AR/S
      AIS = AI/S
      BRS = BR/S
      BIS = BI/S
      S = BRS**2 + BIS**2
      CR = (ARS*BRS + AIS*BIS)/S
      CI = (AIS*BRS - ARS*BIS)/S
      RETURN
      END
C
C     ============================================================
C     CSROOT - COMPLEX SQUARE ROOT
C     ============================================================
C     DECK CSROOT
      SUBROUTINE CSROOT (XR, XI, YR, YI)
C***BEGIN PROLOGUE  CSROOT
C***SUBSIDIARY
C***PURPOSE  Compute the complex square root of a complex number.
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (CSROOT-S)
C***AUTHOR  (UNKNOWN)
C***END PROLOGUE  CSROOT
      REAL XR,XI,YR,YI,S,TR,TI,PYTHAG
C
C     BRANCH CHOSEN SO THAT YR .GE. 0.0 AND SIGN(YI) .EQ. SIGN(XI)
C***FIRST EXECUTABLE STATEMENT  CSROOT
      TR = XR
      TI = XI
      S = SQRT(0.5E0*(PYTHAG(TR,TI) + ABS(TR)))
      IF (TR .GE. 0.0E0) YR = S
      IF (TI .LT. 0.0E0) S = -S
      IF (TR .LE. 0.0E0) YI = S
      IF (TR .LT. 0.0E0) YR = 0.5E0*(TI/YI)
      IF (TR .GT. 0.0E0) YI = 0.5E0*(TI/YR)
      RETURN
      END
C
C     ============================================================
C     PYTHAG - SQRT(A**2+B**2) WITHOUT OVERFLOW
C     ============================================================
C     DECK PYTHAG
      REAL FUNCTION PYTHAG (A, B)
C***BEGIN PROLOGUE  PYTHAG
C***SUBSIDIARY
C***PURPOSE  Compute sqrt(A**2+B**2) without overflow or underflow.
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (PYTHAG-S)
C***AUTHOR  (UNKNOWN)
C***END PROLOGUE  PYTHAG
      REAL A,B
C
      REAL P,Q,R,S,T
C***FIRST EXECUTABLE STATEMENT  PYTHAG
      P = AMAX1(ABS(A),ABS(B))
      Q = AMIN1(ABS(A),ABS(B))
      IF (Q .EQ. 0.0E0) GO TO 20
   10 CONTINUE
         R = (Q/P)**2
         T = 4.0E0 + R
         IF (T .EQ. 4.0E0) GO TO 20
         S = R/T
         P = P + 2.0E0*P*S
         Q = Q*S
      GO TO 10
   20 PYTHAG = P
      RETURN
      END
C
C     ============================================================
C     DENORM - DOUBLE PRECISION EUCLIDEAN NORM
C     ============================================================
C     DECK DENORM
      DOUBLE PRECISION FUNCTION DENORM (N, X)
C***BEGIN PROLOGUE  DENORM
C***SUBSIDIARY
C***PURPOSE  Subsidiary to DNSQ and DNSQE
C***LIBRARY   SLATEC
C***TYPE      DOUBLE PRECISION (ENORM-S, DENORM-D)
C***AUTHOR  (UNKNOWN)
C***END PROLOGUE  DENORM
      INTEGER I, N
      DOUBLE PRECISION AGIANT, FLOATN, ONE, RDWARF, RGIANT, S1, S2, S3,
     1     X(N), X1MAX, X3MAX, XABS, ZERO
      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/
C***FIRST EXECUTABLE STATEMENT  DENORM
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = DABS(X(I))
         IF (XABS .GT. RDWARF .AND. XABS .LT. AGIANT) GO TO 70
            IF (XABS .LE. RDWARF) GO TO 30
C
C              SUM FOR LARGE COMPONENTS.
C
               IF (XABS .LE. X1MAX) GO TO 10
                  S1 = ONE + S1*(X1MAX/XABS)**2
                  X1MAX = XABS
                  GO TO 20
   10          CONTINUE
                  S1 = S1 + (XABS/X1MAX)**2
   20          CONTINUE
               GO TO 60
   30       CONTINUE
C
C              SUM FOR SMALL COMPONENTS.
C
               IF (XABS .LE. X3MAX) GO TO 40
                  S3 = ONE + S3*(X3MAX/XABS)**2
                  X3MAX = XABS
                  GO TO 50
   40          CONTINUE
                  IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2
   50          CONTINUE
   60       CONTINUE
            GO TO 80
   70    CONTINUE
C
C           SUM FOR INTERMEDIATE COMPONENTS.
C
            S2 = S2 + XABS**2
   80    CONTINUE
   90    CONTINUE
C
C     CALCULATION OF NORM.
C
      IF (S1 .EQ. ZERO) GO TO 100
         DENORM = X1MAX*DSQRT(S1+(S2/X1MAX)/X1MAX)
         GO TO 130
  100 CONTINUE
         IF (S2 .EQ. ZERO) GO TO 110
            IF (S2 .GE. X3MAX)
     1         DENORM = DSQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
            IF (S2 .LT. X3MAX)
     1         DENORM = DSQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
            GO TO 120
  110    CONTINUE
            DENORM = X3MAX*DSQRT(S3)
  120    CONTINUE
  130 CONTINUE
      RETURN
      END
