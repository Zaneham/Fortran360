C     DECK PYTHAG
      REAL FUNCTION PYTHAG (A, B)
C***BEGIN PROLOGUE  PYTHAG
C***SUBSIDIARY
C***PURPOSE  Compute the complex square root of a complex number without
C            destructive overflow or underflow.
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (PYTHAG-S)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     Finds sqrt(A**2+B**2) without overflow or destructive underflow
C
C***SEE ALSO  EISDOC
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   811101  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900402  Added TYPE section.  (WRB)
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
C     DECK ENORM
      REAL FUNCTION ENORM (N, X)
C***BEGIN PROLOGUE  ENORM
C***SUBSIDIARY
C***PURPOSE  Subsidiary to SNLS1, SNLS1E, SNSQ and SNSQE
C***LIBRARY   SLATEC
C***TYPE      SINGLE PRECISION (ENORM-S, DENORM-D)
C***AUTHOR  (UNKNOWN)
C***DESCRIPTION
C
C     Given an N-vector X, this function calculates the
C     Euclidean norm of X.
C
C     The Euclidean norm is computed by accumulating the sum of
C     squares in three different sums. The sums of squares for the
C     small and large components are scaled so that no overflows
C     occur. Non-destructive underflows are permitted. Underflows
C     and overflows do not occur in the computation of the unscaled
C     sum of squares for the intermediate components.
C     The definitions of small, intermediate and large components
C     depend on two constants, RDWARF and RGIANT. The main
C     restrictions on these constants are that RDWARF**2 not
C     underflow and RGIANT**2 not overflow. The constants
C     given here are suitable for every known computer.
C
C     The function statement is
C
C       REAL FUNCTION ENORM(N,X)
C
C     where
C
C       N is a positive integer input variable.
C
C       X is an input array of length N.
C
C***SEE ALSO  SNLS1, SNLS1E, SNSQ, SNSQE
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   800301  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   900328  Added TYPE section.  (WRB)
C***END PROLOGUE  ENORM
      INTEGER N
      REAL X(N)
      INTEGER I
      REAL AGIANT,FLOATN,ONE,RDWARF,RGIANT,S1,S2,S3,XABS,X1MAX,X3MAX,
     1     ZERO
      DATA ONE,ZERO,RDWARF,RGIANT /1.0E0,0.0E0,3.834E-20,1.304E19/
C***FIRST EXECUTABLE STATEMENT  ENORM
      S1 = ZERO
      S2 = ZERO
      S3 = ZERO
      X1MAX = ZERO
      X3MAX = ZERO
      FLOATN = N
      AGIANT = RGIANT/FLOATN
      DO 90 I = 1, N
         XABS = ABS(X(I))
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
         ENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)
         GO TO 130
  100 CONTINUE
         IF (S2 .EQ. ZERO) GO TO 110
            IF (S2 .GE. X3MAX)
     1         ENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))
            IF (S2 .LT. X3MAX)
     1         ENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))
            GO TO 120
  110    CONTINUE
            ENORM = X3MAX*SQRT(S3)
  120    CONTINUE
  130 CONTINUE
      RETURN
C
C     LAST CARD OF FUNCTION ENORM.
C
      END
C     TEST_NUMUTIL - TEST SLATEC NUMERICAL UTILITIES
C     PYTHAG AND ENORM ON IBM SYSTEM/360 FORTRAN G
C
      REAL PYTHAG, ENORM
      REAL A, B, RESULT, EXPECT, ERR
      REAL X(5)
      INTEGER I
C
      WRITE(6,100)
  100 FORMAT(' SLATEC NUMERICAL UTILITIES TEST')
      WRITE(6,110)
  110 FORMAT(' ================================')
      WRITE(6,120)
  120 FORMAT(' ')
C
C     TEST PYTHAG - SQRT(A**2 + B**2) WITHOUT OVERFLOW
      WRITE(6,200)
  200 FORMAT(' PYTHAG TESTS (SQRT(A**2+B**2)):')
C
C     PYTHAG(3,4) = 5
      A = 3.0
      B = 4.0
      RESULT = PYTHAG(A, B)
      EXPECT = 5.0
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,210) A, B, RESULT, EXPECT, ERR
  210 FORMAT('   PYTHAG(',F6.1,',',F6.1,')=',F10.4,
     1       ' EXPECT=',F10.4,' ERR=',E10.2)
C
C     PYTHAG(5,12) = 13
      A = 5.0
      B = 12.0
      RESULT = PYTHAG(A, B)
      EXPECT = 13.0
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,210) A, B, RESULT, EXPECT, ERR
C
C     PYTHAG(1,1) = SQRT(2)
      A = 1.0
      B = 1.0
      RESULT = PYTHAG(A, B)
      EXPECT = 1.4142136
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,210) A, B, RESULT, EXPECT, ERR
C
C     PYTHAG(0,5) = 5
      A = 0.0
      B = 5.0
      RESULT = PYTHAG(A, B)
      EXPECT = 5.0
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,210) A, B, RESULT, EXPECT, ERR
C
C     LARGE VALUES - WOULD OVERFLOW WITH NAIVE SQRT(A**2+B**2)
      A = 1.0E30
      B = 1.0E30
      RESULT = PYTHAG(A, B)
      EXPECT = 1.4142136E30
      ERR = ABS(RESULT - EXPECT) / EXPECT
      WRITE(6,220) A, B, RESULT, ERR
  220 FORMAT('   PYTHAG(',E10.2,',',E10.2,')=',E12.4,
     1       ' REL.ERR=',E10.2)
C
      WRITE(6,120)
C
C     TEST ENORM - EUCLIDEAN NORM
      WRITE(6,300)
  300 FORMAT(' ENORM TESTS (EUCLIDEAN NORM):')
C
C     ENORM([3,4]) = 5
      X(1) = 3.0
      X(2) = 4.0
      RESULT = ENORM(2, X)
      EXPECT = 5.0
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,310) RESULT, EXPECT, ERR
  310 FORMAT('   ENORM([3,4])=',F10.4,' EXPECT=',F10.4,
     1       ' ERR=',E10.2)
C
C     ENORM([1,2,2]) = 3
      X(1) = 1.0
      X(2) = 2.0
      X(3) = 2.0
      RESULT = ENORM(3, X)
      EXPECT = 3.0
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,320) RESULT, EXPECT, ERR
  320 FORMAT('   ENORM([1,2,2])=',F10.4,' EXPECT=',F10.4,
     1       ' ERR=',E10.2)
C
C     ENORM([1,1,1,1,1]) = SQRT(5)
      DO 400 I = 1, 5
         X(I) = 1.0
  400 CONTINUE
      RESULT = ENORM(5, X)
      EXPECT = 2.2360680
      ERR = ABS(RESULT - EXPECT)
      WRITE(6,330) RESULT, EXPECT, ERR
  330 FORMAT('   ENORM([1,1,1,1,1])=',F10.4,' EXPECT=',F10.4,
     1       ' ERR=',E10.2)
C
      WRITE(6,120)
      WRITE(6,500)
  500 FORMAT(' SLATEC NUMERICAL UTILITIES TEST COMPLETE')
      STOP
      END
