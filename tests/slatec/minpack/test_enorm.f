C     MINPACK TEST: ENORM/DENORM (EUCLIDEAN NORM)
C     FORTRAN IV COMPATIBLE FOR IBM SYSTEM/360
C
C     TESTS THE EUCLIDEAN NORM COMPUTATION AGAINST KNOWN VALUES
C     ENORM USES SCALED ACCUMULATION TO AVOID OVERFLOW/UNDERFLOW
C
C     REFERENCE: MORE, GARBOW, HILLSTROM - MINPACK (ANL-80-74)
C
      PROGRAM TNORM
      INTEGER I, N, PASSED, TOTAL
      DOUBLE PRECISION X(10), RESULT, EXPECT, ERROR, TOL
      DOUBLE PRECISION DENORM
C
      TOL = 1.0D-12
      PASSED = 0
      TOTAL = 0
C
      WRITE(6,100)
  100 FORMAT('1',/,'MINPACK TEST: ENORM/DENORM (EUCLIDEAN NORM)')
      WRITE(6,101)
  101 FORMAT(' ',60('-'))
C
C     ================================================================
C     TEST 1: UNIT VECTOR (1,0,0,...,0)
C     NORM SHOULD BE 1.0
C     ================================================================
C
      WRITE(6,200)
  200 FORMAT(/,' TEST 1: UNIT VECTOR E1')
      TOTAL = TOTAL + 1
C
      N = 10
      DO 10 I = 1, N
         X(I) = 0.0D0
   10 CONTINUE
      X(1) = 1.0D0
C
      RESULT = DENORM(N, X)
      EXPECT = 1.0D0
      ERROR = DABS(RESULT - EXPECT)
C
      IF (ERROR .GE. TOL) GO TO 11
         WRITE(6,201) RESULT
  201    FORMAT('   PASS: NORM = ',D20.13)
         PASSED = PASSED + 1
         GO TO 19
   11 CONTINUE
         WRITE(6,202) RESULT, EXPECT, ERROR
  202    FORMAT('   FAIL: GOT ',D20.13,' EXPECTED ',D20.13,
     &          ' ERROR ',D12.5)
   19 CONTINUE
C
C     ================================================================
C     TEST 2: VECTOR OF ONES (1,1,1,...,1)
C     NORM SHOULD BE SQRT(N)
C     ================================================================
C
      WRITE(6,300)
  300 FORMAT(/,' TEST 2: VECTOR OF ONES')
      TOTAL = TOTAL + 1
C
      N = 10
      DO 20 I = 1, N
         X(I) = 1.0D0
   20 CONTINUE
C
      RESULT = DENORM(N, X)
      EXPECT = DSQRT(DFLOAT(N))
      ERROR = DABS(RESULT - EXPECT)
C
      IF (ERROR .GE. TOL) GO TO 21
         WRITE(6,301) RESULT, EXPECT
  301    FORMAT('   PASS: NORM = ',D20.13,' EXPECTED ',D20.13)
         PASSED = PASSED + 1
         GO TO 29
   21 CONTINUE
         WRITE(6,202) RESULT, EXPECT, ERROR
   29 CONTINUE
C
C     ================================================================
C     TEST 3: 3-4-5 RIGHT TRIANGLE
C     (3,4,0,...,0) SHOULD GIVE NORM = 5
C     ================================================================
C
      WRITE(6,400)
  400 FORMAT(/,' TEST 3: 3-4-5 RIGHT TRIANGLE')
      TOTAL = TOTAL + 1
C
      N = 10
      DO 30 I = 1, N
         X(I) = 0.0D0
   30 CONTINUE
      X(1) = 3.0D0
      X(2) = 4.0D0
C
      RESULT = DENORM(N, X)
      EXPECT = 5.0D0
      ERROR = DABS(RESULT - EXPECT)
C
      IF (ERROR .GE. TOL) GO TO 31
         WRITE(6,401) RESULT
  401    FORMAT('   PASS: NORM(3,4) = ',D20.13,' = 5')
         PASSED = PASSED + 1
         GO TO 39
   31 CONTINUE
         WRITE(6,202) RESULT, EXPECT, ERROR
   39 CONTINUE
C
C     ================================================================
C     TEST 4: SCALED VECTOR (TESTS OVERFLOW PROTECTION)
C     LARGE COMPONENTS: (1E15, 1E15, 1E15)
C     ================================================================
C
      WRITE(6,500)
  500 FORMAT(/,' TEST 4: LARGE COMPONENTS (OVERFLOW TEST)')
      TOTAL = TOTAL + 1
C
      N = 3
      X(1) = 1.0D15
      X(2) = 1.0D15
      X(3) = 1.0D15
C
      RESULT = DENORM(N, X)
      EXPECT = DSQRT(3.0D0) * 1.0D15
      ERROR = DABS(RESULT - EXPECT) / EXPECT
C
      IF (ERROR .GE. 1.0D-10) GO TO 41
         WRITE(6,501) RESULT
  501    FORMAT('   PASS: NORM = ',D20.13)
         PASSED = PASSED + 1
         GO TO 49
   41 CONTINUE
         WRITE(6,502) RESULT, EXPECT, ERROR
  502    FORMAT('   FAIL: GOT ',D20.13,' EXPECTED ',D20.13,
     &          ' REL.ERR ',D12.5)
   49 CONTINUE
C
C     ================================================================
C     TEST 5: SMALL COMPONENTS (TESTS UNDERFLOW PROTECTION)
C     SMALL COMPONENTS: (1E-15, 1E-15, 1E-15)
C     ================================================================
C
      WRITE(6,600)
  600 FORMAT(/,' TEST 5: SMALL COMPONENTS (UNDERFLOW TEST)')
      TOTAL = TOTAL + 1
C
      N = 3
      X(1) = 1.0D-15
      X(2) = 1.0D-15
      X(3) = 1.0D-15
C
      RESULT = DENORM(N, X)
      EXPECT = DSQRT(3.0D0) * 1.0D-15
      ERROR = DABS(RESULT - EXPECT) / EXPECT
C
      IF (ERROR .GE. 1.0D-10) GO TO 51
         WRITE(6,601) RESULT
  601    FORMAT('   PASS: NORM = ',D20.13)
         PASSED = PASSED + 1
         GO TO 59
   51 CONTINUE
         WRITE(6,502) RESULT, EXPECT, ERROR
   59 CONTINUE
C
C     ================================================================
C     TEST 6: MIXED MAGNITUDE (STRESSES ALL THREE ACCUMULATORS)
C     (1E-18, 1.0, 1E18)
C     ================================================================
C
      WRITE(6,700)
  700 FORMAT(/,' TEST 6: MIXED MAGNITUDE (STRESS TEST)')
      TOTAL = TOTAL + 1
C
      N = 3
      X(1) = 1.0D-18
      X(2) = 1.0D0
      X(3) = 1.0D18
C
      RESULT = DENORM(N, X)
C     DOMINATED BY 1E18, SO EXPECT APPROX 1E18
      EXPECT = 1.0D18
      ERROR = DABS(RESULT - EXPECT) / EXPECT
C
      IF (ERROR .GE. 1.0D-10) GO TO 61
         WRITE(6,701) RESULT
  701    FORMAT('   PASS: NORM = ',D20.13,' (DOMINATED BY 1E18)')
         PASSED = PASSED + 1
         GO TO 69
   61 CONTINUE
         WRITE(6,502) RESULT, EXPECT, ERROR
   69 CONTINUE
C
C     ================================================================
C     TEST 7: ZERO VECTOR
C     ================================================================
C
      WRITE(6,800)
  800 FORMAT(/,' TEST 7: ZERO VECTOR')
      TOTAL = TOTAL + 1
C
      N = 5
      DO 70 I = 1, N
         X(I) = 0.0D0
   70 CONTINUE
C
      RESULT = DENORM(N, X)
C
      IF (RESULT .NE. 0.0D0) GO TO 71
         WRITE(6,801)
  801    FORMAT('   PASS: NORM(0) = 0')
         PASSED = PASSED + 1
         GO TO 79
   71 CONTINUE
         WRITE(6,802) RESULT
  802    FORMAT('   FAIL: NORM(0) = ',D20.13,' (EXPECTED 0)')
   79 CONTINUE
C
C     ================================================================
C     SUMMARY
C     ================================================================
C
      WRITE(6,101)
      WRITE(6,900) PASSED, TOTAL
  900 FORMAT(/,' SUMMARY: ',I2,'/',I2,' TESTS PASSED')
C
      IF (PASSED .NE. TOTAL) GO TO 91
         WRITE(6,901)
  901    FORMAT(' RESULT: ALL PASS')
         GO TO 99
   91 CONTINUE
         WRITE(6,902)
  902    FORMAT(' RESULT: SOME FAILED')
   99 CONTINUE
C
      STOP
      END
C
C     ================================================================
C     DENORM - DOUBLE PRECISION EUCLIDEAN NORM
C     ================================================================
C
      DOUBLE PRECISION FUNCTION DENORM (N, X)
C***BEGIN PROLOGUE  DENORM
C***PURPOSE  Compute Euclidean norm with overflow/underflow protection
C***LIBRARY   SLATEC (MINPACK)
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
