C     BLAS TEST: DAXPY (Y = ALPHA*X + Y)
C     FORTRAN IV COMPATIBLE FOR IBM SYSTEM/360
C
C     TESTS THE BASIC LINEAR ALGEBRA DAXPY OPERATION
C
C     REFERENCE: LAWSON ET AL. - BLAS (ACM TOMS 5(3), 1979)
C
      PROGRAM TAXPY
      INTEGER I, N, PASSED, TOTAL
      DOUBLE PRECISION X(5), Y(5), EXPECT(5), ALPHA, ERROR, TOL
      DOUBLE PRECISION DABS
C
      TOL = 1.0D-12
      PASSED = 0
      TOTAL = 0
C
      WRITE(6,100)
  100 FORMAT('1',/,'BLAS TEST: DAXPY (Y = ALPHA*X + Y)')
      WRITE(6,101)
  101 FORMAT(' ',60('-'))
C
C     ================================================================
C     TEST 1: ALPHA = 1 (SIMPLE ADDITION)
C     ================================================================
C
      WRITE(6,200)
  200 FORMAT(/,' TEST 1: ALPHA = 1.0 (Y = X + Y)')
      TOTAL = TOTAL + 1
C
      N = 5
      DO 10 I = 1, N
         X(I) = DFLOAT(I)
         Y(I) = DFLOAT(I * 10)
         EXPECT(I) = DFLOAT(I) + DFLOAT(I * 10)
   10 CONTINUE
C
      ALPHA = 1.0D0
      CALL DAXPY(N, ALPHA, X, 1, Y, 1)
C
      ERROR = 0.0D0
      DO 11 I = 1, N
         ERROR = ERROR + DABS(Y(I) - EXPECT(I))
   11 CONTINUE
C
      IF (ERROR .GE. TOL) GO TO 12
         WRITE(6,201)
  201    FORMAT('   PASS: Y = X + Y COMPUTED CORRECTLY')
         PASSED = PASSED + 1
         GO TO 19
   12 CONTINUE
         WRITE(6,202) ERROR
  202    FORMAT('   FAIL: TOTAL ERROR = ',D12.5)
   19 CONTINUE
C
C     ================================================================
C     TEST 2: ALPHA = 2 (SCALING)
C     ================================================================
C
      WRITE(6,300)
  300 FORMAT(/,' TEST 2: ALPHA = 2.0 (Y = 2*X + Y)')
      TOTAL = TOTAL + 1
C
      DO 20 I = 1, N
         X(I) = DFLOAT(I)
         Y(I) = 0.0D0
         EXPECT(I) = 2.0D0 * DFLOAT(I)
   20 CONTINUE
C
      ALPHA = 2.0D0
      CALL DAXPY(N, ALPHA, X, 1, Y, 1)
C
      ERROR = 0.0D0
      DO 21 I = 1, N
         ERROR = ERROR + DABS(Y(I) - EXPECT(I))
   21 CONTINUE
C
      IF (ERROR .GE. TOL) GO TO 22
         WRITE(6,301)
  301    FORMAT('   PASS: Y = 2*X COMPUTED CORRECTLY')
         PASSED = PASSED + 1
         GO TO 29
   22 CONTINUE
         WRITE(6,202) ERROR
   29 CONTINUE
C
C     ================================================================
C     TEST 3: ALPHA = 0 (NO CHANGE)
C     ================================================================
C
      WRITE(6,400)
  400 FORMAT(/,' TEST 3: ALPHA = 0.0 (Y UNCHANGED)')
      TOTAL = TOTAL + 1
C
      DO 30 I = 1, N
         X(I) = DFLOAT(I)
         Y(I) = DFLOAT(I * 10)
         EXPECT(I) = DFLOAT(I * 10)
   30 CONTINUE
C
      ALPHA = 0.0D0
      CALL DAXPY(N, ALPHA, X, 1, Y, 1)
C
      ERROR = 0.0D0
      DO 31 I = 1, N
         ERROR = ERROR + DABS(Y(I) - EXPECT(I))
   31 CONTINUE
C
      IF (ERROR .GE. TOL) GO TO 32
         WRITE(6,401)
  401    FORMAT('   PASS: Y UNCHANGED WHEN ALPHA = 0')
         PASSED = PASSED + 1
         GO TO 39
   32 CONTINUE
         WRITE(6,202) ERROR
   39 CONTINUE
C
C     ================================================================
C     TEST 4: ALPHA = -1 (SUBTRACTION)
C     ================================================================
C
      WRITE(6,500)
  500 FORMAT(/,' TEST 4: ALPHA = -1.0 (Y = Y - X)')
      TOTAL = TOTAL + 1
C
      DO 40 I = 1, N
         X(I) = DFLOAT(I)
         Y(I) = DFLOAT(I * 10)
         EXPECT(I) = DFLOAT(I * 10) - DFLOAT(I)
   40 CONTINUE
C
      ALPHA = -1.0D0
      CALL DAXPY(N, ALPHA, X, 1, Y, 1)
C
      ERROR = 0.0D0
      DO 41 I = 1, N
         ERROR = ERROR + DABS(Y(I) - EXPECT(I))
   41 CONTINUE
C
      IF (ERROR .GE. TOL) GO TO 42
         WRITE(6,501)
  501    FORMAT('   PASS: Y = Y - X COMPUTED CORRECTLY')
         PASSED = PASSED + 1
         GO TO 49
   42 CONTINUE
         WRITE(6,202) ERROR
   49 CONTINUE
C
C     ================================================================
C     TEST 5: OUTPUT VALUES FOR GOLDEN REFERENCE
C     ================================================================
C
      WRITE(6,600)
  600 FORMAT(/,' TEST 5: GOLDEN VALUES FOR LEVEL 3 TESTS')
      TOTAL = TOTAL + 1
C
      DO 50 I = 1, 5
         X(I) = DFLOAT(I)
         Y(I) = 100.0D0
   50 CONTINUE
C
      ALPHA = 3.14159265358979D0
      CALL DAXPY(5, ALPHA, X, 1, Y, 1)
C
      WRITE(6,601)
  601 FORMAT('   ALPHA = PI, X = (1,2,3,4,5), Y0 = 100')
      DO 51 I = 1, 5
         WRITE(6,602) I, Y(I)
  602    FORMAT('   Y(',I1,') = ',D20.13)
   51 CONTINUE
      PASSED = PASSED + 1
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
C     DAXPY - DOUBLE PRECISION A*X PLUS Y
C     ================================================================
C
      SUBROUTINE DAXPY(N, DA, DX, INCX, DY, INCY)
C***BEGIN PROLOGUE  DAXPY
C***PURPOSE  Compute Y = A*X + Y
C***LIBRARY   SLATEC (BLAS)
C***END PROLOGUE  DAXPY
      INTEGER I, INCX, INCY, IX, IY, N
      DOUBLE PRECISION DA, DX(*), DY(*)
C***FIRST EXECUTABLE STATEMENT  DAXPY
      IF (N .LE. 0) RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF (INCX .EQ. 1 .AND. INCY .EQ. 1) GO TO 20
C
C     CODE FOR UNEQUAL INCREMENTS
C
      IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
      DO 10 I = 1, N
         DY(IY) = DY(IY) + DA*DX(IX)
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
C
C     CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 CONTINUE
      DO 30 I = 1, N
         DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      RETURN
      END
