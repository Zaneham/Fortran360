C     BLAS TEST: DROTG (GENERATE GIVENS ROTATION)
C     FORTRAN IV COMPATIBLE FOR IBM SYSTEM/360
C
C     TESTS GIVENS ROTATION GENERATION
C     GIVEN (A,B), PRODUCES (C,S,R) SUCH THAT:
C        [C  S] [A]   [R]
C        [-S C] [B] = [0]
C
C     REFERENCE: LAWSON ET AL. - BLAS (ACM TOMS 5(3), 1979)
C
      PROGRAM TROTG
      INTEGER PASSED, TOTAL
      DOUBLE PRECISION A, B, C, S, R, Z
      DOUBLE PRECISION EXPECT, ERROR, TOL
      DOUBLE PRECISION DABS, DSQRT
C
      TOL = 1.0D-12
      PASSED = 0
      TOTAL = 0
C
      WRITE(6,100)
  100 FORMAT('1',/,'BLAS TEST: DROTG (GENERATE GIVENS ROTATION)')
      WRITE(6,101)
  101 FORMAT(' ',60('-'))
C
C     ================================================================
C     TEST 1: 3-4-5 PYTHAGOREAN TRIPLE
C     ================================================================
C
      WRITE(6,200)
  200 FORMAT(/,' TEST 1: 3-4-5 PYTHAGOREAN TRIPLE')
      TOTAL = TOTAL + 1
C
      A = 3.0D0
      B = 4.0D0
      CALL DROTG(A, B, C, S)
      R = A
C
      WRITE(6,201) R, C, S
  201 FORMAT('   R = ',D20.13,/,'   C = ',D20.13,/,'   S = ',D20.13)
C
      ERROR = DABS(R - 5.0D0) + DABS(C - 0.6D0) + DABS(S - 0.8D0)
      IF (ERROR .GE. TOL) GO TO 11
         WRITE(6,202)
  202    FORMAT('   PASS: R=5, C=0.6, S=0.8')
         PASSED = PASSED + 1
         GO TO 19
   11 CONTINUE
         WRITE(6,203) ERROR
  203    FORMAT('   FAIL: TOTAL ERROR = ',D12.5)
   19 CONTINUE
C
C     ================================================================
C     TEST 2: 5-12-13 PYTHAGOREAN TRIPLE
C     ================================================================
C
      WRITE(6,300)
  300 FORMAT(/,' TEST 2: 5-12-13 PYTHAGOREAN TRIPLE')
      TOTAL = TOTAL + 1
C
      A = 5.0D0
      B = 12.0D0
      CALL DROTG(A, B, C, S)
      R = A
C
      WRITE(6,201) R, C, S
C
      ERROR = DABS(R - 13.0D0)
      ERROR = ERROR + DABS(C - 5.0D0/13.0D0)
      ERROR = ERROR + DABS(S - 12.0D0/13.0D0)
      IF (ERROR .GE. TOL) GO TO 21
         WRITE(6,301)
  301    FORMAT('   PASS: R=13, C=5/13, S=12/13')
         PASSED = PASSED + 1
         GO TO 29
   21 CONTINUE
         WRITE(6,203) ERROR
   29 CONTINUE
C
C     ================================================================
C     TEST 3: 8-15-17 PYTHAGOREAN TRIPLE
C     ================================================================
C
      WRITE(6,400)
  400 FORMAT(/,' TEST 3: 8-15-17 PYTHAGOREAN TRIPLE')
      TOTAL = TOTAL + 1
C
      A = 8.0D0
      B = 15.0D0
      CALL DROTG(A, B, C, S)
      R = A
C
      WRITE(6,201) R, C, S
C
      ERROR = DABS(R - 17.0D0)
      ERROR = ERROR + DABS(C - 8.0D0/17.0D0)
      ERROR = ERROR + DABS(S - 15.0D0/17.0D0)
      IF (ERROR .GE. TOL) GO TO 31
         WRITE(6,401)
  401    FORMAT('   PASS: R=17, C=8/17, S=15/17')
         PASSED = PASSED + 1
         GO TO 39
   31 CONTINUE
         WRITE(6,203) ERROR
   39 CONTINUE
C
C     ================================================================
C     TEST 4: B = 0 (NO ROTATION NEEDED)
C     ================================================================
C
      WRITE(6,500)
  500 FORMAT(/,' TEST 4: B = 0 (IDENTITY ROTATION)')
      TOTAL = TOTAL + 1
C
      A = 5.0D0
      B = 0.0D0
      CALL DROTG(A, B, C, S)
      R = A
C
      WRITE(6,201) R, C, S
C
      ERROR = DABS(C - 1.0D0) + DABS(S)
      IF (ERROR .GE. TOL) GO TO 41
         WRITE(6,501)
  501    FORMAT('   PASS: C=1, S=0 (IDENTITY)')
         PASSED = PASSED + 1
         GO TO 49
   41 CONTINUE
         WRITE(6,203) ERROR
   49 CONTINUE
C
C     ================================================================
C     TEST 5: A = 0
C     ================================================================
C
      WRITE(6,550)
  550 FORMAT(/,' TEST 5: A = 0')
      TOTAL = TOTAL + 1
C
      A = 0.0D0
      B = 5.0D0
      CALL DROTG(A, B, C, S)
      R = A
C
      WRITE(6,201) R, C, S
C
      ERROR = DABS(R - 5.0D0) + DABS(C) + DABS(S - 1.0D0)
      IF (ERROR .GE. TOL) GO TO 51
         WRITE(6,551)
  551    FORMAT('   PASS: R=5, C=0, S=1')
         PASSED = PASSED + 1
         GO TO 59
   51 CONTINUE
         WRITE(6,203) ERROR
   59 CONTINUE
C
C     ================================================================
C     TEST 6: ORTHOGONALITY C^2 + S^2 = 1
C     ================================================================
C
      WRITE(6,600)
  600 FORMAT(/,' TEST 6: ORTHOGONALITY C^2 + S^2 = 1')
      TOTAL = TOTAL + 1
C
      A = 7.0D0
      B = 24.0D0
      CALL DROTG(A, B, C, S)
C
      WRITE(6,601) C*C + S*S
  601 FORMAT('   C^2 + S^2 = ',D20.13)
C
      ERROR = DABS(C*C + S*S - 1.0D0)
      IF (ERROR .GE. TOL) GO TO 61
         WRITE(6,602)
  602    FORMAT('   PASS: ORTHOGONALITY PRESERVED')
         PASSED = PASSED + 1
         GO TO 69
   61 CONTINUE
         WRITE(6,203) ERROR
   69 CONTINUE
C
C     ================================================================
C     TEST 7: ROTATION ELIMINATES B
C     ================================================================
C
      WRITE(6,700)
  700 FORMAT(/,' TEST 7: ROTATION ELIMINATES B')
      TOTAL = TOTAL + 1
C
      A = 3.0D0
      B = 4.0D0
      CALL DROTG(A, B, C, S)
C
C     APPLY ROTATION TO ORIGINAL (3,4)
C     X' = C*3 + S*4, Y' = -S*3 + C*4
C
      EXPECT = C*3.0D0 + S*4.0D0
      ERROR = DABS(-S*3.0D0 + C*4.0D0)
C
      WRITE(6,701) EXPECT, ERROR
  701 FORMAT('   X'' = ',D20.13,/,'   Y'' = ',D20.13,' (SHOULD BE 0)')
C
      IF (ERROR .GE. TOL) GO TO 71
         WRITE(6,702)
  702    FORMAT('   PASS: ROTATION ZEROES SECOND COMPONENT')
         PASSED = PASSED + 1
         GO TO 79
   71 CONTINUE
         WRITE(6,203) ERROR
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
C     DROTG - CONSTRUCT GIVENS PLANE ROTATION
C     ================================================================
C
      SUBROUTINE DROTG(DA, DB, DC, DS)
C***BEGIN PROLOGUE  DROTG
C***PURPOSE  Construct Givens plane rotation
C***LIBRARY   SLATEC (BLAS)
C***END PROLOGUE  DROTG
      DOUBLE PRECISION DA, DB, DC, DS
      DOUBLE PRECISION R, ROE, SCALE, Z
      DOUBLE PRECISION DABS, DSQRT, DSIGN
C***FIRST EXECUTABLE STATEMENT  DROTG
      ROE = DB
      IF (DABS(DA) .GT. DABS(DB)) ROE = DA
      SCALE = DABS(DA) + DABS(DB)
      IF (SCALE .NE. 0.0D0) GO TO 10
         DC = 1.0D0
         DS = 0.0D0
         R = 0.0D0
         Z = 0.0D0
         GO TO 20
   10 CONTINUE
         R = SCALE*DSQRT((DA/SCALE)**2 + (DB/SCALE)**2)
         R = DSIGN(1.0D0, ROE)*R
         DC = DA/R
         DS = DB/R
         Z = 1.0D0
         IF (DABS(DA) .GT. DABS(DB)) Z = DS
         IF (DABS(DB) .GE. DABS(DA) .AND. DC .NE. 0.0D0) Z = 1.0D0/DC
   20 CONTINUE
      DA = R
      DB = Z
      RETURN
      END
