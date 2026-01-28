C     INTERPOLATION TEST: POLYNOMIAL (NEWTON DIVIDED DIFFERENCES)
C     FORTRAN IV COMPATIBLE FOR IBM SYSTEM/360
C
C     TESTS POLYNOMIAL INTERPOLATION USING NEWTON'S METHOD
C     THIS IS THE MATHEMATICAL CORE THAT DPLINT/DPOLVL IMPLEMENT
C
C     TEST CASE: y = x^3 THROUGH x = 0, 1, 2, 3, 4
C     EVALUATE AT: 0.5, 1.5, 2.5, 3.5
C
      PROGRAM TPOLY
      INTEGER I, N, PASSED, TOTAL
      DOUBLE PRECISION X(5), Y(5), C(5)
      DOUBLE PRECISION PEVAL, XE, ERROR, TOL
C
      TOL = 1.0D-12
      PASSED = 0
      TOTAL = 0
C
      WRITE(6,100)
  100 FORMAT('1',/,'INTERPOLATION TEST: POLYNOMIAL (IBM 360 GOLDEN)')
      WRITE(6,101)
  101 FORMAT(' ',60('-'))
C
C     ================================================================
C     SETUP: y = x^3 AT x = 0, 1, 2, 3, 4
C     ================================================================
C
      N = 5
      X(1) = 0.0D0
      X(2) = 1.0D0
      X(3) = 2.0D0
      X(4) = 3.0D0
      X(5) = 4.0D0
C
      Y(1) = 0.0D0
      Y(2) = 1.0D0
      Y(3) = 8.0D0
      Y(4) = 27.0D0
      Y(5) = 64.0D0
C
      WRITE(6,110)
  110 FORMAT(/,' DATA POINTS (y = x^3):')
      DO 15 I = 1, N
         WRITE(6,111) X(I), Y(I)
  111    FORMAT('   x = ',F4.1,'  y = ',F8.1)
   15 CONTINUE
C
C     COPY Y TO C
      DO 20 I = 1, N
         C(I) = Y(I)
   20 CONTINUE
C
C     DIVIDED DIFFERENCES - EXPLICIT (NO LOOPS WITH NEGATIVE STEP)
C     STAGE J=2
      C(5) = (C(5) - C(4)) / (X(5) - X(4))
      C(4) = (C(4) - C(3)) / (X(4) - X(3))
      C(3) = (C(3) - C(2)) / (X(3) - X(2))
      C(2) = (C(2) - C(1)) / (X(2) - X(1))
C     STAGE J=3
      C(5) = (C(5) - C(4)) / (X(5) - X(3))
      C(4) = (C(4) - C(3)) / (X(4) - X(2))
      C(3) = (C(3) - C(2)) / (X(3) - X(1))
C     STAGE J=4
      C(5) = (C(5) - C(4)) / (X(5) - X(2))
      C(4) = (C(4) - C(3)) / (X(4) - X(1))
C     STAGE J=5
      C(5) = (C(5) - C(4)) / (X(5) - X(1))
C
C     PRINT NEWTON COEFFICIENTS
      WRITE(6,120)
  120 FORMAT(/,' NEWTON COEFFICIENTS:')
      DO 25 I = 1, N
         WRITE(6,121) I, C(I)
  121    FORMAT('   C(',I1,') = ',D22.15)
   25 CONTINUE
C
C     ================================================================
C     TEST 1: p(0.5) - SHOULD BE 0.125
C     ================================================================
C
      TOTAL = TOTAL + 1
      XE = 0.5D0
      PEVAL = C(5)
      PEVAL = PEVAL * (XE - X(4)) + C(4)
      PEVAL = PEVAL * (XE - X(3)) + C(3)
      PEVAL = PEVAL * (XE - X(2)) + C(2)
      PEVAL = PEVAL * (XE - X(1)) + C(1)
C
      WRITE(6,200)
  200 FORMAT(/,' TEST 1: p(0.5)')
      WRITE(6,201) PEVAL
  201 FORMAT('   IBM 360: p(0.5) = ',D22.15)
      ERROR = DABS(PEVAL - 0.125D0)
      IF (ERROR .GE. TOL) GO TO 51
         WRITE(6,202)
  202    FORMAT('   PASS')
         PASSED = PASSED + 1
         GO TO 59
   51 CONTINUE
         WRITE(6,203) ERROR
  203    FORMAT('   FAIL: ERROR = ',D12.5)
   59 CONTINUE
C
C     ================================================================
C     TEST 2: p(1.5) - SHOULD BE 3.375
C     ================================================================
C
      TOTAL = TOTAL + 1
      XE = 1.5D0
      PEVAL = C(5)
      PEVAL = PEVAL * (XE - X(4)) + C(4)
      PEVAL = PEVAL * (XE - X(3)) + C(3)
      PEVAL = PEVAL * (XE - X(2)) + C(2)
      PEVAL = PEVAL * (XE - X(1)) + C(1)
C
      WRITE(6,300)
  300 FORMAT(/,' TEST 2: p(1.5)')
      WRITE(6,301) PEVAL
  301 FORMAT('   IBM 360: p(1.5) = ',D22.15)
      ERROR = DABS(PEVAL - 3.375D0)
      IF (ERROR .GE. TOL) GO TO 52
         WRITE(6,202)
         PASSED = PASSED + 1
         GO TO 69
   52 CONTINUE
         WRITE(6,203) ERROR
   69 CONTINUE
C
C     ================================================================
C     TEST 3: p(2.5) - SHOULD BE 15.625
C     ================================================================
C
      TOTAL = TOTAL + 1
      XE = 2.5D0
      PEVAL = C(5)
      PEVAL = PEVAL * (XE - X(4)) + C(4)
      PEVAL = PEVAL * (XE - X(3)) + C(3)
      PEVAL = PEVAL * (XE - X(2)) + C(2)
      PEVAL = PEVAL * (XE - X(1)) + C(1)
C
      WRITE(6,400)
  400 FORMAT(/,' TEST 3: p(2.5)')
      WRITE(6,401) PEVAL
  401 FORMAT('   IBM 360: p(2.5) = ',D22.15)
      ERROR = DABS(PEVAL - 15.625D0)
      IF (ERROR .GE. TOL) GO TO 53
         WRITE(6,202)
         PASSED = PASSED + 1
         GO TO 79
   53 CONTINUE
         WRITE(6,203) ERROR
   79 CONTINUE
C
C     ================================================================
C     TEST 4: p(3.5) - SHOULD BE 42.875
C     ================================================================
C
      TOTAL = TOTAL + 1
      XE = 3.5D0
      PEVAL = C(5)
      PEVAL = PEVAL * (XE - X(4)) + C(4)
      PEVAL = PEVAL * (XE - X(3)) + C(3)
      PEVAL = PEVAL * (XE - X(2)) + C(2)
      PEVAL = PEVAL * (XE - X(1)) + C(1)
C
      WRITE(6,500)
  500 FORMAT(/,' TEST 4: p(3.5)')
      WRITE(6,501) PEVAL
  501 FORMAT('   IBM 360: p(3.5) = ',D22.15)
      ERROR = DABS(PEVAL - 42.875D0)
      IF (ERROR .GE. TOL) GO TO 54
         WRITE(6,202)
         PASSED = PASSED + 1
         GO TO 89
   54 CONTINUE
         WRITE(6,203) ERROR
   89 CONTINUE
C
C     ================================================================
C     SUMMARY - GOLDEN VALUES FOR LEVEL 3 TESTS
C     ================================================================
C
      WRITE(6,600)
  600 FORMAT(/,' IBM 360 GOLDEN VALUES FOR LEVEL 3 TESTS:')
      WRITE(6,101)
      WRITE(6,601) 0.5D0
  601 FORMAT('   p(0.50) = see above')
      WRITE(6,602) 1.5D0
  602 FORMAT('   p(1.50) = see above')
      WRITE(6,603) 2.5D0
  603 FORMAT('   p(2.50) = see above')
      WRITE(6,604) 3.5D0
  604 FORMAT('   p(3.50) = see above')
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
