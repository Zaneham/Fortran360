C     MINPACK TEST: DNLS1 (NONLINEAR LEAST SQUARES)
C     FORTRAN IV COMPATIBLE FOR IBM SYSTEM/360
C
C     TESTS LEVENBERG-MARQUARDT ALGORITHM ON CLASSIC PROBLEMS:
C     1. ROSENBROCK FUNCTION (BANANA VALLEY)
C     2. POWELL SINGULAR FUNCTION
C     3. FREUDENSTEIN-ROTH FUNCTION
C
C     REFERENCE: MORE, GARBOW, HILLSTROM - TESTING UNCONSTRAINED
C                OPTIMIZATION SOFTWARE (ACM TOMS 7:1, 1981)
C
      PROGRAM TNLS1
      IMPLICIT NONE
C
C     PARAMETERS
      INTEGER MAXN, MAXM
      PARAMETER (MAXN = 4, MAXM = 4)
C
C     VARIABLES
      INTEGER M, N, INFO, NFEV, NJEV, IOPT, MODE, NPRINT, MAXFEV
      INTEGER IPVT(MAXN), I, PASSED, TOTAL, LDFJAC
      DOUBLE PRECISION X(MAXN), FVEC(MAXM), FJAC(MAXM,MAXN)
      DOUBLE PRECISION DIAG(MAXN), QTF(MAXN)
      DOUBLE PRECISION WA1(MAXN), WA2(MAXN), WA3(MAXN), WA4(MAXM)
      DOUBLE PRECISION FTOL, XTOL, GTOL, EPSFCN, FACTOR
      DOUBLE PRECISION FNORM, TOL, EXPECT
      DOUBLE PRECISION DENORM
      EXTERNAL DENORM, ROSBRK, POWELL, FREUD
C
      TOL = 1.0D-6
      PASSED = 0
      TOTAL = 0
C
      WRITE(6,100)
  100 FORMAT('1',/,'MINPACK TEST: DNLS1 (NONLINEAR LEAST SQUARES)')
      WRITE(6,101)
  101 FORMAT('-',60('-'))
C
C     ================================================================
C     TEST 1: ROSENBROCK FUNCTION
C     MIN F(X) = 100*(X2-X1**2)**2 + (1-X1)**2
C     SOLUTION: X* = (1, 1), F* = 0
C     STARTING POINT: (-1.2, 1.0)
C     ================================================================
C
      WRITE(6,200)
  200 FORMAT(/,'TEST 1: ROSENBROCK FUNCTION (BANANA VALLEY)')
      WRITE(6,201)
  201 FORMAT('  MIN: 100*(X2-X1^2)^2 + (1-X1)^2')
      WRITE(6,202)
  202 FORMAT('  START: (-1.2, 1.0), SOLUTION: (1, 1)')
      TOTAL = TOTAL + 1
C
C     PROBLEM DIMENSIONS
      M = 2
      N = 2
      LDFJAC = MAXM
C
C     INITIAL GUESS
      X(1) = -1.2D0
      X(2) = 1.0D0
C
C     TOLERANCES
      FTOL = 1.0D-10
      XTOL = 1.0D-10
      GTOL = 0.0D0
      MAXFEV = 400
      EPSFCN = 0.0D0
      MODE = 1
      FACTOR = 100.0D0
      NPRINT = 0
      IOPT = 1
C
      CALL DNLS1(ROSBRK, IOPT, M, N, X, FVEC, FJAC, LDFJAC,
     &           FTOL, XTOL, GTOL, MAXFEV, EPSFCN, DIAG, MODE,
     &           FACTOR, NPRINT, INFO, NFEV, NJEV, IPVT, QTF,
     &           WA1, WA2, WA3, WA4)
C
      FNORM = DENORM(M, FVEC)
C
      WRITE(6,210) X(1), X(2)
  210 FORMAT('  SOLUTION: X = (',D15.8,', ',D15.8,')')
      WRITE(6,211) FNORM
  211 FORMAT('  FINAL ||F|| = ',D15.8)
      WRITE(6,212) INFO, NFEV
  212 FORMAT('  INFO = ',I2,', NFEV = ',I4)
C
C     CHECK SOLUTION: X SHOULD BE (1, 1)
      IF (DABS(X(1)-1.0D0).LT.TOL .AND. DABS(X(2)-1.0D0).LT.TOL) THEN
         WRITE(6,213)
  213    FORMAT('  PASS: CONVERGED TO (1, 1)')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,214)
  214    FORMAT('  FAIL: DID NOT CONVERGE TO (1, 1)')
      ENDIF
C
C     ================================================================
C     TEST 2: POWELL SINGULAR FUNCTION
C     F1 = X1 + 10*X2
C     F2 = SQRT(5)*(X3 - X4)
C     F3 = (X2 - 2*X3)**2
C     F4 = SQRT(10)*(X1 - X4)**2
C     SOLUTION: X* = (0, 0, 0, 0), F* = 0
C     STARTING POINT: (3, -1, 0, 1)
C     ================================================================
C
      WRITE(6,300)
  300 FORMAT(/,'TEST 2: POWELL SINGULAR FUNCTION')
      WRITE(6,301)
  301 FORMAT('  4 EQUATIONS, 4 UNKNOWNS')
      WRITE(6,302)
  302 FORMAT('  START: (3,-1,0,1), SOLUTION: (0,0,0,0)')
      TOTAL = TOTAL + 1
C
C     PROBLEM DIMENSIONS
      M = 4
      N = 4
      LDFJAC = MAXM
C
C     INITIAL GUESS
      X(1) = 3.0D0
      X(2) = -1.0D0
      X(3) = 0.0D0
      X(4) = 1.0D0
C
C     TOLERANCES
      FTOL = 1.0D-10
      XTOL = 1.0D-10
      GTOL = 0.0D0
      MAXFEV = 1000
      EPSFCN = 0.0D0
      MODE = 1
      FACTOR = 100.0D0
      NPRINT = 0
      IOPT = 1
C
      CALL DNLS1(POWELL, IOPT, M, N, X, FVEC, FJAC, LDFJAC,
     &           FTOL, XTOL, GTOL, MAXFEV, EPSFCN, DIAG, MODE,
     &           FACTOR, NPRINT, INFO, NFEV, NJEV, IPVT, QTF,
     &           WA1, WA2, WA3, WA4)
C
      FNORM = DENORM(M, FVEC)
C
      WRITE(6,310) X(1), X(2), X(3), X(4)
  310 FORMAT('  SOLUTION: X = (',D12.5,',',D12.5,',',D12.5,',',D12.5,')')
      WRITE(6,211) FNORM
      WRITE(6,212) INFO, NFEV
C
C     CHECK: ||X|| SHOULD BE SMALL (NEAR ORIGIN)
      EXPECT = DSQRT(X(1)**2 + X(2)**2 + X(3)**2 + X(4)**2)
      IF (EXPECT .LT. 1.0D-4) THEN
         WRITE(6,311)
  311    FORMAT('  PASS: CONVERGED TO ORIGIN')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,312) EXPECT
  312    FORMAT('  FAIL: ||X|| = ',D12.5,' (EXPECTED ~ 0)')
      ENDIF
C
C     ================================================================
C     TEST 3: FREUDENSTEIN-ROTH FUNCTION
C     F1 = -13 + X1 + ((5-X2)*X2 - 2)*X2
C     F2 = -29 + X1 + ((X2+1)*X2 - 14)*X2
C     SOLUTION: X* = (5, 4) WITH F* = 0
C               OR X* = (11.41..., -0.8968...) LOCAL MIN
C     STARTING POINT: (0.5, -2)
C     ================================================================
C
      WRITE(6,400)
  400 FORMAT(/,'TEST 3: FREUDENSTEIN-ROTH FUNCTION')
      WRITE(6,401)
  401 FORMAT('  2 EQUATIONS, 2 UNKNOWNS')
      WRITE(6,402)
  402 FORMAT('  START: (0.5,-2), GLOBAL: (5,4), LOCAL: (11.4,-0.9)')
      TOTAL = TOTAL + 1
C
C     PROBLEM DIMENSIONS
      M = 2
      N = 2
      LDFJAC = MAXM
C
C     INITIAL GUESS - THIS ONE FINDS THE LOCAL MINIMUM
      X(1) = 0.5D0
      X(2) = -2.0D0
C
C     TOLERANCES
      FTOL = 1.0D-10
      XTOL = 1.0D-10
      GTOL = 0.0D0
      MAXFEV = 400
      EPSFCN = 0.0D0
      MODE = 1
      FACTOR = 100.0D0
      NPRINT = 0
      IOPT = 1
C
      CALL DNLS1(FREUD, IOPT, M, N, X, FVEC, FJAC, LDFJAC,
     &           FTOL, XTOL, GTOL, MAXFEV, EPSFCN, DIAG, MODE,
     &           FACTOR, NPRINT, INFO, NFEV, NJEV, IPVT, QTF,
     &           WA1, WA2, WA3, WA4)
C
      FNORM = DENORM(M, FVEC)
C
      WRITE(6,210) X(1), X(2)
      WRITE(6,211) FNORM
      WRITE(6,212) INFO, NFEV
C
C     ACCEPT EITHER SOLUTION
      IF (DABS(X(1)-5.0D0).LT.TOL .AND. DABS(X(2)-4.0D0).LT.TOL) THEN
         WRITE(6,410)
  410    FORMAT('  PASS: FOUND GLOBAL MINIMUM (5, 4)')
         PASSED = PASSED + 1
      ELSE IF (DABS(X(1)-11.41D0).LT.0.1D0) THEN
         WRITE(6,411)
  411    FORMAT('  PASS: FOUND LOCAL MINIMUM (~11.4, -0.9)')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,412)
  412    FORMAT('  FAIL: DID NOT CONVERGE TO KNOWN SOLUTION')
      ENDIF
C
C     ================================================================
C     SUMMARY
C     ================================================================
C
      WRITE(6,101)
      WRITE(6,900) PASSED, TOTAL
  900 FORMAT(/,'SUMMARY: ',I2,'/',I2,' TESTS PASSED')
C
      IF (PASSED .EQ. TOTAL) THEN
         WRITE(6,901)
  901    FORMAT('RESULT: ALL PASS')
      ELSE
         WRITE(6,902)
  902    FORMAT('RESULT: SOME FAILED')
      ENDIF
C
      STOP
      END
C
C     ================================================================
C     ROSENBROCK FUNCTION SUBROUTINE
C     ================================================================
C
      SUBROUTINE ROSBRK(IFLAG, M, N, X, FVEC, FJAC, LDFJAC)
      IMPLICIT NONE
      INTEGER IFLAG, M, N, LDFJAC
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N)
C
C     F1 = 10*(X2 - X1**2)
C     F2 = 1 - X1
C
      IF (IFLAG .EQ. 1) THEN
C        EVALUATE FUNCTION
         FVEC(1) = 10.0D0 * (X(2) - X(1)**2)
         FVEC(2) = 1.0D0 - X(1)
      ELSE IF (IFLAG .EQ. 2) THEN
C        EVALUATE JACOBIAN
         FJAC(1,1) = -20.0D0 * X(1)
         FJAC(1,2) = 10.0D0
         FJAC(2,1) = -1.0D0
         FJAC(2,2) = 0.0D0
      ENDIF
C
      RETURN
      END
C
C     ================================================================
C     POWELL SINGULAR FUNCTION SUBROUTINE
C     ================================================================
C
      SUBROUTINE POWELL(IFLAG, M, N, X, FVEC, FJAC, LDFJAC)
      IMPLICIT NONE
      INTEGER IFLAG, M, N, LDFJAC
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N)
      DOUBLE PRECISION SQRT5, SQRT10
C
      SQRT5 = DSQRT(5.0D0)
      SQRT10 = DSQRT(10.0D0)
C
      IF (IFLAG .EQ. 1) THEN
C        EVALUATE FUNCTION
         FVEC(1) = X(1) + 10.0D0*X(2)
         FVEC(2) = SQRT5 * (X(3) - X(4))
         FVEC(3) = (X(2) - 2.0D0*X(3))**2
         FVEC(4) = SQRT10 * (X(1) - X(4))**2
      ELSE IF (IFLAG .EQ. 2) THEN
C        EVALUATE JACOBIAN
         FJAC(1,1) = 1.0D0
         FJAC(1,2) = 10.0D0
         FJAC(1,3) = 0.0D0
         FJAC(1,4) = 0.0D0
C
         FJAC(2,1) = 0.0D0
         FJAC(2,2) = 0.0D0
         FJAC(2,3) = SQRT5
         FJAC(2,4) = -SQRT5
C
         FJAC(3,1) = 0.0D0
         FJAC(3,2) = 2.0D0*(X(2) - 2.0D0*X(3))
         FJAC(3,3) = -4.0D0*(X(2) - 2.0D0*X(3))
         FJAC(3,4) = 0.0D0
C
         FJAC(4,1) = 2.0D0*SQRT10*(X(1) - X(4))
         FJAC(4,2) = 0.0D0
         FJAC(4,3) = 0.0D0
         FJAC(4,4) = -2.0D0*SQRT10*(X(1) - X(4))
      ENDIF
C
      RETURN
      END
C
C     ================================================================
C     FREUDENSTEIN-ROTH FUNCTION SUBROUTINE
C     ================================================================
C
      SUBROUTINE FREUD(IFLAG, M, N, X, FVEC, FJAC, LDFJAC)
      IMPLICIT NONE
      INTEGER IFLAG, M, N, LDFJAC
      DOUBLE PRECISION X(N), FVEC(M), FJAC(LDFJAC,N)
C
      IF (IFLAG .EQ. 1) THEN
C        EVALUATE FUNCTION
         FVEC(1) = -13.0D0 + X(1) + ((5.0D0-X(2))*X(2) - 2.0D0)*X(2)
         FVEC(2) = -29.0D0 + X(1) + ((X(2)+1.0D0)*X(2) - 14.0D0)*X(2)
      ELSE IF (IFLAG .EQ. 2) THEN
C        EVALUATE JACOBIAN
C        DF1/DX1 = 1
C        DF1/DX2 = (5-X2)*2*X2 - X2^2 - 2 = 10*X2 - 3*X2^2 - 2
         FJAC(1,1) = 1.0D0
         FJAC(1,2) = 10.0D0*X(2) - 3.0D0*X(2)**2 - 2.0D0
C        DF2/DX1 = 1
C        DF2/DX2 = (X2+1)*2*X2 + X2^2 - 14 = 3*X2^2 + 2*X2 - 14
         FJAC(2,1) = 1.0D0
         FJAC(2,2) = 3.0D0*X(2)**2 + 2.0D0*X(2) - 14.0D0
      ENDIF
C
      RETURN
      END
