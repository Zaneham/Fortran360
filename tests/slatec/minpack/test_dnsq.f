C     MINPACK TEST: DNSQ (NONLINEAR EQUATIONS)
C     FORTRAN IV COMPATIBLE FOR IBM SYSTEM/360
C
C     TESTS POWELL'S HYBRID METHOD FOR SYSTEMS OF NONLINEAR EQUATIONS
C     FIND X SUCH THAT F(X) = 0
C
C     REFERENCE: POWELL, M.J.D. - A HYBRID METHOD FOR NONLINEAR
C                EQUATIONS (NUMERICAL METHODS FOR NONLINEAR
C                ALGEBRAIC EQUATIONS, 1970)
C
      PROGRAM TNSQ
      IMPLICIT NONE
C
C     PARAMETERS
      INTEGER MAXN
      PARAMETER (MAXN = 10)
C
C     VARIABLES
      INTEGER N, INFO, NFEV, IOPT, MODE, NPRINT, MAXFEV, LR
      INTEGER I, PASSED, TOTAL
      DOUBLE PRECISION X(MAXN), FVEC(MAXN), FJAC(MAXN,MAXN)
      DOUBLE PRECISION DIAG(MAXN), R(MAXN*(MAXN+1)/2), QTF(MAXN)
      DOUBLE PRECISION WA1(MAXN), WA2(MAXN), WA3(MAXN), WA4(MAXN)
      DOUBLE PRECISION XTOL, EPSFCN, FACTOR
      DOUBLE PRECISION FNORM, TOL, ERROR
      DOUBLE PRECISION DENORM
      EXTERNAL DENORM, HELICAL, TRIG, BROYDN
C
      TOL = 1.0D-6
      PASSED = 0
      TOTAL = 0
C
      WRITE(6,100)
  100 FORMAT('1',/,'MINPACK TEST: DNSQ (NONLINEAR EQUATIONS)')
      WRITE(6,101)
  101 FORMAT('-',60('-'))
C
C     ================================================================
C     TEST 1: HELICAL VALLEY FUNCTION (3 EQUATIONS)
C     F1 = 10*(X3 - 10*THETA(X1,X2))
C     F2 = 10*(SQRT(X1^2+X2^2) - 1)
C     F3 = X3
C     WHERE THETA = (1/2PI)*ARCTAN(X2/X1) IF X1 > 0
C     SOLUTION: X* = (1, 0, 0)
C     STARTING POINT: (-1, 0, 0)
C     ================================================================
C
      WRITE(6,200)
  200 FORMAT(/,'TEST 1: HELICAL VALLEY FUNCTION')
      WRITE(6,201)
  201 FORMAT('  3 EQUATIONS, 3 UNKNOWNS')
      WRITE(6,202)
  202 FORMAT('  START: (-1,0,0), SOLUTION: (1,0,0)')
      TOTAL = TOTAL + 1
C
C     PROBLEM DIMENSIONS
      N = 3
      LR = N*(N+1)/2
C
C     INITIAL GUESS
      X(1) = -1.0D0
      X(2) = 0.0D0
      X(3) = 0.0D0
C
C     TOLERANCES
      XTOL = 1.0D-10
      MAXFEV = 2000
      EPSFCN = 0.0D0
      MODE = 1
      FACTOR = 100.0D0
      NPRINT = 0
      IOPT = 1
C
      CALL DNSQ(HELICAL, HELICAL, IOPT, N, X, FVEC, FJAC, MAXN,
     &          XTOL, MAXFEV, MODE, DIAG, FACTOR, NPRINT, INFO,
     &          NFEV, R, LR, QTF, WA1, WA2, WA3, WA4)
C
      FNORM = DENORM(N, FVEC)
C
      WRITE(6,210) X(1), X(2), X(3)
  210 FORMAT('  SOLUTION: X = (',D12.5,', ',D12.5,', ',D12.5,')')
      WRITE(6,211) FNORM
  211 FORMAT('  FINAL ||F|| = ',D15.8)
      WRITE(6,212) INFO, NFEV
  212 FORMAT('  INFO = ',I2,', NFEV = ',I4)
C
C     CHECK SOLUTION: X SHOULD BE (1, 0, 0)
      ERROR = DABS(X(1)-1.0D0) + DABS(X(2)) + DABS(X(3))
      IF (ERROR .LT. TOL) THEN
         WRITE(6,213)
  213    FORMAT('  PASS: CONVERGED TO (1, 0, 0)')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,214) ERROR
  214    FORMAT('  FAIL: ERROR = ',D12.5)
      ENDIF
C
C     ================================================================
C     TEST 2: TRIGONOMETRIC FUNCTION (N EQUATIONS)
C     F_I = N - SUM_J(COS(X_J)) + I*(1-COS(X_I)) - SIN(X_I)
C     SOLUTION: X* = (0, 0, ..., 0) FOR SPECIFIC N
C     ================================================================
C
      WRITE(6,300)
  300 FORMAT(/,'TEST 2: TRIGONOMETRIC FUNCTION (N=5)')
      WRITE(6,301)
  301 FORMAT('  5 EQUATIONS, 5 UNKNOWNS')
      WRITE(6,302)
  302 FORMAT('  START: (1/N, ..., 1/N)')
      TOTAL = TOTAL + 1
C
C     PROBLEM DIMENSIONS
      N = 5
      LR = N*(N+1)/2
C
C     INITIAL GUESS: X_I = 1/N
      DO 30 I = 1, N
         X(I) = 1.0D0 / DFLOAT(N)
   30 CONTINUE
C
C     TOLERANCES
      XTOL = 1.0D-10
      MAXFEV = 2000
      EPSFCN = 0.0D0
      MODE = 1
      FACTOR = 100.0D0
      NPRINT = 0
      IOPT = 1
C
      CALL DNSQ(TRIG, TRIG, IOPT, N, X, FVEC, FJAC, MAXN,
     &          XTOL, MAXFEV, MODE, DIAG, FACTOR, NPRINT, INFO,
     &          NFEV, R, LR, QTF, WA1, WA2, WA3, WA4)
C
      FNORM = DENORM(N, FVEC)
C
      WRITE(6,310) (X(I), I=1,N)
  310 FORMAT('  SOLUTION: X = (',5D11.4,')')
      WRITE(6,211) FNORM
      WRITE(6,212) INFO, NFEV
C
C     CHECK: ||F|| SHOULD BE SMALL
      IF (FNORM .LT. 1.0D-8) THEN
         WRITE(6,311)
  311    FORMAT('  PASS: ||F|| < 1E-8')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,312)
  312    FORMAT('  FAIL: ||F|| NOT SMALL ENOUGH')
      ENDIF
C
C     ================================================================
C     TEST 3: BROYDEN TRIDIAGONAL (SPARSE SYSTEM)
C     F_I = (3-2*X_I)*X_I - X_{I-1} - 2*X_{I+1} + 1
C     WHERE X_0 = X_{N+1} = 0
C     ================================================================
C
      WRITE(6,400)
  400 FORMAT(/,'TEST 3: BROYDEN TRIDIAGONAL (N=10)')
      WRITE(6,401)
  401 FORMAT('  10 EQUATIONS, 10 UNKNOWNS (SPARSE)')
      WRITE(6,402)
  402 FORMAT('  START: (-1, -1, ..., -1)')
      TOTAL = TOTAL + 1
C
C     PROBLEM DIMENSIONS
      N = 10
      LR = N*(N+1)/2
C
C     INITIAL GUESS
      DO 40 I = 1, N
         X(I) = -1.0D0
   40 CONTINUE
C
C     TOLERANCES
      XTOL = 1.0D-10
      MAXFEV = 2000
      EPSFCN = 0.0D0
      MODE = 1
      FACTOR = 100.0D0
      NPRINT = 0
      IOPT = 1
C
      CALL DNSQ(BROYDN, BROYDN, IOPT, N, X, FVEC, FJAC, MAXN,
     &          XTOL, MAXFEV, MODE, DIAG, FACTOR, NPRINT, INFO,
     &          NFEV, R, LR, QTF, WA1, WA2, WA3, WA4)
C
      FNORM = DENORM(N, FVEC)
C
      WRITE(6,410) X(1), X(2), X(N-1), X(N)
  410 FORMAT('  SOLUTION: X(1:2) = ',D11.4,', ',D11.4,
     &       '  X(N-1:N) = ',D11.4,', ',D11.4)
      WRITE(6,211) FNORM
      WRITE(6,212) INFO, NFEV
C
C     CHECK: ||F|| SHOULD BE SMALL
      IF (FNORM .LT. 1.0D-8) THEN
         WRITE(6,411)
  411    FORMAT('  PASS: ||F|| < 1E-8')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,412)
  412    FORMAT('  FAIL: ||F|| NOT SMALL ENOUGH')
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
C     HELICAL VALLEY FUNCTION
C     ================================================================
C
      SUBROUTINE HELICAL(N, X, FVEC, FJAC, LDFJAC, IFLAG)
      IMPLICIT NONE
      INTEGER N, LDFJAC, IFLAG
      DOUBLE PRECISION X(N), FVEC(N), FJAC(LDFJAC,N)
      DOUBLE PRECISION PI, TWOPI, THETA, R, TEMP
      DOUBLE PRECISION DATAN2
C
      PI = 3.141592653589793D0
      TWOPI = 2.0D0 * PI
C
      IF (X(1) .GT. 0.0D0) THEN
         THETA = DATAN2(X(2), X(1)) / TWOPI
      ELSE IF (X(1) .LT. 0.0D0) THEN
         THETA = DATAN2(X(2), X(1)) / TWOPI + 0.5D0
      ELSE
         THETA = 0.25D0
         IF (X(2) .LT. 0.0D0) THETA = -0.25D0
      ENDIF
C
      R = DSQRT(X(1)**2 + X(2)**2)
C
      IF (IFLAG .EQ. 1) THEN
C        EVALUATE FUNCTION
         FVEC(1) = 10.0D0 * (X(3) - 10.0D0*THETA)
         FVEC(2) = 10.0D0 * (R - 1.0D0)
         FVEC(3) = X(3)
      ELSE IF (IFLAG .EQ. 2) THEN
C        EVALUATE JACOBIAN
         TEMP = 50.0D0 / (PI * R**2)
         FJAC(1,1) = TEMP * X(2)
         FJAC(1,2) = -TEMP * X(1)
         FJAC(1,3) = 10.0D0
C
         FJAC(2,1) = 10.0D0 * X(1) / R
         FJAC(2,2) = 10.0D0 * X(2) / R
         FJAC(2,3) = 0.0D0
C
         FJAC(3,1) = 0.0D0
         FJAC(3,2) = 0.0D0
         FJAC(3,3) = 1.0D0
      ENDIF
C
      RETURN
      END
C
C     ================================================================
C     TRIGONOMETRIC FUNCTION
C     ================================================================
C
      SUBROUTINE TRIG(N, X, FVEC, FJAC, LDFJAC, IFLAG)
      IMPLICIT NONE
      INTEGER N, LDFJAC, IFLAG, I, J
      DOUBLE PRECISION X(N), FVEC(N), FJAC(LDFJAC,N)
      DOUBLE PRECISION SUM, TEMP
C
      IF (IFLAG .EQ. 1) THEN
C        EVALUATE FUNCTION
         SUM = 0.0D0
         DO 10 J = 1, N
            SUM = SUM + DCOS(X(J))
   10    CONTINUE
C
         DO 20 I = 1, N
            FVEC(I) = DFLOAT(N) - SUM + DFLOAT(I)*(1.0D0-DCOS(X(I)))
     &                - DSIN(X(I))
   20    CONTINUE
      ELSE IF (IFLAG .EQ. 2) THEN
C        EVALUATE JACOBIAN
         DO 40 J = 1, N
            TEMP = DSIN(X(J))
            DO 30 I = 1, N
               FJAC(I,J) = TEMP
   30       CONTINUE
            FJAC(J,J) = DFLOAT(J+1)*TEMP - DCOS(X(J))
   40    CONTINUE
      ENDIF
C
      RETURN
      END
C
C     ================================================================
C     BROYDEN TRIDIAGONAL FUNCTION
C     ================================================================
C
      SUBROUTINE BROYDN(N, X, FVEC, FJAC, LDFJAC, IFLAG)
      IMPLICIT NONE
      INTEGER N, LDFJAC, IFLAG, I, J
      DOUBLE PRECISION X(N), FVEC(N), FJAC(LDFJAC,N)
      DOUBLE PRECISION XIM1, XIP1
C
      IF (IFLAG .EQ. 1) THEN
C        EVALUATE FUNCTION
         DO 10 I = 1, N
            IF (I .EQ. 1) THEN
               XIM1 = 0.0D0
            ELSE
               XIM1 = X(I-1)
            ENDIF
            IF (I .EQ. N) THEN
               XIP1 = 0.0D0
            ELSE
               XIP1 = X(I+1)
            ENDIF
            FVEC(I) = (3.0D0 - 2.0D0*X(I))*X(I) - XIM1 - 2.0D0*XIP1
     &                + 1.0D0
   10    CONTINUE
      ELSE IF (IFLAG .EQ. 2) THEN
C        EVALUATE JACOBIAN (TRIDIAGONAL)
         DO 30 J = 1, N
            DO 20 I = 1, N
               FJAC(I,J) = 0.0D0
   20       CONTINUE
   30    CONTINUE
C
         DO 40 I = 1, N
            FJAC(I,I) = 3.0D0 - 4.0D0*X(I)
            IF (I .GT. 1) FJAC(I,I-1) = -1.0D0
            IF (I .LT. N) FJAC(I,I+1) = -2.0D0
   40    CONTINUE
      ENDIF
C
      RETURN
      END
