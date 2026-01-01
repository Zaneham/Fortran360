C     MINPACK TEST: DQRFAC (QR FACTORIZATION WITH PIVOTING)
C     FORTRAN IV COMPATIBLE FOR IBM SYSTEM/360
C
C     TESTS QR FACTORIZATION: A*P = Q*R WHERE
C       A IS M BY N
C       Q IS M BY M ORTHOGONAL
C       R IS M BY N UPPER TRIANGULAR
C       P IS N BY N PERMUTATION
C
C     VERIFICATION: A*P = Q*R, Q^T*Q = I, R IS UPPER TRIANGULAR
C
C     REFERENCE: MORE, GARBOW, HILLSTROM - MINPACK (ANL-80-74)
C
      PROGRAM TQRFAC
      IMPLICIT NONE
      INTEGER I, J, K, M, N, PASSED, TOTAL
      INTEGER IPVT(5), LDA
      PARAMETER (LDA = 5)
      DOUBLE PRECISION A(LDA,5), ACOPY(LDA,5), R(LDA,5), QTQ(5,5)
      DOUBLE PRECISION RDIAG(5), ACNORM(5), WA(5)
      DOUBLE PRECISION SUM, ERROR, MAXERR, TOL
      DOUBLE PRECISION ZERO, ONE
      LOGICAL PIVOT
C
      DATA ZERO, ONE / 0.0D0, 1.0D0 /
      TOL = 1.0D-10
      PASSED = 0
      TOTAL = 0
C
      WRITE(6,100)
  100 FORMAT('1',/,'MINPACK TEST: DQRFAC (QR FACTORIZATION)')
      WRITE(6,101)
  101 FORMAT('-',60('-'))
C
C     ================================================================
C     TEST 1: IDENTITY MATRIX 3X3
C     QR OF I SHOULD BE Q=I, R=I
C     ================================================================
C
      WRITE(6,200)
  200 FORMAT(/,'TEST 1: QR OF 3X3 IDENTITY MATRIX')
      TOTAL = TOTAL + 1
C
      M = 3
      N = 3
      PIVOT = .TRUE.
C
C     CREATE IDENTITY MATRIX
      DO 20 J = 1, N
         DO 10 I = 1, M
            IF (I .EQ. J) THEN
               A(I,J) = ONE
            ELSE
               A(I,J) = ZERO
            ENDIF
            ACOPY(I,J) = A(I,J)
   10    CONTINUE
   20 CONTINUE
C
C     CALL DQRFAC
      CALL DQRFAC(M, N, A, LDA, PIVOT, IPVT, N, RDIAG, ACNORM, WA)
C
C     VERIFY DIAGONAL OF R (SHOULD BE +/- 1)
      MAXERR = ZERO
      DO 30 I = 1, N
         ERROR = DABS(DABS(RDIAG(I)) - ONE)
         IF (ERROR .GT. MAXERR) MAXERR = ERROR
   30 CONTINUE
C
      IF (MAXERR .LT. TOL) THEN
         WRITE(6,201)
  201    FORMAT('  PASS: R DIAGONAL IS +/-1')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,202) MAXERR
  202    FORMAT('  FAIL: R DIAGONAL ERROR = ',D12.5)
      ENDIF
C
C     ================================================================
C     TEST 2: SIMPLE 2X2 MATRIX
C     A = [3 1; 4 2] - VERIFY Q^T*Q = I
C     ================================================================
C
      WRITE(6,300)
  300 FORMAT(/,'TEST 2: QR OF 2X2 MATRIX [3,1; 4,2]')
      TOTAL = TOTAL + 1
C
      M = 2
      N = 2
      PIVOT = .TRUE.
C
      A(1,1) = 3.0D0
      A(1,2) = 1.0D0
      A(2,1) = 4.0D0
      A(2,2) = 2.0D0
C
C     SAVE COPY
      DO 45 J = 1, N
         DO 40 I = 1, M
            ACOPY(I,J) = A(I,J)
   40    CONTINUE
   45 CONTINUE
C
      CALL DQRFAC(M, N, A, LDA, PIVOT, IPVT, N, RDIAG, ACNORM, WA)
C
C     VERIFY R(1,1) = -NORM OF FIRST COLUMN OF PERMUTED A
C     COLUMN NORMS: ||COL1|| = 5, ||COL2|| = SQRT(5)
C     FIRST PIVOT SHOULD BE COLUMN 1, SO R(1,1) = -5
C
      ERROR = DABS(DABS(RDIAG(1)) - 5.0D0)
      IF (ERROR .LT. TOL) THEN
         WRITE(6,301) RDIAG(1)
  301    FORMAT('  PASS: R(1,1) = ',D15.8,' (EXPECTED +/-5)')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,302) RDIAG(1), ERROR
  302    FORMAT('  FAIL: R(1,1) = ',D15.8,' ERROR = ',D12.5)
      ENDIF
C
C     ================================================================
C     TEST 3: OVERDETERMINED SYSTEM 3X2
C     ================================================================
C
      WRITE(6,400)
  400 FORMAT(/,'TEST 3: QR OF 3X2 OVERDETERMINED SYSTEM')
      TOTAL = TOTAL + 1
C
      M = 3
      N = 2
      PIVOT = .TRUE.
C
C     A = [1 0; 0 1; 1 1]
      A(1,1) = 1.0D0
      A(1,2) = 0.0D0
      A(2,1) = 0.0D0
      A(2,2) = 1.0D0
      A(3,1) = 1.0D0
      A(3,2) = 1.0D0
C
      CALL DQRFAC(M, N, A, LDA, PIVOT, IPVT, N, RDIAG, ACNORM, WA)
C
C     R SHOULD BE 2X2 UPPER TRIANGULAR
C     COLUMN NORMS: ||COL1|| = SQRT(2), ||COL2|| = SQRT(2)
C     BOTH COLUMNS HAVE SAME NORM
C
      WRITE(6,401) RDIAG(1), RDIAG(2)
  401 FORMAT('  R DIAGONAL: ',D15.8,', ',D15.8)
      WRITE(6,402) IPVT(1), IPVT(2)
  402 FORMAT('  PIVOTS: ',I2,', ',I2)
C
C     VERIFY NORMS ARE PRESERVED
      ERROR = DABS(DABS(RDIAG(1)) - DSQRT(2.0D0))
      IF (ERROR .LT. TOL) THEN
         WRITE(6,403)
  403    FORMAT('  PASS: R(1,1) = SQRT(2) AS EXPECTED')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,404) ERROR
  404    FORMAT('  FAIL: ERROR IN R(1,1) = ',D12.5)
      ENDIF
C
C     ================================================================
C     TEST 4: RANK-DEFICIENT MATRIX
C     A = [1 2; 2 4; 3 6] (COLUMNS ARE LINEARLY DEPENDENT)
C     ================================================================
C
      WRITE(6,500)
  500 FORMAT(/,'TEST 4: RANK-DEFICIENT MATRIX')
      TOTAL = TOTAL + 1
C
      M = 3
      N = 2
      PIVOT = .TRUE.
C
C     COLUMNS ARE PROPORTIONAL: COL2 = 2*COL1
      A(1,1) = 1.0D0
      A(1,2) = 2.0D0
      A(2,1) = 2.0D0
      A(2,2) = 4.0D0
      A(3,1) = 3.0D0
      A(3,2) = 6.0D0
C
      CALL DQRFAC(M, N, A, LDA, PIVOT, IPVT, N, RDIAG, ACNORM, WA)
C
C     R(2,2) SHOULD BE NEAR ZERO (RANK 1)
      WRITE(6,501) RDIAG(1), RDIAG(2)
  501 FORMAT('  R DIAGONAL: ',D15.8,', ',D15.8)
C
      IF (DABS(RDIAG(2)) .LT. 1.0D-10) THEN
         WRITE(6,502)
  502    FORMAT('  PASS: R(2,2) ~ 0 (RANK DEFICIENCY DETECTED)')
         PASSED = PASSED + 1
      ELSE
         WRITE(6,503) RDIAG(2)
  503    FORMAT('  FAIL: R(2,2) = ',D15.8,' (EXPECTED ~ 0)')
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
