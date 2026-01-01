C     TEST PROGRAM FOR DPOFA/DPOSL (CHOLESKY FACTORIZATION)
C     FORTRAN IV COMPATIBLE - IBM SYSTEM/360
C
C     TESTS:
C       1. 2X2 SPD MATRIX
C       2. 3X3 IDENTITY MATRIX
C       3. 3X3 TRIDIAGONAL SPD
C       4. NON-SPD DETECTION
C
      PROGRAM TDPOFA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(3,3), B(3), XGOLD(3)
      INTEGER NPASS, NFAIL
C
      NPASS = 0
      NFAIL = 0
C
      WRITE(6,100)
  100 FORMAT('1','DPOFA/DPOSL TEST PROGRAM'/
     *       ' ','========================'/)
C
C     TEST 1: 2X2 SPD MATRIX
C     [4 2] [X1]   [6]
C     [2 5] [X2] = [7]   X = (1, 1)
C
      A(1,1) = 4.0D0
      A(1,2) = 2.0D0
      A(2,1) = 2.0D0
      A(2,2) = 5.0D0
      B(1) = 6.0D0
      B(2) = 7.0D0
C
      CALL DPOFA(A, 3, 2, INFO)
      IF (INFO .NE. 0) GO TO 110
      CALL DPOSL(A, 3, 2, B)
C
      ERR1 = DABS(B(1) - 1.0D0)
      ERR2 = DABS(B(2) - 1.0D0)
      IF (ERR1 .LT. 1.0D-10 .AND. ERR2 .LT. 1.0D-10) GO TO 105
      WRITE(6,101) B(1), B(2)
  101 FORMAT(' ','[FAIL] 2X2 SPD: X =',2F12.6)
      NFAIL = NFAIL + 1
      GO TO 120
  105 WRITE(6,102)
  102 FORMAT(' ','[PASS] 2X2 SPD: X = (1, 1)')
      NPASS = NPASS + 1
      GO TO 120
  110 WRITE(6,103) INFO
  103 FORMAT(' ','[FAIL] 2X2 SPD: CHOLESKY FAILED AT',I3)
      NFAIL = NFAIL + 1
C
C     TEST 2: 3X3 IDENTITY MATRIX
C
  120 CONTINUE
      A(1,1) = 1.0D0
      A(1,2) = 0.0D0
      A(1,3) = 0.0D0
      A(2,1) = 0.0D0
      A(2,2) = 1.0D0
      A(2,3) = 0.0D0
      A(3,1) = 0.0D0
      A(3,2) = 0.0D0
      A(3,3) = 1.0D0
      B(1) = 1.0D0
      B(2) = 2.0D0
      B(3) = 3.0D0
      XGOLD(1) = 1.0D0
      XGOLD(2) = 2.0D0
      XGOLD(3) = 3.0D0
C
      CALL DPOFA(A, 3, 3, INFO)
      IF (INFO .NE. 0) GO TO 130
      CALL DPOSL(A, 3, 3, B)
C
      ERRMAX = 0.0D0
      DO 125 I = 1, 3
        ERR = DABS(B(I) - XGOLD(I))
        IF (ERR .GT. ERRMAX) ERRMAX = ERR
  125 CONTINUE
      IF (ERRMAX .LT. 1.0D-10) GO TO 126
      WRITE(6,121) B(1), B(2), B(3)
  121 FORMAT(' ','[FAIL] IDENTITY: X =',3F12.6)
      NFAIL = NFAIL + 1
      GO TO 140
  126 WRITE(6,122)
  122 FORMAT(' ','[PASS] IDENTITY: CHOL(I)*X = B GIVES X = B')
      NPASS = NPASS + 1
      GO TO 140
  130 WRITE(6,123) INFO
  123 FORMAT(' ','[FAIL] IDENTITY: CHOLESKY FAILED AT',I3)
      NFAIL = NFAIL + 1
C
C     TEST 3: 3X3 TRIDIAGONAL SPD (COMMON IN PDEs)
C     [ 2 -1  0]       [1]
C     [-1  2 -1] * X = [0]   X = (1, 1, 1)
C     [ 0 -1  2]       [1]
C
  140 CONTINUE
      A(1,1) = 2.0D0
      A(1,2) = -1.0D0
      A(1,3) = 0.0D0
      A(2,1) = -1.0D0
      A(2,2) = 2.0D0
      A(2,3) = -1.0D0
      A(3,1) = 0.0D0
      A(3,2) = -1.0D0
      A(3,3) = 2.0D0
      B(1) = 1.0D0
      B(2) = 0.0D0
      B(3) = 1.0D0
      XGOLD(1) = 1.0D0
      XGOLD(2) = 1.0D0
      XGOLD(3) = 1.0D0
C
      CALL DPOFA(A, 3, 3, INFO)
      IF (INFO .NE. 0) GO TO 150
      CALL DPOSL(A, 3, 3, B)
C
      ERRMAX = 0.0D0
      DO 145 I = 1, 3
        ERR = DABS(B(I) - XGOLD(I))
        IF (ERR .GT. ERRMAX) ERRMAX = ERR
  145 CONTINUE
      IF (ERRMAX .LT. 1.0D-10) GO TO 146
      WRITE(6,141) B(1), B(2), B(3)
  141 FORMAT(' ','[FAIL] TRIDIAG: X =',3F12.6)
      NFAIL = NFAIL + 1
      GO TO 160
  146 WRITE(6,142)
  142 FORMAT(' ','[PASS] TRIDIAGONAL SPD: X = (1, 1, 1)')
      NPASS = NPASS + 1
      GO TO 160
  150 WRITE(6,143) INFO
  143 FORMAT(' ','[FAIL] TRIDIAG: CHOLESKY FAILED AT',I3)
      NFAIL = NFAIL + 1
C
C     TEST 4: NON-SPD MATRIX (SHOULD FAIL)
C     [1 2]
C     [2 1]  HAS NEGATIVE EIGENVALUE
C
  160 CONTINUE
      A(1,1) = 1.0D0
      A(1,2) = 2.0D0
      A(2,1) = 2.0D0
      A(2,2) = 1.0D0
C
      CALL DPOFA(A, 3, 2, INFO)
      IF (INFO .NE. 0) GO TO 165
      WRITE(6,161)
  161 FORMAT(' ','[FAIL] NON-SPD NOT DETECTED')
      NFAIL = NFAIL + 1
      GO TO 180
  165 WRITE(6,162) INFO
  162 FORMAT(' ','[PASS] NON-SPD CORRECTLY REJECTED AT COL',I3)
      NPASS = NPASS + 1
C
C     SUMMARY
C
  180 CONTINUE
      WRITE(6,200)
  200 FORMAT(/' ','========================')
      WRITE(6,201) NPASS, NFAIL
  201 FORMAT(' ','SUMMARY:',I3,' PASSED,',I3,' FAILED')
      WRITE(6,202)
  202 FORMAT(' ','========================')
C
      IF (NFAIL .GT. 0) STOP 1
      STOP
      END
C
C     LOCAL DPOFA IMPLEMENTATION
C
      SUBROUTINE DPOFA(A, LDA, N, INFO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(LDA,*)
C
      DO 30 J = 1, N
        S = 0.0D0
        JM1 = J - 1
        IF (JM1 .LT. 1) GO TO 20
        DO 10 K = 1, JM1
          T = A(K,J)
          KM1 = K - 1
          IF (KM1 .LT. 1) GO TO 5
          DO 4 I = 1, KM1
            T = T - A(I,K) * A(I,J)
    4     CONTINUE
    5     CONTINUE
          T = T / A(K,K)
          A(K,J) = T
          S = S + T * T
   10   CONTINUE
   20   CONTINUE
        S = A(J,J) - S
        IF (S .LE. 0.0D0) GO TO 40
        A(J,J) = DSQRT(S)
   30 CONTINUE
      INFO = 0
      RETURN
C
   40 INFO = J
      RETURN
      END
C
C     LOCAL DPOSL IMPLEMENTATION
C
      SUBROUTINE DPOSL(A, LDA, N, B)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(LDA,*), B(*)
C
C     FORWARD SUBSTITUTION: SOLVE L*Y = B
      DO 20 K = 1, N
        T = 0.0D0
        KM1 = K - 1
        IF (KM1 .LT. 1) GO TO 10
        DO 5 J = 1, KM1
          T = T + A(J,K) * B(J)
    5   CONTINUE
   10   CONTINUE
        B(K) = (B(K) - T) / A(K,K)
   20 CONTINUE
C
C     BACK SUBSTITUTION: SOLVE L'*X = Y
      DO 40 KB = 1, N
        K = N + 1 - KB
        T = 0.0D0
        KP1 = K + 1
        IF (KP1 .GT. N) GO TO 30
        DO 25 J = KP1, N
          T = T + A(K,J) * B(J)
   25   CONTINUE
   30   CONTINUE
        B(K) = (B(K) - T) / A(K,K)
   40 CONTINUE
C
      RETURN
      END
