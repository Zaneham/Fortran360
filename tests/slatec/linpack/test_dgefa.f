C     TEST PROGRAM FOR DGEFA/DGESL (LU FACTORIZATION)
C     FORTRAN IV COMPATIBLE - IBM SYSTEM/360
C
C     TESTS:
C       1. 2X2 SIMPLE SYSTEM
C       2. 3X3 IDENTITY MATRIX
C       3. 3X3 PASCAL MATRIX
C       4. 3X3 VANDERMONDE MATRIX
C
      PROGRAM TDGEFA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(3,3), B(3), XGOLD(3)
      INTEGER IPVT(3)
      INTEGER NPASS, NFAIL
C
      NPASS = 0
      NFAIL = 0
C
      WRITE(6,100)
  100 FORMAT('1','DGEFA/DGESL TEST PROGRAM'/
     *       ' ','========================'/)
C
C     TEST 1: 2X2 SYSTEM
C     [2 1] [X1]   [5]
C     [1 3] [X2] = [7]
C     SOLUTION: X = (1.6, 1.8)
C
      A(1,1) = 2.0D0
      A(1,2) = 1.0D0
      A(2,1) = 1.0D0
      A(2,2) = 3.0D0
      B(1) = 5.0D0
      B(2) = 7.0D0
C
      CALL DGEFA(A, 3, 2, IPVT, INFO)
      IF (INFO .NE. 0) GO TO 110
      CALL DGESL(A, 3, 2, IPVT, B, 0)
C
      ERR1 = DABS(B(1) - 1.6D0)
      ERR2 = DABS(B(2) - 1.8D0)
      IF (ERR1 .LT. 1.0D-10 .AND. ERR2 .LT. 1.0D-10) GO TO 105
      WRITE(6,101) B(1), B(2)
  101 FORMAT(' ','[FAIL] 2X2 SYSTEM: X =',2F12.6)
      NFAIL = NFAIL + 1
      GO TO 120
  105 WRITE(6,102)
  102 FORMAT(' ','[PASS] 2X2 SYSTEM: X = (1.6, 1.8)')
      NPASS = NPASS + 1
      GO TO 120
  110 WRITE(6,103)
  103 FORMAT(' ','[FAIL] 2X2 SYSTEM: FACTORIZATION FAILED')
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
C
      CALL DGEFA(A, 3, 3, IPVT, INFO)
      IF (INFO .NE. 0) GO TO 130
      CALL DGESL(A, 3, 3, IPVT, B, 0)
C
      ERR1 = DABS(B(1) - 1.0D0)
      ERR2 = DABS(B(2) - 2.0D0)
      ERR3 = DABS(B(3) - 3.0D0)
      IF (ERR1 .LT. 1.0D-10 .AND. ERR2 .LT. 1.0D-10 .AND.
     *    ERR3 .LT. 1.0D-10) GO TO 125
      WRITE(6,121) B(1), B(2), B(3)
  121 FORMAT(' ','[FAIL] IDENTITY: X =',3F12.6)
      NFAIL = NFAIL + 1
      GO TO 140
  125 WRITE(6,122)
  122 FORMAT(' ','[PASS] IDENTITY: I*X = B GIVES X = B')
      NPASS = NPASS + 1
      GO TO 140
  130 WRITE(6,123)
  123 FORMAT(' ','[FAIL] IDENTITY: FACTORIZATION FAILED')
      NFAIL = NFAIL + 1
C
C     TEST 3: PASCAL MATRIX 3X3
C     [1 1 1]       [3]
C     [1 2 3] * X = [6]   X = (1,1,1)
C     [1 3 6]       [10]
C
  140 CONTINUE
      A(1,1) = 1.0D0
      A(1,2) = 1.0D0
      A(1,3) = 1.0D0
      A(2,1) = 1.0D0
      A(2,2) = 2.0D0
      A(2,3) = 3.0D0
      A(3,1) = 1.0D0
      A(3,2) = 3.0D0
      A(3,3) = 6.0D0
      B(1) = 3.0D0
      B(2) = 6.0D0
      B(3) = 10.0D0
      XGOLD(1) = 1.0D0
      XGOLD(2) = 1.0D0
      XGOLD(3) = 1.0D0
C
      CALL DGEFA(A, 3, 3, IPVT, INFO)
      IF (INFO .NE. 0) GO TO 150
      CALL DGESL(A, 3, 3, IPVT, B, 0)
C
      ERRMAX = 0.0D0
      DO 145 I = 1, 3
        ERR = DABS(B(I) - XGOLD(I))
        IF (ERR .GT. ERRMAX) ERRMAX = ERR
  145 CONTINUE
      IF (ERRMAX .LT. 1.0D-10) GO TO 146
      WRITE(6,141) B(1), B(2), B(3)
  141 FORMAT(' ','[FAIL] PASCAL: X =',3F12.6)
      NFAIL = NFAIL + 1
      GO TO 160
  146 WRITE(6,142)
  142 FORMAT(' ','[PASS] PASCAL 3X3: X = (1, 1, 1)')
      NPASS = NPASS + 1
      GO TO 160
  150 WRITE(6,143)
  143 FORMAT(' ','[FAIL] PASCAL: FACTORIZATION FAILED')
      NFAIL = NFAIL + 1
C
C     TEST 4: VANDERMONDE 3X3 WITH NODES 1,2,3
C     [1 1 1]       [6]
C     [1 2 4] * X = [17]   X = (1,2,3)
C     [1 3 9]       [34]
C
  160 CONTINUE
      A(1,1) = 1.0D0
      A(1,2) = 1.0D0
      A(1,3) = 1.0D0
      A(2,1) = 1.0D0
      A(2,2) = 2.0D0
      A(2,3) = 4.0D0
      A(3,1) = 1.0D0
      A(3,2) = 3.0D0
      A(3,3) = 9.0D0
      B(1) = 6.0D0
      B(2) = 17.0D0
      B(3) = 34.0D0
      XGOLD(1) = 1.0D0
      XGOLD(2) = 2.0D0
      XGOLD(3) = 3.0D0
C
      CALL DGEFA(A, 3, 3, IPVT, INFO)
      IF (INFO .NE. 0) GO TO 170
      CALL DGESL(A, 3, 3, IPVT, B, 0)
C
      ERRMAX = 0.0D0
      DO 165 I = 1, 3
        ERR = DABS(B(I) - XGOLD(I))
        IF (ERR .GT. ERRMAX) ERRMAX = ERR
  165 CONTINUE
      IF (ERRMAX .LT. 1.0D-10) GO TO 166
      WRITE(6,161) B(1), B(2), B(3)
  161 FORMAT(' ','[FAIL] VANDERMONDE: X =',3F12.6)
      NFAIL = NFAIL + 1
      GO TO 180
  166 WRITE(6,162)
  162 FORMAT(' ','[PASS] VANDERMONDE 3X3: X = (1, 2, 3)')
      NPASS = NPASS + 1
      GO TO 180
  170 WRITE(6,163)
  163 FORMAT(' ','[FAIL] VANDERMONDE: FACTORIZATION FAILED')
      NFAIL = NFAIL + 1
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
C     LOCAL DGEFA IMPLEMENTATION
C
      SUBROUTINE DGEFA(A, LDA, N, IPVT, INFO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(LDA,*), IPVT(*)
C
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
C
      DO 60 K = 1, NM1
C       FIND PIVOT
        L = K
        KP1 = K + 1
        DO 10 J = KP1, N
          IF (DABS(A(J,K)) .GT. DABS(A(L,K))) L = J
   10   CONTINUE
        IPVT(K) = L
C
        IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C       SWAP ROWS
        IF (L .EQ. K) GO TO 20
        T = A(L,K)
        A(L,K) = A(K,K)
        A(K,K) = T
   20   CONTINUE
C
C       COMPUTE MULTIPLIERS
        T = -1.0D0 / A(K,K)
        DO 30 J = KP1, N
          A(J,K) = A(J,K) * T
   30   CONTINUE
C
C       ROW ELIMINATION
        DO 35 J = KP1, N
          T = A(L,J)
          IF (L .EQ. K) GO TO 32
          A(L,J) = A(K,J)
          A(K,J) = T
   32     CONTINUE
          DO 34 I = KP1, N
            A(I,J) = A(I,J) + T * A(I,K)
   34     CONTINUE
   35   CONTINUE
        GO TO 50
C
   40   INFO = K
        RETURN
C
   50   CONTINUE
   60 CONTINUE
C
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END
C
C     LOCAL DGESL IMPLEMENTATION
C
      SUBROUTINE DGESL(A, LDA, N, IPVT, B, JOB)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(LDA,*), IPVT(*), B(*)
C
      IF (JOB .NE. 0) RETURN
C
C     FORWARD SUBSTITUTION
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 30
      DO 20 K = 1, NM1
        L = IPVT(K)
        T = B(L)
        IF (L .EQ. K) GO TO 10
        B(L) = B(K)
        B(K) = T
   10   CONTINUE
        KP1 = K + 1
        DO 15 I = KP1, N
          B(I) = B(I) + T * A(I,K)
   15   CONTINUE
   20 CONTINUE
C
C     BACK SUBSTITUTION
   30 CONTINUE
      DO 50 KB = 1, N
        K = N + 1 - KB
        B(K) = B(K) / A(K,K)
        T = -B(K)
        KM1 = K - 1
        IF (KM1 .LT. 1) GO TO 50
        DO 40 I = 1, KM1
          B(I) = B(I) + T * A(I,K)
   40   CONTINUE
   50 CONTINUE
C
      RETURN
      END
