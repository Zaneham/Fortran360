C     TEST13 - MATRIX OPERATIONS
C     BASIC LINEAR ALGEBRA OPERATIONS
C
      REAL A(3,3), B(3,3), C(3,3)
      REAL V(3), W(3)
      INTEGER I, J
C
      WRITE(6,100)
  100 FORMAT(' MATRIX OPERATION TESTS:')
C
C     INITIALIZE MATRIX A
      A(1,1) = 1.0
      A(1,2) = 2.0
      A(1,3) = 3.0
      A(2,1) = 4.0
      A(2,2) = 5.0
      A(2,3) = 6.0
      A(3,1) = 7.0
      A(3,2) = 8.0
      A(3,3) = 9.0
C
C     INITIALIZE MATRIX B (IDENTITY)
      DO 20 I = 1, 3
         DO 10 J = 1, 3
            B(I,J) = 0.0
            IF (I .EQ. J) B(I,J) = 1.0
   10    CONTINUE
   20 CONTINUE
C
      WRITE(6,110)
  110 FORMAT(' MATRIX A:')
      CALL PRTMAT(A, 3, 3)
C
      WRITE(6,120)
  120 FORMAT(' MATRIX B (IDENTITY):')
      CALL PRTMAT(B, 3, 3)
C
C     MATRIX ADDITION
      CALL MATADD(A, B, C, 3, 3)
      WRITE(6,130)
  130 FORMAT(' A + B:')
      CALL PRTMAT(C, 3, 3)
C
C     MATRIX MULTIPLICATION A * B
      CALL MATMUL(A, B, C, 3, 3, 3)
      WRITE(6,140)
  140 FORMAT(' A * B:')
      CALL PRTMAT(C, 3, 3)
C
C     MATRIX-VECTOR MULTIPLICATION
      V(1) = 1.0
      V(2) = 0.0
      V(3) = 0.0
      CALL MATVEC(A, V, W, 3, 3)
      WRITE(6,200)
  200 FORMAT(' A * [1,0,0] = ')
      WRITE(6,210) (W(I), I=1,3)
  210 FORMAT(3F10.4)
C
      V(1) = 1.0
      V(2) = 1.0
      V(3) = 1.0
      CALL MATVEC(A, V, W, 3, 3)
      WRITE(6,220)
  220 FORMAT(' A * [1,1,1] = ')
      WRITE(6,210) (W(I), I=1,3)
C
C     MATRIX TRANSPOSE
      CALL MATTRS(A, C, 3, 3)
      WRITE(6,300)
  300 FORMAT(' TRANSPOSE OF A:')
      CALL PRTMAT(C, 3, 3)
C
      WRITE(6,999)
  999 FORMAT(' TEST13 COMPLETE')
      STOP
      END
C
C     PRINT MATRIX
C
      SUBROUTINE PRTMAT(A, M, N)
      INTEGER M, N, I, J
      REAL A(3,3)
      DO 10 I = 1, M
         WRITE(6,100) (A(I,J), J=1,N)
  100    FORMAT(3F10.4)
   10 CONTINUE
      RETURN
      END
C
C     MATRIX ADDITION C = A + B
C
      SUBROUTINE MATADD(A, B, C, M, N)
      INTEGER M, N, I, J
      REAL A(3,3), B(3,3), C(3,3)
      DO 20 I = 1, M
         DO 10 J = 1, N
            C(I,J) = A(I,J) + B(I,J)
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
C
C     MATRIX MULTIPLICATION C = A * B
C
      SUBROUTINE MATMUL(A, B, C, M, K, N)
      INTEGER M, K, N, I, J, L
      REAL A(3,3), B(3,3), C(3,3), SUM
      DO 30 I = 1, M
         DO 20 J = 1, N
            SUM = 0.0
            DO 10 L = 1, K
               SUM = SUM + A(I,L) * B(L,J)
   10       CONTINUE
            C(I,J) = SUM
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
C
C     MATRIX-VECTOR MULTIPLICATION W = A * V
C
      SUBROUTINE MATVEC(A, V, W, M, N)
      INTEGER M, N, I, J
      REAL A(3,3), V(1), W(1), SUM
      DO 20 I = 1, M
         SUM = 0.0
         DO 10 J = 1, N
            SUM = SUM + A(I,J) * V(J)
   10    CONTINUE
         W(I) = SUM
   20 CONTINUE
      RETURN
      END
C
C     MATRIX TRANSPOSE B = A^T
C
      SUBROUTINE MATTRS(A, B, M, N)
      INTEGER M, N, I, J
      REAL A(3,3), B(3,3)
      DO 20 I = 1, M
         DO 10 J = 1, N
            B(J,I) = A(I,J)
   10    CONTINUE
   20 CONTINUE
      RETURN
      END
