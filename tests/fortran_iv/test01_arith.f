C     TEST01 - BASIC ARITHMETIC OPERATIONS
C     TESTS INTEGER AND REAL ARITHMETIC
C
      INTEGER I, J, K, L
      REAL A, B, C, D
C
C     INTEGER ARITHMETIC
      I = 10
      J = 3
      K = I + J
      L = I - J
      WRITE(6,100) I, J, K, L
  100 FORMAT(' I=',I5,' J=',I5,' I+J=',I5,' I-J=',I5)
C
      K = I * J
      L = I / J
      WRITE(6,110) K, L
  110 FORMAT(' I*J=',I5,' I/J=',I5)
C
C     REAL ARITHMETIC
      A = 10.5
      B = 3.2
      C = A + B
      D = A - B
      WRITE(6,200) A, B, C, D
  200 FORMAT(' A=',F8.3,' B=',F8.3,' A+B=',F8.3,' A-B=',F8.3)
C
      C = A * B
      D = A / B
      WRITE(6,210) C, D
  210 FORMAT(' A*B=',F10.4,' A/B=',F10.4)
C
C     MIXED MODE
      C = A + I
      D = B * J
      WRITE(6,300) C, D
  300 FORMAT(' A+I=',F10.4,' B*J=',F10.4)
C
      WRITE(6,999)
  999 FORMAT(' TEST01 COMPLETE')
      STOP
      END
