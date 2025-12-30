C     TEST02 - DO LOOP OPERATIONS
C     TESTS VARIOUS DO LOOP CONSTRUCTS
C
      INTEGER I, J, K, SUM, N
      REAL TOTAL, X
C
C     SIMPLE DO LOOP - SUM 1 TO 10
      SUM = 0
      DO 10 I = 1, 10
         SUM = SUM + I
   10 CONTINUE
      WRITE(6,100) SUM
  100 FORMAT(' SUM OF 1 TO 10 = ',I5)
C
C     DO LOOP WITH STEP
      SUM = 0
      DO 20 I = 2, 20, 2
         SUM = SUM + I
   20 CONTINUE
      WRITE(6,110) SUM
  110 FORMAT(' SUM OF EVENS 2 TO 20 = ',I5)
C
C     NESTED DO LOOPS
      K = 0
      DO 40 I = 1, 3
         DO 30 J = 1, 4
            K = K + 1
   30    CONTINUE
   40 CONTINUE
      WRITE(6,120) K
  120 FORMAT(' NESTED LOOP COUNT = ',I5)
C
C     FACTORIAL USING DO LOOP
      N = 6
      K = 1
      DO 50 I = 1, N
         K = K * I
   50 CONTINUE
      WRITE(6,130) N, K
  130 FORMAT(' FACTORIAL OF ',I3,' = ',I10)
C
C     REAL SUMMATION
      TOTAL = 0.0
      DO 60 I = 1, 5
         X = I
         TOTAL = TOTAL + 1.0/X
   60 CONTINUE
      WRITE(6,140) TOTAL
  140 FORMAT(' HARMONIC SUM (5 TERMS) = ',F10.6)
C
      WRITE(6,999)
  999 FORMAT(' TEST02 COMPLETE')
      STOP
      END
