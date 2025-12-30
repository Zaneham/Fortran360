C     TEST06 - LOGICAL OPERATIONS
C     TESTS LOGICAL VARIABLES AND OPERATORS
C
      LOGICAL L1, L2, L3, L4
      INTEGER I, J, K
C
      WRITE(6,100)
  100 FORMAT(' LOGICAL OPERATION TESTS:')
C
C     BASIC LOGICAL VALUES
      L1 = .TRUE.
      L2 = .FALSE.
C
C     LOGICAL AND
      L3 = L1 .AND. L2
      WRITE(6,110) L1, L2, L3
  110 FORMAT(' ',L1,' .AND. ',L1,' = ',L1)
C
      L3 = L1 .AND. L1
      WRITE(6,110) L1, L1, L3
C
C     LOGICAL OR
      L3 = L1 .OR. L2
      WRITE(6,120) L1, L2, L3
  120 FORMAT(' ',L1,' .OR. ',L1,' = ',L1)
C
      L3 = L2 .OR. L2
      WRITE(6,120) L2, L2, L3
C
C     LOGICAL NOT
      L3 = .NOT. L1
      L4 = .NOT. L2
      WRITE(6,130) L1, L3
  130 FORMAT(' .NOT. ',L1,' = ',L1)
      WRITE(6,130) L2, L4
C
C     COMPARISON OPERATORS
      I = 5
      J = 10
      L1 = I .LT. J
      L2 = I .GT. J
      L3 = I .EQ. J
      L4 = I .NE. J
      WRITE(6,200) I, J
  200 FORMAT(' I=',I3,' J=',I3)
      WRITE(6,210) L1, L2, L3, L4
  210 FORMAT(' LT=',L1,' GT=',L1,' EQ=',L1,' NE=',L1)
C
C     COMPOUND CONDITIONS
      K = 7
      L1 = (I .LT. K) .AND. (K .LT. J)
      WRITE(6,220) I, K, J, L1
  220 FORMAT(' ',I2,' < ',I2,' < ',I2,' IS ',L1)
C
      WRITE(6,999)
  999 FORMAT(' TEST06 COMPLETE')
      STOP
      END
