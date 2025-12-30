C     TEST05 - MATHEMATICAL FUNCTIONS
C     TESTS INTRINSIC MATH FUNCTIONS
C
      REAL X, Y, PI
      REAL S, C, T, SQ, EX, AL
C
      PI = 3.14159265
C
      WRITE(6,100)
  100 FORMAT(' MATHEMATICAL FUNCTION TESTS:')
C
C     TRIGONOMETRIC FUNCTIONS
      X = PI / 4.0
      S = SIN(X)
      C = COS(X)
      T = TAN(X)
      WRITE(6,110) X, S, C, T
  110 FORMAT(' X=',F8.5,' SIN=',F8.5,' COS=',F8.5,' TAN=',F8.5)
C
C     SQUARE ROOT
      X = 2.0
      SQ = SQRT(X)
      WRITE(6,120) X, SQ
  120 FORMAT(' SQRT(',F5.2,') = ',F10.6)
C
      X = 144.0
      SQ = SQRT(X)
      WRITE(6,120) X, SQ
C
C     EXPONENTIAL AND LOG
      X = 1.0
      EX = EXP(X)
      AL = ALOG(EX)
      WRITE(6,130) X, EX, AL
  130 FORMAT(' X=',F5.2,' EXP=',F10.6,' LOG(EXP)=',F10.6)
C
      X = 2.0
      EX = EXP(X)
      AL = ALOG(EX)
      WRITE(6,130) X, EX, AL
C
C     ABSOLUTE VALUE
      X = -5.5
      Y = ABS(X)
      WRITE(6,140) X, Y
  140 FORMAT(' ABS(',F8.3,') = ',F8.3)
C
C     MAX AND MIN
      X = 10.5
      Y = 7.3
      WRITE(6,150) X, Y, AMAX1(X,Y), AMIN1(X,Y)
  150 FORMAT(' MAX(',F5.1,',',F5.1,')=',F5.1,
     1       ' MIN=',F5.1)
C
C     POWER FUNCTION
      X = 2.0
      Y = 10.0
      WRITE(6,160) X, Y, X**Y
  160 FORMAT(' ',F4.1,' ** ',F4.1,' = ',F12.2)
C
      WRITE(6,999)
  999 FORMAT(' TEST05 COMPLETE')
      STOP
      END
