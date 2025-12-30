C     TEST14 - POLYNOMIAL EVALUATION
C     HORNER'S METHOD AND POLYNOMIAL OPERATIONS
C
      REAL COEF(6), X, Y
      REAL ROOTS(5), VALS(5)
      INTEGER I, N
C
      WRITE(6,100)
  100 FORMAT(' POLYNOMIAL EVALUATION TESTS:')
C
C     P(X) = 1 + 2X + 3X^2 + 4X^3 + 5X^4
C     COEF(1) = CONSTANT TERM
      COEF(1) = 1.0
      COEF(2) = 2.0
      COEF(3) = 3.0
      COEF(4) = 4.0
      COEF(5) = 5.0
      N = 5
C
      WRITE(6,110)
  110 FORMAT(' P(X) = 1 + 2X + 3X^2 + 4X^3 + 5X^4')
      WRITE(6,120)
  120 FORMAT(' EVALUATING AT VARIOUS POINTS:')
C
      DO 10 I = -2, 3
         X = I
         CALL HORNER(COEF, N, X, Y)
         WRITE(6,130) X, Y
  130    FORMAT('   P(',F5.1,') = ',F12.4)
   10 CONTINUE
C
C     TEST POLYNOMIAL DERIVATIVE
C     P'(X) = 2 + 6X + 12X^2 + 20X^3
      WRITE(6,200)
  200 FORMAT(/,' DERIVATIVE P''(X) = 2 + 6X + 12X^2 + 20X^3:')
C
      DO 20 I = 0, 3
         X = I
         CALL HORNRD(COEF, N, X, Y)
         WRITE(6,210) X, Y
  210    FORMAT('   P''(',F5.1,') = ',F12.4)
   20 CONTINUE
C
C     TEST WITH CHEBYSHEV POLYNOMIAL T4(X)
C     T4(X) = 8X^4 - 8X^2 + 1
      WRITE(6,300)
  300 FORMAT(/,' CHEBYSHEV T4(X) = 8X^4 - 8X^2 + 1:')
      COEF(1) = 1.0
      COEF(2) = 0.0
      COEF(3) = -8.0
      COEF(4) = 0.0
      COEF(5) = 8.0
C
C     T4 HAS ROOTS AT COS(PI/8), COS(3PI/8), COS(5PI/8), COS(7PI/8)
      ROOTS(1) = COS(3.14159265/8.0)
      ROOTS(2) = COS(3.0*3.14159265/8.0)
      ROOTS(3) = COS(5.0*3.14159265/8.0)
      ROOTS(4) = COS(7.0*3.14159265/8.0)
C
      WRITE(6,310)
  310 FORMAT(' ROOTS OF T4(X) AND VERIFICATION:')
      DO 30 I = 1, 4
         CALL HORNER(COEF, N, ROOTS(I), VALS(I))
         WRITE(6,320) ROOTS(I), VALS(I)
  320    FORMAT('   X=',F10.6,' T4(X)=',E12.4)
   30 CONTINUE
C
      WRITE(6,999)
  999 FORMAT(' TEST14 COMPLETE')
      STOP
      END
C
C     HORNER'S METHOD FOR POLYNOMIAL EVALUATION
C     P(X) = COEF(1) + COEF(2)*X + COEF(3)*X^2 + ...
C
      SUBROUTINE HORNER(COEF, N, X, Y)
      INTEGER N, I
      REAL COEF(1), X, Y
C
      Y = COEF(N)
      DO 10 I = N-1, 1, -1
         Y = Y * X + COEF(I)
   10 CONTINUE
      RETURN
      END
C
C     HORNER'S METHOD FOR DERIVATIVE
C     P'(X) = COEF(2) + 2*COEF(3)*X + 3*COEF(4)*X^2 + ...
C
      SUBROUTINE HORNRD(COEF, N, X, DY)
      INTEGER N, I
      REAL COEF(1), X, DY
C
      DY = (N-1) * COEF(N)
      DO 10 I = N-1, 2, -1
         DY = DY * X + (I-1) * COEF(I)
   10 CONTINUE
      RETURN
      END
