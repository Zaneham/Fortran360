# MINPACK Test Suite for IBM System/360

*In Which We Persuade 1980s Optimisation Algorithms to Run on 1960s Hardware, and They Agree Only Reluctantly*

## Overview

This directory contains FORTRAN IV compatible tests for the MINPACK optimisation library, designed to execute on authentic IBM FORTRAN G/H compilers under MVT on Hercules emulation. One might reasonably ask why anyone would do this. One might also reasonably ask why anyone climbs Everest, and the answer is much the same: because it's there, and because it seemed like a good idea at the time.

MINPACK was developed at Argonne National Laboratory in the late 1970s and early 1980s by Jorge Moré, Burton Garbow, and Kenneth Hillstrom. It remains one of the most widely used optimisation libraries in scientific computing, which says something either about its quality or about the scientific community's reluctance to change working code. Probably both.

## Test Files

| File | Description | Routines Tested | Lines of FORTRAN |
|------|-------------|-----------------|------------------|
| `test_enorm.f` | Euclidean norm computation | DENORM, ENORM | 231 |
| `test_qrfac.f` | QR factorisation with pivoting | DQRFAC, QRFAC | 215 |
| `test_dnls1.f` | Nonlinear least squares | DNLS1, SNLS1 | 340 |
| `test_dnsq.f` | Nonlinear equations | DNSQ, SNSQ | 350 |

## Supporting Files

| File | Description |
|------|-------------|
| `denorm.f` | DENORM function (double precision Euclidean norm, implemented with the sort of paranoid care one normally reserves for defusing explosives) |

## Test Problems

### Nonlinear Least Squares (DNLS1)

The Levenberg-Marquardt algorithm, named after two mathematicians who independently discovered it and presumably had an awkward conversation at a conference later, is tested against three classical problems:

1. **Rosenbrock Function** (2 variables)
   - The notorious "banana valley" function, so called because its contours resemble a banana, and because optimisers tend to go bananas trying to navigate it
   - Starting point: (-1.2, 1.0), which is about as far from the solution as one can get whilst still being in the same postcode
   - Solution: (1, 1)
   - Reference: Rosenbrock (1960), who presumably had better things to do but did this instead

2. **Powell Singular Function** (4 variables)
   - Tests behaviour near singularity, where most algorithms begin to question their life choices
   - Starting point: (3, -1, 0, 1)
   - Solution: (0, 0, 0, 0), the mathematical equivalent of giving up
   - Reference: Powell (1962)

3. **Freudenstein-Roth Function** (2 variables)
   - Has both global and local minima, like a mathematical Choose Your Own Adventure
   - Starting point: (0.5, -2)
   - Global minimum: (5, 4), though the algorithm may settle for the local minimum like a tired hiker accepting the nearest pub
   - Reference: Freudenstein & Roth (1963)

### Nonlinear Equations (DNSQ)

Powell's hybrid method, which combines the best features of Newton's method and something called "dog-leg steps" (not, as far as we know, named after an actual dog), is tested against:

1. **Helical Valley Function** (3 variables)
   - Tests 3D geometry with transcendental functions, because why make things simple
   - Starting point: (-1, 0, 0)
   - Solution: (1, 0, 0)
   - Reference: Fletcher & Powell (1963)

2. **Trigonometric Function** (N variables)
   - A scalable test problem, for when you want your computer to suffer proportionally to N
   - Starting point: (1/N, ..., 1/N)
   - Reference: Spedicato (1975)

3. **Broyden Tridiagonal** (N variables)
   - Sparse Jacobian structure, which sounds like a prog rock album but is actually quite useful
   - Starting point: (-1, ..., -1), the mathematical expression of pessimism
   - Reference: Broyden (1965)

## ENORM/DENORM Tests

Tests the scaled Euclidean norm computation, which is designed to calculate sqrt(x₁² + x₂² + ... + xₙ²) without either:
- Overflowing to infinity (embarrassing)
- Underflowing to zero (equally embarrassing)
- Producing the wrong answer (most embarrassing of all)

Test cases:
1. Unit vector (norm = 1) — the trivial case, included to ensure we haven't broken something fundamental
2. Vector of ones (norm = √N) — still trivial, but with more confidence
3. 3-4-5 triangle (norm = 5) — Pythagoras would be proud, or at least unsurprised
4. Large components (10¹⁵ scale) — where lesser algorithms fear to tread
5. Small components (10⁻¹⁵ scale) — where lesser algorithms fail to notice anything at all
6. Mixed magnitude (10⁻¹⁸ to 10¹⁸) — the stress test that separates the robust from the rubbish
7. Zero vector — should return zero; anything else indicates a fundamental misunderstanding of mathematics

## Running Tests

### Prerequisites

1. Hercules mainframe emulator with MVT (the operating system, not the band)
2. IBM FORTRAN G compiler (IEYFORT)
3. Linkage editor (IEWL) with FORTLIB
4. A cup of tea (optional but recommended)

### Compilation

```jcl
//MINPACK JOB (TEST),'MINPACK',CLASS=A,MSGCLASS=A
//FORT EXEC FORTGCLG
//FORT.SYSIN DD *
[contents of test file]
/*
//LKED.SYSLIB DD DSN=SYS1.FORTLIB,DISP=SHR
//GO.SYSPRINT DD SYSOUT=A
```

### Using fortran360 CLI

```bash
cd /c/dev/fortran360
python -m src.fortran360.cli run tests/slatec/minpack/test_enorm.f
python -m src.fortran360.cli run tests/slatec/minpack/test_dnls1.f
python -m src.fortran360.cli run tests/slatec/minpack/test_dnsq.f
```

## Expected Results

All tests should report `RESULT: ALL PASS` when run on a properly configured IBM 360 or compatible modern compiler. If they do not, please ensure:

1. The computer is plugged in
2. The computer is turned on
3. You have not accidentally loaded COBOL instead of FORTRAN
4. Mercury is not in retrograde

### IBM 360 Specific Notes

1. **Hexadecimal Floating-Point**: IBM 360 uses base-16 floating-point rather than IEEE 754, because in the 1960s IBM believed they knew better. They may have been right. The jury is still out.
   - Machine epsilon: ~10⁻¹⁶ (roughly)
   - Overflow threshold: ~10⁷⁵ (quite large)
   - Underflow threshold: ~10⁻⁷⁸ (quite small)

2. **No IMPLICIT NONE**: FORTRAN IV doesn't support IMPLICIT NONE. All variables follow the I-N integer convention, which assumes programmers can remember which letters go with which types. They could not, and FORTRAN 77 fixed this. Eventually.

3. **No IF-THEN-ELSE**: Uses arithmetic IF and logical IF only, because structured programming hadn't been invented yet.

4. **Fixed-Form Source**: Columns 1-72 only, with column 6 for continuation. Column 73 onwards is ignored, which was handy for sequence numbers in the punch card era and is now handy for nothing at all.

## References

- Moré, J.J., Garbow, B.S., Hillstrom, K.E. (1980). User Guide for MINPACK-1. ANL-80-74. Argonne National Laboratory.
- Moré, J.J., Garbow, B.S., Hillstrom, K.E. (1981). Testing Unconstrained Optimisation Software. ACM TOMS 7(1), 17-41.
- Dennis, J.E., Schnabel, R.B. (1983). Numerical Methods for Unconstrained Optimisation and Nonlinear Equations. Prentice-Hall.
- Levenberg, K. (1944). A Method for the Solution of Certain Problems in Least Squares. Quarterly of Applied Mathematics 2, 164-168.
- Marquardt, D. (1963). An Algorithm for Least-Squares Estimation of Nonlinear Parameters. SIAM Journal on Applied Mathematics 11, 431-441.

## Historical Note

MINPACK was written at Argonne National Laboratory and is in the public domain as a work of the U.S. Government. The algorithms implemented are based on work by Levenberg (1944) and Marquardt (1963) for nonlinear least squares, and Powell (1970) for the hybrid method.

These tests validate that optimisation algorithms from the 1980s produce correct results on hardware from the 1960s. This is either a remarkable testament to the portability of well-designed numerical software, or evidence that mathematicians are constitutionally incapable of throwing anything away.

One suspects it is both.

---

*Tests created: January 2026*
*Framework: fortran360*
*Target: IBM System/360 (Hercules/TK4-)*
*Tea consumed: Several cups*
