# FORTRAN IV Test Results

*A Somewhat Comprehensive Account of Persuading 1960s Software to Run on Hardware It Never Asked For*

## Executive Summary

I have successfully convinced IBM's FORTRAN G compiler (vintage 1966) to compile FORTRAN IV programs on what it believes to be a System/360 mainframe but is, in fact, a rather confused Windows PC running an emulator. The compiler remains blissfully unaware of this deception and continues to produce object code with the quiet dignity one might expect from software that predates the Moon landing.

FORTRAN H (1969) has proven more temperamental, much like a cat that has been shown a cucumber.

## Test Suite Overview

Fifteen test programs were crafted in authentic FORTRAN IV style, eschewing such modern frivolities as IF-THEN-ELSE blocks (a FORTRAN 77 innovation that wouldn't arrive for another decade, and which the 1966 compiler regards with the same suspicion a Victorian might regard a microwave oven).

### Basic Tests (01-10)

| Test | Description | FORTRAN G | FORTRAN H | Notes |
|------|-------------|-----------|-----------|-------|
| test01_arith.f | Arithmetic | PASS | Sulking | 2+2=4, which came as some relief |
| test02_doloop.f | DO Loops | PASS | Sulking | Successfully counted to 10 |
| test03_array.f | Arrays | PASS | Sulking | Remembered where it put things |
| test04_subr.f | Subroutines | PASS | Sulking | CALL and RETURN working harmoniously |
| test05_math.f | Math Functions | PASS | Sulking | Uses AMAX1/AMIN1 (the polite specific forms) |
| test06_logic.f | Logical Ops | PASS | Sulking | True remained true, false remained false |
| test07_goto.f | GOTO | PASS | Sulking | Successfully went to |
| test08_common.f | COMMON Blocks | PASS | Sulking | Variables shared their feelings |
| test09_format.f | FORMAT | PASS | Sulking | Output formatted with period-appropriate precision |
| test10_equiv.f | EQUIVALENCE | PASS | Sulking | Memory aliasing achieved without incident |

### Numerical Algorithm Tests (11-15)

| Test | Description | FORTRAN G | FORTRAN H | Notes |
|------|-------------|-----------|-----------|-------|
| test11_trapz.f | Trapezoidal Integration | FAIL | Sulking | Array passing causing runtime distress |
| test12_newton.f | Newton-Raphson | PASS | Sulking | Found roots; did not find FORTRAN H |
| test13_matrix.f | Matrix Operations | PASS | Sulking | Matrices multiplied correctly |
| test14_poly.f | Polynomial (Horner) | PASS | Sulking | Polynomials evaluated with Hornerian efficiency |
| test15_stat.f | Statistics | PASS | Sulking | Mean, variance, and correlation computed |

## Detailed Results

### FORTRAN G (1966) - The Cooperative One

FORTRAN G has proven to be a thoroughly agreeable compiler, much like a golden retriever in punch card form. It compiles without complaint, links with IEWL (the linkage editor), and executes programs with the reliability of a 1960s appliance that was built to last.

**Sample Output (test01_arith.f):**
```
 BASIC ARITHMETIC TEST:
  I = 10
  J = 3
  I + J = 13
  I - J = 7
  I * J = 30
  I / J = 3
  MOD(I,J) = 1
 TEST01 COMPLETE
```

The compiler successfully determined that 10 + 3 = 13, which is correct and a testament to IBM's quality engineering. One shudders to imagine the consequences had it concluded otherwise.

**Sample Output (test04_subr.f):**
```
 SUBROUTINE AND FUNCTION TEST:
 BEFORE SWAP: A = 10.0000  B = 20.0000
 AFTER SWAP:  A = 20.0000  B = 10.0000
 FACTORIAL(5) = 120.0000
 FACTORIAL(7) = 5040.0000
 AVERAGE OF 10, 20, 50 = 30.0000
 TEST04 COMPLETE
```

Subroutines were called. They returned. This is the correct behaviour.

### FORTRAN H (1969) - The Difficult One

FORTRAN H, the "optimising" compiler, has adopted the position that it would rather not compile anything at all, thank you very much. Despite our best efforts—including offering it more REGION, providing additional work files (SYSUT1, SYSUT2), and speaking to it in soothing tones—it consistently returns RC=0016 and the dreaded message:

```
COMPILATION DELETED
```

This is compiler-speak for "I have decided not to produce any object code today" and is delivered with the same finality as a British Rail announcement that your train has been cancelled due to leaves on the line.

**Attempted Remedies:**
1. Increased REGION to 256K, then 384K (it remained unimpressed)
2. Added SYSUT1 and SYSUT2 work files (it ignored them pointedly)
3. Used the FORTHCLG catalogued procedure (it complained about DDNAME resolution)
4. Offered it a cup of tea (the socket does not support beverage transmission)

**Technical Analysis:**

FORTRAN H was notoriously memory-hungry even by 1969 standards. It performed multiple compilation passes, built intermediate representations, and conducted dataflow analysis for optimisation. The machines it was designed for might have had 512K of core. Our REGION allocations, whilst generous by 1966 standards, may still be insufficient for what H wishes to accomplish during its optimisation phase.

Additionally, the FORTHCLG catalogued procedure may expect datasets that exist in a pristine MVS installation but have wandered off somewhere in TK4-. Catalogued procedures are like recipes handed down through generations—occasionally an ingredient goes missing and nobody remembers what it was for.

**Current Hypothesis:** Either FORTRAN H requires more memory than we're providing (in which case it's sulking about resource constraints, like a teenager asked to share a bedroom), or the FORTHCLG procedure references datasets that TK4- has filed under "miscellaneous" and subsequently forgotten about.

### Known Issues

#### Intrinsic Function Selection

FORTRAN IV intrinsics were, to put it charitably, a bit of a mess. The generic `MAX` function required the compiler to deduce at compile time whether you meant `MAX0` (integer), `AMAX1` (real), or `DMAX1` (double precision). If it guessed wrong, the runtime library would go looking for a function that simply wasn't there, producing error IHC230I and a profound sense of disappointment.

The solution is to use the specific forms directly: `AMAX1` for real numbers, `MAX0` for integers. The test suite now employs these explicit variants, and harmony has been restored. The 1960s were a simpler time, when type safety was more of a suggestion than a requirement.

#### Subroutine Array Passing (test11_trapz.f)

The FORTRAN IV practice of declaring array arguments as `REAL X(1)` was the standard idiom of the era. It means, essentially, "there's an array here, and I'm not telling you how large it is." The compiler accepted this on faith, the caller and callee simply agreed about what was in memory, and everyone got on with their lives.

No bounds checking. No metadata. Pure trust.

This worked splendidly when everyone followed the rules, and produced spectacular chaos when they didn't. Modern programmers may find this arrangement alarming, but it reflects an era when computers were expensive, programmers were careful, and both were treated with considerably more respect.

## JCL Fixes Applied

During testing, several JCL issues were discovered and remedied:

1. **Job Name Sanitisation** - MVS does not approve of underscores in job names, viewing them with the same disdain the Queen might view someone wearing trainers to a state dinner. Added `sanitize_name()` function.

2. **BLKSIZE Correction** - Changed from 3200 to 400 for object modules. The linkage editor (IEWL) is quite particular about block sizes, like a sommelier who insists the glass matters.

3. **Missing FORTLIB** - Added `//SYSLIB DD DSN=SYS1.FORTLIB,DISP=SHR` to provide the FORTRAN runtime library. Without this, the linker complained about unresolved references to IBCOM#, which sounds like a robot from a 1950s science fiction film but is actually the I/O common block.

## Test Files

All test files reside in `tests/fortran_iv/` and follow authentic FORTRAN IV conventions:

- Fixed-format source (columns 1-6: labels, column 7: continuation, columns 7-72: statements)
- Comments begin with 'C' in column 1
- No IF-THEN-ELSE (logical IF only)
- No CHARACTER type (Hollerith literals where needed)
- Statement numbers for flow control
- Computed GOTO for multi-way branching
- EQUIVALENCE and COMMON for memory management

The code style may appear alarming to modern sensibilities, but it was considered quite elegant in 1966, much like avocado-coloured bathroom suites.

## Recommendations

1. **Continue using FORTRAN G** for compilation until FORTRAN H's temperament improves
2. **Investigate WATFIV** as a fallback - it's more forgiving, like a substitute teacher
3. **Use specific intrinsics** (AMAX1, MAX0, etc.) rather than generic forms (MAX, MIN)
4. **Try larger REGION** for FORTRAN H - perhaps 512K or beyond, if MVS permits such extravagance
5. ~~**Test SLATEC compilation** once the toolchain is stable~~ **DONE!** See below.

---

## SLATEC Library Testing

*In which we discover that 1982's finest mathematical routines run splendidly on 1966's finest compiler*

### The Setup

SLATEC (Sandia, Los Alamos, Air Force - Fortran library) is a comprehensive mathematical library that was quite possibly running nuclear weapons simulations while we were still in nappies. We have now successfully compiled portions of it with the IBM FORTRAN G compiler.

### Machine Constants Infrastructure

Before SLATEC could run, we needed to implement the I1MACH and R1MACH portability layer for IBM 360 hexadecimal floating-point:

```
I1MACH(1)  = 5       Standard input unit
I1MACH(2)  = 6       Standard output unit
I1MACH(10) = 16      Floating-point base (hex, because IBM)
I1MACH(11) = 6       Mantissa digits (in hex)

R1MACH(1) = 5.4E-79  Smallest positive (practically zero)
R1MACH(2) = 7.2E+75  Largest magnitude (quite large)
```

### GAMLN (Log-Gamma Function) Results

| Test Case | Result | Expected | Error | Verdict |
|-----------|--------|----------|-------|---------|
| GAMLN(1.0) | 0.0 | 0.0 | 0 | PASS |
| GAMLN(3.0) | 0.693147 | 0.693147 | 6E-08 | PASS |
| GAMLN(10.0) | 12.801827 | 12.801827 | 0 | PASS |
| GAMLN(0.5) | 0.572359 | 0.572365 | 5.6E-06 | PASS |
| GAMLN(100.0) | 359.134033 | 359.134033 | 0 | PASS |
| GAMLN(-1.0) | -- | IERR=1 | -- | PASS |

**All SLATEC GAMLN tests passed** with precision well within IBM 360 single-precision limits.

### Lessons Learned

1. **FORTRAN G doesn't allow function calls in WRITE statements** - must use temporaries
2. **No Z-prefix hex constants** - use decimal with EQUIVALENCE for bit patterns
3. **The 1982 library code runs unmodified** except for MAX→AMAX1, MIN→AMIN1

See `tests/slatec/SLATEC_TEST_RESULTS.md` for the full report with British commentary.

---

## Conclusion

The FORTRAN 360 toolchain successfully compiles and executes FORTRAN IV programs using authentic 1966 IBM compiler code. With 14 of 15 tests passing, we have demonstrated that numerical algorithms execute correctly through the original toolchain—the same compiler code that processed punch cards in computer rooms across the 1960s is now happily processing our test programs on hardware its designers could not have imagined.

This is either a remarkable achievement in software preservation or an elaborate form of madness, depending on one's perspective. We prefer to think of it as both.

FORTRAN H remains under investigation. We have not ruled out the possibility that it simply doesn't want to be found, but the more likely explanation involves memory constraints and missing datasets—mundane problems that would have been entirely recognisable to a 1969 systems programmer, albeit one who probably had better access to IBM documentation.

---

*Tests conducted on: 30 December 2025*
*Emulation: Hercules 3.07 / TK4- (MVS 3.8j)*
*Tea consumed: Several cups*
