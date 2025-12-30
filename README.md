# FORTRAN 360

*Compile and run FORTRAN programs using authentic IBM System/360 compilers (1966-1969), discovered on a bloke's CD-ROM and now running on hardware that would have cost less than a single day's rental of the original machine.*

## What Is This, Then?

This is a Python wrapper that lets you compile FORTRAN IV programs using the actual IBM compilers from the 1960s. Not a recreation. Not a reimplementation. The genuine article—the same bits that ran on machines the size of refrigerators, now executing on your laptop whilst it pretends to be a mainframe.

The compilers were preserved on a CD-ROM image from the [MVS 3.8j Turnkey system](http://wotho.ethz.ch/tk4-/), lovingly maintained by people who understand that some software is too historically significant to let die. We found them there, blew off the digital dust, and put them back to work.

She'll be right.

## Features

- **Authentic IBM compilers** running on MVS 3.8j via Hercules emulation
- **FORTRAN G** (1966) - The basic compiler, agreeable and cooperative
- **FORTRAN H** (1969) - The optimising compiler, temperamental and demanding
- **Full CLG pipeline** - Compile, Link, and Go, just like the old days
- **Structured output parsing** - Because reading JES2 output shouldn't require a degree in archaeology

## Requirements

- Python 3.8+
- TK4- (Turnkey MVS 3.8j) - Hercules mainframe emulation
- Windows (tested), Linux/macOS should work
- A cup of tea (optional but recommended)
- High tolerance for vintage computing quirks

## Installation

```bash
cd C:\dev\fortran360
pip install -e .
```

## Quick Start

1. Start Hercules/MVS (allow a moment for the virtual mainframe to warm up):
```bash
cd C:\Mainframe
echo DAEMON > unattended/mode
./mvs.bat
```

2. Check that MVS is running and accepting visitors:
```bash
python -m src.fortran360.cli status
```

3. Compile and run a program:
```bash
python -m src.fortran360.cli run examples/hello.f
```

If it works, you've just run code through the same compiler that processed programs for banks, airlines, and scientific institutions in the 1960s. If it doesn't work, check that Hercules is running—the mainframe won't IPL itself.

## Usage

### Compile Only
```bash
python -m src.fortran360.cli compile myprogram.f
python -m src.fortran360.cli compile myprogram.f --era=H  # Use FORTRAN H (if it's in the mood)
```

### Compile, Link, and Execute
```bash
python -m src.fortran360.cli run myprogram.f
python -m src.fortran360.cli run myprogram.f --era=1966  # FORTRAN G
python -m src.fortran360.cli run myprogram.f --era=1969  # FORTRAN H
```

### Check MVS Status
```bash
python -m src.fortran360.cli status
```

## The Compilers

| Era | Compiler | Program | Disposition |
|-----|----------|---------|-------------|
| 1966 | FORTRAN G | IEYFORT | Reliable, cooperative, like a good sheepdog |
| 1969 | FORTRAN H | IEKAA00 | Optimising, temperamental, requires patience |

FORTRAN G will compile nearly anything you throw at it and produce sensible object code. FORTRAN H will produce *better* object code but occasionally decides it would rather not compile anything at all, thank you very much. We're still working on understanding its needs.

## Project Structure

```
fortran360/
├── src/fortran360/
│   ├── __init__.py      # Package init
│   ├── parser.py        # Output parser (interprets mainframe hieroglyphics)
│   ├── submit.py        # Job submission (talks to the card reader)
│   ├── jcl.py           # JCL generation (the dark arts)
│   └── cli.py           # Command-line interface
├── tests/
│   ├── fixtures/        # Test data
│   ├── fortran_iv/      # FORTRAN IV test programs
│   ├── test_parser.py   # Parser tests
│   ├── test_submit.py   # Submission tests
│   └── test_jcl.py      # JCL/CLG tests
└── examples/
    ├── hello.f          # Hello World (1966 edition)
    └── sum.f            # Sum calculation
```

## Running Tests

```bash
cd C:\dev\fortran360
python -m pytest tests/ -v
```

Note: Integration tests require Hercules/MVS to be running. The mainframe doesn't emulate itself.

## Current Status

- 14 of 15 FORTRAN IV test cases passing on FORTRAN G
- FORTRAN H under investigation (memory requirements, catalogued procedure issues)
- Numerical algorithms (Newton-Raphson, matrix operations, statistics) execute correctly
- The toolchain has been validated for SLATEC library work

See `tests/fortran_iv/TEST_RESULTS.md` for the full report, complete with appropriate commentary.

## Next Steps

- [ ] Sort out FORTRAN H's attitude problem (likely needs more REGION)
- [ ] WATFIV compiler support as a fallback
- [ ] `--diff` mode for comparing compiler outputs
- [ ] SLATEC library compilation tests

## Historical Note

These compilers first ran on IBM System/360 hardware in the 1960s—machines that cost millions of dollars, filled entire rooms, and required dedicated staff in white coats. The same compiler code is now executing on consumer hardware via Hercules emulation, producing identical object code to what ran on the original iron.

FORTRAN G (IEYFORT) was released with OS/360 in 1966. FORTRAN H (IEKAA00) followed in 1969 with loop optimisation and improved register allocation. Both compilers remain capable of compiling valid FORTRAN IV code today, which is more than can be said for most software written last year.

The fact that we can still run these compilers—that the bits survived on tapes, then disk images, then CD-ROMs, then downloads—is a testament to the people who understood that preserving computing history matters. The original IBM engineers wrote software that outlasted the hardware, the company's mainframe dominance, and several generations of programmers. Not bad for a bunch of code written before the Moon landing.

## Acknowledgements

This project stands on the shoulders of giants, or at least people with very large tape collections:

- **The MVS 3.8j / TK4- Community** - For preserving and maintaining a working MVS system that anyone can run. Without their decades of work, these compilers would be nothing but documentation.
- **Jürgen Winkelmann** - Creator of TK4-, who assembled the Turnkey system that makes all this possible.
- **Volker Bandke** - Creator of the original TK3 system.
- **The Hercules Developers** - For building a System/370 emulator that actually works.
- **The COBOL Preservation Folks** - Our sister group in the vintage compiler preservation effort. FORTRAN and COBOL have been siblings since the 1960s, occasionally squabbling over who gets more memory but ultimately working together when it matters.
- **Whoever Made That CD-ROM** - Seriously, the fact that production IBM compiler code from 1966 survived on someone's CD-ROM image and is now running on modern hardware is either a miracle of digital preservation or proof that mainframe people never throw anything away. Either way, cheers.

## A Final Thought

Somewhere in the world, there is probably still a System/360 running production code. The compilers in this repository would work on that machine, just as they work here. That's 60 years of compatibility—a span of time that has seen the rise and fall of countless "revolutionary" technologies.

FORTRAN was designed to be compiled, and these compilers were designed to compile it. Everything else is just implementation details.

---

*"The determined Real Programmer can write FORTRAN programs in any language."*
— Ed Post, 1983

*Built with tea, spite, and an unreasonable fondness for fixed-format source code.*
