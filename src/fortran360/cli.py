"""
Command-line interface for FORTRAN 360.

Talks to a 1960s mainframe so you don't have to learn JCL.
You're welcome.

Usage:
    fortran360 compile hello.f           # Compile only
    fortran360 run hello.f               # Compile, link, and execute
    fortran360 run hello.f --era=G       # Use FORTRAN G (1966)
    fortran360 run hello.f --era=H       # Use FORTRAN H (1969)
"""

import argparse
import sys
from pathlib import Path
from typing import Optional

from .jcl import (
    Compiler,
    CompileOptions,
    ExecuteOptions,
    generate_compile_only_jcl,
    generate_compile_link_go_jcl,
)
from .submit import MVSConnection, MVSConfig, JobSubmissionError
from .parser import parse_fortran_output, JobResult


def get_compiler(era: str) -> Compiler:
    """Get compiler from era string. Time travel, basically."""
    era_map = {
        "G": Compiler.G,
        "1966": Compiler.G,
        "H": Compiler.H,
        "1969": Compiler.H,
    }
    return era_map.get(era.upper(), Compiler.G)


def format_result(result: JobResult, verbose: bool = False) -> str:
    """Format compilation result for display."""
    lines = []

    # Status line
    if result.success:
        status = "SUCCESS"
    elif result.has_errors:
        status = "FAILED"
    else:
        status = "WARNINGS"

    lines.append(f"[{status}] {result.job_name} - RC={result.return_code}")
    lines.append(f"Compiler: {result.compiler_version}")

    # Statistics
    if result.stats.source_statements > 0:
        lines.append(f"Source statements: {result.stats.source_statements}")
        lines.append(f"Program size: {result.stats.program_size} bytes")

    # Diagnostics
    if result.diagnostics:
        lines.append("")
        lines.append("Diagnostics:")
        for diag in result.diagnostics:
            loc = f"line {diag.line_number}" if diag.line_number else ""
            lines.append(f"  {diag.code} {diag.message} {loc}")
            if diag.source_line and verbose:
                lines.append(f"    > {diag.source_line}")

    return "\n".join(lines)


def cmd_compile(args) -> int:
    """Handle compile command. Sends your code back to 1966."""
    # Read source file
    source_path = Path(args.source)
    if not source_path.exists():
        print(f"Error: File not found: {source_path}", file=sys.stderr)
        return 1

    source_code = source_path.read_text()

    # Check MVS availability
    conn = MVSConnection()
    if not conn.is_available():
        print("Error: MVS not available. Start Hercules/TK4- first.", file=sys.stderr)
        return 1

    # Generate job name from filename
    job_name = source_path.stem[:8].upper()

    # Set up compiler options
    compiler = get_compiler(args.era)
    options = CompileOptions(compiler=compiler)

    # Generate and submit JCL
    jcl = generate_compile_only_jcl(source_code, job_name, options)

    if args.verbose:
        print("Submitting JCL:")
        print("-" * 40)
        print(jcl[:500])
        print("-" * 40)

    try:
        files = conn.get_printer_files()
        pre_size = files[0].stat().st_size if files else 0

        conn.submit_jcl(jcl)
        print(f"Submitted {job_name}...")

        output = conn.wait_for_output(job_name, pre_size)

        if not output:
            print("Error: Timeout waiting for job output", file=sys.stderr)
            return 1

        result = parse_fortran_output(output)
        if result:
            print(format_result(result, args.verbose))
            return 0 if result.success else 1
        else:
            print("Error: Could not parse compiler output", file=sys.stderr)
            return 1

    except JobSubmissionError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


def cmd_run(args) -> int:
    """Handle run command (compile + link + go). The full mainframe experience."""
    # Read source file
    source_path = Path(args.source)
    if not source_path.exists():
        print(f"Error: File not found: {source_path}", file=sys.stderr)
        return 1

    source_code = source_path.read_text()

    # Check MVS availability
    conn = MVSConnection()
    if not conn.is_available():
        print("Error: MVS not available. Start Hercules/TK4- first.", file=sys.stderr)
        return 1

    # Generate names
    job_name = source_path.stem[:8].upper()
    load_name = job_name

    # Set up options
    compiler = get_compiler(args.era)
    compile_options = CompileOptions(compiler=compiler)
    execute_options = ExecuteOptions()

    # Read input file if provided
    if args.input:
        input_path = Path(args.input)
        if input_path.exists():
            execute_options.input_data = input_path.read_text()

    # Generate CLG JCL
    jcl = generate_compile_link_go_jcl(
        source_code, job_name, load_name,
        compile_options=compile_options,
        execute_options=execute_options
    )

    if args.verbose:
        print("Submitting JCL:")
        print("-" * 40)
        print(jcl[:800])
        print("-" * 40)

    try:
        files = conn.get_printer_files()
        pre_size = files[0].stat().st_size if files else 0

        conn.submit_jcl(jcl)
        print(f"Submitted {job_name} (compile, link, go)...")

        output = conn.wait_for_output(job_name, pre_size)

        if not output:
            print("Error: Timeout waiting for job output", file=sys.stderr)
            return 1

        # Print the output
        print("\n" + "=" * 60)
        print("PROGRAM OUTPUT:")
        print("=" * 60)

        # Extract program output (from GO step)
        in_go_output = False
        for line in output.split('\n'):
            if 'GO       EXEC' in line or 'FT06F001' in line:
                in_go_output = True
            elif in_go_output:
                # Skip JES2 messages
                if line.strip() and not line.startswith('IEF') and not line.startswith('$HASP'):
                    # Check if it's actual program output
                    if not any(x in line for x in ['****A', 'IEF', '$HASP', 'COND CODE']):
                        print(line)
                if 'END   JOB' in line:
                    break

        print("=" * 60)

        result = parse_fortran_output(output)
        if result:
            print()
            print(format_result(result, args.verbose))
            return 0 if result.success else 1

        return 0

    except JobSubmissionError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


def cmd_status(args) -> int:
    """Check MVS status. Is the mainframe awake?"""
    conn = MVSConnection()

    if conn.is_available():
        print("MVS Status: AVAILABLE")
        print(f"  Host: {conn.config.host}")
        print(f"  Card Reader Port: {conn.config.reader_port}")
        print(f"  Printer Directory: {conn.config.printer_dir}")
        return 0
    else:
        print("MVS Status: NOT AVAILABLE")
        print("  Start Hercules/TK4- to enable FORTRAN compilation")
        return 1


def main():
    """Main CLI entry point."""
    parser = argparse.ArgumentParser(
        prog="fortran360",
        description="Compile FORTRAN using authentic IBM System/360 compilers (1966-1969)"
    )
    parser.add_argument(
        "--version", action="version",
        version="%(prog)s 0.1.0"
    )

    subparsers = parser.add_subparsers(dest="command", help="Commands")

    # compile command
    compile_parser = subparsers.add_parser(
        "compile", help="Compile FORTRAN source (no link)"
    )
    compile_parser.add_argument("source", help="FORTRAN source file")
    compile_parser.add_argument(
        "--era", default="G",
        choices=["G", "H", "1966", "1969"],
        help="Compiler era: G=1966, H=1969 (default: G)"
    )
    compile_parser.add_argument(
        "-v", "--verbose", action="store_true",
        help="Verbose output"
    )
    compile_parser.set_defaults(func=cmd_compile)

    # run command
    run_parser = subparsers.add_parser(
        "run", help="Compile, link, and execute FORTRAN program"
    )
    run_parser.add_argument("source", help="FORTRAN source file")
    run_parser.add_argument(
        "--era", default="G",
        choices=["G", "H", "1966", "1969"],
        help="Compiler era: G=1966, H=1969 (default: G)"
    )
    run_parser.add_argument(
        "--input", "-i",
        help="Input file for program"
    )
    run_parser.add_argument(
        "-v", "--verbose", action="store_true",
        help="Verbose output"
    )
    run_parser.set_defaults(func=cmd_run)

    # status command
    status_parser = subparsers.add_parser(
        "status", help="Check MVS availability"
    )
    status_parser.set_defaults(func=cmd_status)

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        return 1

    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
