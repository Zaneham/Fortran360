"""
Output parser for IBM FORTRAN G/H compiler output.

Extracts structured data from JES2 printer spool files.
"""

import re
from dataclasses import dataclass, field
from typing import List, Optional
from enum import Enum


class Severity(Enum):
    """Diagnostic severity levels."""
    INFO = 0
    WARNING = 4
    ERROR = 8
    SEVERE = 12
    TERMINAL = 16


@dataclass
class Diagnostic:
    """A single compiler diagnostic message."""
    line_number: int
    code: str  # e.g., "IEY013I"
    severity: Severity
    message: str
    source_line: Optional[str] = None


@dataclass
class CompileStats:
    """Compiler statistics."""
    source_statements: int = 0
    program_size: int = 0
    diagnostics_count: int = 0
    highest_severity: int = 0


@dataclass
class JobResult:
    """Complete result of a compilation job."""
    job_name: str
    job_number: int
    step_name: str
    program: str
    return_code: int
    condition_code: int
    compiler_version: str
    compile_date: str
    compile_time: str
    stats: CompileStats
    diagnostics: List[Diagnostic] = field(default_factory=list)
    source_listing: List[str] = field(default_factory=list)
    raw_output: str = ""

    @property
    def success(self) -> bool:
        """True if compilation succeeded (RC <= 4)."""
        return self.return_code <= 4

    @property
    def has_warnings(self) -> bool:
        """True if there were warning-level diagnostics."""
        return any(d.severity == Severity.WARNING for d in self.diagnostics)

    @property
    def has_errors(self) -> bool:
        """True if there were error-level diagnostics."""
        return any(d.severity.value >= Severity.ERROR.value for d in self.diagnostics)


class FortranOutputParser:
    """Parser for FORTRAN G/H compiler output from JES2 spool."""

    # Regex patterns for parsing
    RE_JOB_HEADER = re.compile(
        r'\*\*\*\*A\s+START\s+JOB\s+(\d+)\s+(\w+)'
    )
    RE_COMPILER_HEADER = re.compile(
        r'FORTRAN IV ([GH])\s+LEVEL\s+(\d+)\s+(\w+)\s+DATE\s*=\s*(\d+)\s+(\d+/\d+/\d+)'
    )
    RE_STEP_RESULT = re.compile(
        r'(\w+)\s+(\w+)\s+(\w+)\s+RC=\s*(\d+)'
    )
    RE_COND_CODE = re.compile(
        r'STEP WAS EXECUTED - COND CODE (\d+)'
    )
    RE_STATISTICS = re.compile(
        r'\*STATISTICS\*\s+SOURCE STATEMENTS\s*=\s*(\d+),\s*PROGRAM SIZE\s*=\s*(\d+)'
    )
    RE_DIAGNOSTICS_SUMMARY = re.compile(
        r'\*STATISTICS\*\s+(\d+)\s+DIAGNOSTICS GENERATED,\s+HIGHEST SEVERITY CODE IS\s+(\d+)'
    )
    RE_NO_DIAGNOSTICS = re.compile(
        r'\*STATISTICS\*\s+NO DIAGNOSTICS GENERATED'
    )
    RE_DIAGNOSTIC_LINE = re.compile(
        r'\*{5,}(\d+)\)\s+(\w+)\s+(\w+)'
    )
    RE_SOURCE_LINE = re.compile(
        r'^\s{0,3}(\d{4})\s{6,}(.+)$'
    )
    RE_JES2_STATS = re.compile(
        r'(\d+)\s+(CARDS READ|SYSOUT PRINT RECORDS|SYSOUT PUNCH RECORDS)'
    )

    def parse(self, output: str) -> Optional[JobResult]:
        """Parse compiler output and return structured result."""
        # Find the FORTRAN job section
        job_match = self.RE_JOB_HEADER.search(output)
        if not job_match:
            return None

        job_number = int(job_match.group(1))
        job_name = job_match.group(2)

        # Find compiler header
        compiler_match = self.RE_COMPILER_HEADER.search(output)
        if compiler_match:
            compiler_level = compiler_match.group(1)
            compiler_version = f"FORTRAN IV {compiler_level} LEVEL {compiler_match.group(2)}"
            compile_date = compiler_match.group(4)
            compile_time = compiler_match.group(5)
        else:
            compiler_version = "FORTRAN IV G"
            compile_date = ""
            compile_time = ""

        # Find step result
        step_match = self.RE_STEP_RESULT.search(output)
        if step_match:
            step_name = step_match.group(2)
            program = step_match.group(3)
            return_code = int(step_match.group(4))
        else:
            step_name = "FORT"
            program = "IEYFORT"
            return_code = -1

        # Find condition code
        cond_match = self.RE_COND_CODE.search(output)
        condition_code = int(cond_match.group(1)) if cond_match else return_code

        # Parse statistics
        stats = CompileStats()
        stats_match = self.RE_STATISTICS.search(output)
        if stats_match:
            stats.source_statements = int(stats_match.group(1))
            stats.program_size = int(stats_match.group(2))

        # Parse diagnostics summary
        diag_match = self.RE_DIAGNOSTICS_SUMMARY.search(output)
        if diag_match:
            stats.diagnostics_count = int(diag_match.group(1))
            stats.highest_severity = int(diag_match.group(2))
        elif self.RE_NO_DIAGNOSTICS.search(output):
            stats.diagnostics_count = 0
            stats.highest_severity = 0

        # Parse individual diagnostics
        diagnostics = self._parse_diagnostics(output)

        # Parse source listing
        source_listing = self._parse_source_listing(output)

        return JobResult(
            job_name=job_name,
            job_number=job_number,
            step_name=step_name,
            program=program,
            return_code=return_code,
            condition_code=condition_code,
            compiler_version=compiler_version,
            compile_date=compile_date,
            compile_time=compile_time,
            stats=stats,
            diagnostics=diagnostics,
            source_listing=source_listing,
            raw_output=output
        )

    def _parse_diagnostics(self, output: str) -> List[Diagnostic]:
        """Extract individual diagnostic messages."""
        diagnostics = []
        lines = output.split('\n')

        for i, line in enumerate(lines):
            # Look for diagnostic markers like "***********01)  IEY013I SYNTAX"
            match = self.RE_DIAGNOSTIC_LINE.search(line)
            if match:
                diag_num = int(match.group(1))
                code = match.group(2)
                msg_type = match.group(3)

                # Determine severity from return code suffix
                # IEYxxxI = Informational, IEYxxxW = Warning, IEYxxxE = Error, etc.
                if code.endswith('I'):
                    severity = Severity.WARNING  # FORTRAN G uses 'I' for warnings too
                elif code.endswith('W'):
                    severity = Severity.WARNING
                elif code.endswith('E'):
                    severity = Severity.ERROR
                elif code.endswith('S'):
                    severity = Severity.SEVERE
                elif code.endswith('T'):
                    severity = Severity.TERMINAL
                else:
                    severity = Severity.ERROR

                # Try to find the source line number from preceding lines
                source_line_num = 0
                source_text = ""
                for j in range(max(0, i-3), i):
                    src_match = self.RE_SOURCE_LINE.match(lines[j])
                    if src_match:
                        source_line_num = int(src_match.group(1))
                        source_text = src_match.group(2).strip()

                diagnostics.append(Diagnostic(
                    line_number=source_line_num,
                    code=code,
                    severity=severity,
                    message=msg_type,
                    source_line=source_text if source_text else None
                ))

        return diagnostics

    def _parse_source_listing(self, output: str) -> List[str]:
        """Extract the source listing section."""
        source_lines = []
        in_source = False

        for line in output.split('\n'):
            # Start of source listing
            if 'FORTRAN IV' in line and 'LEVEL' in line and 'PAGE' in line:
                in_source = True
                continue

            # End markers
            if '*OPTIONS IN EFFECT*' in line or '*STATISTICS*' in line:
                in_source = False
                continue

            if in_source:
                match = self.RE_SOURCE_LINE.match(line)
                if match:
                    source_lines.append(f"{match.group(1)}: {match.group(2)}")

        return source_lines


def parse_fortran_output(output: str) -> Optional[JobResult]:
    """Convenience function to parse FORTRAN compiler output."""
    parser = FortranOutputParser()
    return parser.parse(output)
