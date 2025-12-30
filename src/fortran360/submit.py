"""
Job submission to MVS via Hercules card reader socket.

Simulates feeding punch cards into a hopper, except the hopper is a TCP socket
and the cards are ASCII strings. Progress.
"""

import os
import socket
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, List

from .parser import parse_fortran_output, JobResult


@dataclass
class MVSConfig:
    """Configuration for MVS connection. Coordinates for the time machine."""
    host: str = "localhost"
    reader_port: int = 3505
    printer_dir: str = r"C:\Mainframe\prt"
    timeout: int = 60  # seconds to wait for job output


class JobSubmissionError(Exception):
    """Error during job submission. The mainframe is displeased."""
    pass


class MVSConnection:
    """Manages connection to MVS via Hercules. Your portal to 1966."""

    def __init__(self, config: Optional[MVSConfig] = None):
        self.config = config or MVSConfig()

    def is_available(self) -> bool:
        """Check if MVS is available. Is anybody home in 1966?"""
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(2)
            sock.connect((self.config.host, self.config.reader_port))
            sock.close()
            return True
        except (ConnectionRefusedError, socket.timeout, OSError):
            return False

    def submit_jcl(self, jcl: str) -> bool:
        """
        Submit JCL to MVS card reader. Feed the beast.

        Args:
            jcl: JCL text to submit (80 columns or perish)

        Returns:
            True if the mainframe deigned to accept your offering

        Raises:
            JobSubmissionError: The mainframe rejected your cards
        """
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(10)
            sock.connect((self.config.host, self.config.reader_port))

            # Send JCL line by line (80-byte card images)
            for line in jcl.strip().split('\n'):
                # Pad to 80 bytes
                card = line[:80].ljust(80) + '\n'
                sock.send(card.encode('ascii', errors='replace'))

            sock.close()
            return True

        except ConnectionRefusedError:
            raise JobSubmissionError(
                f"Cannot connect to {self.config.host}:{self.config.reader_port}. "
                "Is Hercules/MVS running?"
            )
        except socket.timeout:
            raise JobSubmissionError("Connection timed out")
        except Exception as e:
            raise JobSubmissionError(f"Submission failed: {e}")

    def get_printer_files(self) -> List[Path]:
        """Get list of printer spool files. Check the green-bar paper."""
        printer_dir = Path(self.config.printer_dir)
        if not printer_dir.exists():
            return []

        files = list(printer_dir.glob("*.txt"))
        return sorted(files, key=lambda f: f.stat().st_mtime, reverse=True)

    def wait_for_output(
        self,
        job_name: str,
        pre_size: int = 0,
        pre_file: Optional[Path] = None
    ) -> Optional[str]:
        """
        Wait for new printer output. The 1960s equivalent of watching
        a progress bar, except you're watching a file grow.

        Args:
            job_name: Name of job to look for
            pre_size: Size of printer file before submission
            pre_file: Path to printer file to monitor

        Returns:
            New output content or None if the mainframe is ignoring you
        """
        start_time = time.time()

        while time.time() - start_time < self.config.timeout:
            time.sleep(0.5)

            # Check for new output
            files = self.get_printer_files()
            if not files:
                continue

            # Check the most recent file
            latest = files[0]
            current_size = latest.stat().st_size

            if current_size > pre_size:
                # New output - read only the new portion
                with open(latest, 'r', encoding='utf-8', errors='replace') as f:
                    f.seek(pre_size)
                    new_content = f.read()

                # Check if our job is in the NEW output
                if job_name in new_content:
                    # Extract just this job's output
                    return self._extract_job_output(new_content, job_name)

        return None

    def _extract_job_output(self, content: str, job_name: str) -> str:
        """
        Extract a single job's output from printer spool content.
        Like finding your printout in a stack of green-bar paper.

        Args:
            content: Raw printer spool content (the whole stack)
            job_name: Name of job to extract (your handwriting on the corner)

        Returns:
            Just your bit of the printout
        """
        lines = content.split('\n')
        job_output = []
        in_job = False

        for line in lines:
            # Job start marker
            if f"START  JOB" in line and job_name in line:
                in_job = True
                job_output = [line]
            elif in_job:
                job_output.append(line)
                # Job end marker
                if f"END   JOB" in line and job_name in line:
                    break

        return '\n'.join(job_output)


def generate_compile_jcl(
    source_code: str,
    job_name: str = "FORTCOMP",
    compiler: str = "IEYFORT",
    options: Optional[str] = None
) -> str:
    """
    Generate JCL for FORTRAN compilation. The incantation that makes
    the mainframe actually do something useful.

    Args:
        source_code: FORTRAN source code (columns 1-72 matter)
        job_name: JCL job name (max 8 chars, like a vanity plate)
        compiler: Compiler program (IEYFORT=G, IEKAA00=H)
        options: Compiler options (PARM string of doom)

    Returns:
        Complete JCL, ready for the card reader
    """
    job_name = job_name[:8].upper()

    parm_line = f"//FORT     EXEC PGM={compiler},REGION=100K"
    if options:
        parm_line = f"//FORT     EXEC PGM={compiler},PARM='{options}',REGION=100K"

    jcl = f"""//{job_name} JOB (1),'FORTRAN COMPILE',CLASS=A,MSGCLASS=A,
//         MSGLEVEL=(1,1)
//*
//* FORTRAN COMPILATION - GENERATED BY FORTRAN360
//*
{parm_line}
//SYSPRINT DD SYSOUT=A
//SYSPUNCH DD SYSOUT=B
//SYSLIN   DD SYSOUT=B
//SYSIN    DD *
{source_code}
/*
//
"""
    return jcl


def compile_fortran(
    source_code: str,
    job_name: str = "FORTCOMP",
    compiler: str = "G",
    config: Optional[MVSConfig] = None
) -> JobResult:
    """
    Compile FORTRAN source code using authentic IBM compiler.
    The main event. The reason we're all here.

    Args:
        source_code: FORTRAN source code
        job_name: JCL job name
        compiler: "G" for FORTRAN G (1966), "H" for FORTRAN H (1969)
        config: MVS connection configuration

    Returns:
        JobResult with what the mainframe thought of your code

    Raises:
        JobSubmissionError: The mainframe has opinions about your code
    """
    conn = MVSConnection(config)

    if not conn.is_available():
        raise JobSubmissionError(
            "MVS is not available. Start Hercules/TK4- first."
        )

    # Select compiler
    compiler_pgm = {
        "G": "IEYFORT",   # FORTRAN G (1966)
        "H": "IEKAA00",   # FORTRAN H (1969)
    }.get(compiler.upper(), "IEYFORT")

    # Generate JCL
    jcl = generate_compile_jcl(source_code, job_name, compiler_pgm)

    # Record pre-submission state
    files = conn.get_printer_files()
    pre_size = files[0].stat().st_size if files else 0

    # Submit job
    conn.submit_jcl(jcl)

    # Wait for output
    output = conn.wait_for_output(job_name, pre_size)

    if not output:
        raise JobSubmissionError(
            f"Timeout waiting for job {job_name} output"
        )

    # Parse results
    result = parse_fortran_output(output)

    if not result:
        raise JobSubmissionError(
            f"Could not parse output for job {job_name}"
        )

    return result
