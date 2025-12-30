"""
Tests for FORTRAN output parser.
"""

import os
import sys
import unittest
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from fortran360.parser import (
    FortranOutputParser,
    parse_fortran_output,
    JobResult,
    Severity,
)


FIXTURES_DIR = Path(__file__).parent / "fixtures"


class TestFortranOutputParser(unittest.TestCase):
    """Tests for FortranOutputParser."""

    def setUp(self):
        self.parser = FortranOutputParser()

    def load_fixture(self, name: str) -> str:
        """Load a test fixture file."""
        with open(FIXTURES_DIR / name, "r", encoding="utf-8", errors="replace") as f:
            return f.read()

    def test_parse_successful_compile(self):
        """Test parsing a clean compile with no errors."""
        output = self.load_fixture("fortran_g_success.txt")
        result = self.parser.parse(output)

        self.assertIsNotNone(result)
        self.assertEqual(result.job_name, "FORTGCMP")
        self.assertEqual(result.job_number, 2)
        self.assertEqual(result.return_code, 0)
        self.assertEqual(result.condition_code, 0)
        self.assertTrue(result.success)
        self.assertFalse(result.has_errors)
        self.assertFalse(result.has_warnings)

        # Check statistics
        self.assertEqual(result.stats.source_statements, 4)
        self.assertEqual(result.stats.program_size, 306)
        self.assertEqual(result.stats.diagnostics_count, 0)

        # Check compiler version
        self.assertIn("FORTRAN IV G", result.compiler_version)
        self.assertIn("LEVEL 21", result.compiler_version)

    def test_parse_compile_with_warning(self):
        """Test parsing a compile with warnings (RC=8)."""
        output = self.load_fixture("fortran_g_warning.txt")
        result = self.parser.parse(output)

        self.assertIsNotNone(result)
        self.assertEqual(result.job_name, "FORTGCMP")
        self.assertEqual(result.job_number, 1)
        self.assertEqual(result.return_code, 8)
        self.assertEqual(result.condition_code, 8)
        self.assertFalse(result.success)  # RC=8 is not success

        # Check statistics
        self.assertEqual(result.stats.source_statements, 5)
        self.assertEqual(result.stats.program_size, 306)
        self.assertEqual(result.stats.diagnostics_count, 1)
        self.assertEqual(result.stats.highest_severity, 8)

        # Check diagnostics
        self.assertEqual(len(result.diagnostics), 1)
        diag = result.diagnostics[0]
        self.assertEqual(diag.code, "IEY013I")
        self.assertEqual(diag.message, "SYNTAX")

    def test_parse_returns_none_for_non_fortran(self):
        """Test that parsing non-FORTRAN output returns None."""
        output = "This is not FORTRAN compiler output"
        result = self.parser.parse(output)
        self.assertIsNone(result)

    def test_convenience_function(self):
        """Test the parse_fortran_output convenience function."""
        output = self.load_fixture("fortran_g_success.txt")
        result = parse_fortran_output(output)

        self.assertIsNotNone(result)
        self.assertEqual(result.job_name, "FORTGCMP")

    def test_source_listing_extraction(self):
        """Test that source listing is extracted."""
        output = self.load_fixture("fortran_g_success.txt")
        result = self.parser.parse(output)

        self.assertIsNotNone(result)
        # Should have extracted some source lines
        self.assertGreater(len(result.source_listing), 0)


class TestJobResult(unittest.TestCase):
    """Tests for JobResult dataclass."""

    def test_success_property(self):
        """Test the success property."""
        from fortran360.parser import CompileStats

        # RC=0 is success
        result = JobResult(
            job_name="TEST", job_number=1, step_name="FORT",
            program="IEYFORT", return_code=0, condition_code=0,
            compiler_version="", compile_date="", compile_time="",
            stats=CompileStats()
        )
        self.assertTrue(result.success)

        # RC=4 is success (warnings only)
        result.return_code = 4
        self.assertTrue(result.success)

        # RC=8 is failure
        result.return_code = 8
        self.assertFalse(result.success)

        # RC=12 is failure
        result.return_code = 12
        self.assertFalse(result.success)


if __name__ == "__main__":
    unittest.main()
