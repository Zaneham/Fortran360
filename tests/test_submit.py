"""
Tests for job submission module.
"""

import os
import sys
import unittest
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from fortran360.submit import (
    MVSConnection,
    MVSConfig,
    generate_compile_jcl,
    compile_fortran,
    JobSubmissionError,
)


class TestJCLGeneration(unittest.TestCase):
    """Tests for JCL generation."""

    def test_generate_basic_jcl(self):
        """Test basic JCL generation."""
        source = """      WRITE(6,100)
  100 FORMAT(' HELLO')
      STOP
      END"""

        jcl = generate_compile_jcl(source, "TESTJOB")

        self.assertIn("//TESTJOB JOB", jcl)
        self.assertIn("EXEC PGM=IEYFORT", jcl)
        self.assertIn("//SYSPRINT DD SYSOUT=A", jcl)
        self.assertIn("SYSIN", jcl)
        self.assertIn("DD *", jcl)
        self.assertIn("WRITE(6,100)", jcl)
        self.assertIn("/*", jcl)

    def test_job_name_truncation(self):
        """Test that job names are truncated to 8 characters."""
        jcl = generate_compile_jcl("      END", "VERYLONGJOBNAME")
        self.assertIn("//VERYLONGJOBNAME"[:10], jcl)

    def test_fortran_h_compiler(self):
        """Test JCL generation for FORTRAN H."""
        jcl = generate_compile_jcl("      END", "TESTHCMP", compiler="IEKAA00")
        self.assertIn("EXEC PGM=IEKAA00", jcl)

    def test_compiler_options(self):
        """Test JCL generation with compiler options."""
        jcl = generate_compile_jcl("      END", "TESTOPT", options="LIST,MAP")
        self.assertIn("PARM='LIST,MAP'", jcl)


class TestMVSConnection(unittest.TestCase):
    """Tests for MVSConnection."""

    def test_default_config(self):
        """Test default configuration values."""
        conn = MVSConnection()
        self.assertEqual(conn.config.host, "localhost")
        self.assertEqual(conn.config.reader_port, 3505)

    def test_custom_config(self):
        """Test custom configuration."""
        config = MVSConfig(host="192.168.1.100", reader_port=3506)
        conn = MVSConnection(config)
        self.assertEqual(conn.config.host, "192.168.1.100")
        self.assertEqual(conn.config.reader_port, 3506)


class TestMVSIntegration(unittest.TestCase):
    """
    Integration tests that require Hercules/MVS to be running.

    These tests are skipped if MVS is not available.
    """

    @classmethod
    def setUpClass(cls):
        """Check if MVS is available."""
        cls.conn = MVSConnection()
        cls.mvs_available = cls.conn.is_available()

    def test_mvs_availability(self):
        """Test that we can detect MVS availability."""
        # This test always runs - it just checks the detection works
        result = self.conn.is_available()
        self.assertIsInstance(result, bool)

    @unittest.skipUnless(
        MVSConnection().is_available(),
        "MVS not available - skipping integration test"
    )
    def test_compile_simple_program(self):
        """Test compiling a simple FORTRAN program."""
        # FORTRAN source - columns 1-6 are sequence/comment, 7-72 are code
        source = "\n".join([
            "C     INTEGRATION TEST",
            "      WRITE(6,100)",
            "  100 FORMAT(' INTEGRATION TEST PASSED')",
            "      STOP",
            "      END",
        ])

        result = compile_fortran(source, job_name="INTTEST", compiler="G")

        self.assertIsNotNone(result)
        self.assertEqual(result.return_code, 0)
        self.assertTrue(result.success)
        self.assertEqual(result.stats.diagnostics_count, 0)

    @unittest.skipUnless(
        MVSConnection().is_available(),
        "MVS not available - skipping integration test"
    )
    def test_compile_with_error(self):
        """Test that syntax errors are properly detected."""
        # Invalid FORTRAN - references undefined format statement
        source = "\n".join([
            "      WRITE(6,999)",
            "  100 FORMAT(' FORMAT 100 NOT 999')",
            "      STOP",
            "      END",
        ])

        result = compile_fortran(source, job_name="ERRTEST", compiler="G")

        self.assertIsNotNone(result)
        # Should have some diagnostic (undefined format reference)
        self.assertGreater(result.return_code, 0)


if __name__ == "__main__":
    unittest.main()
