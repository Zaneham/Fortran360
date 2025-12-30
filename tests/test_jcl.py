"""
Tests for JCL generation module.
"""

import sys
import unittest
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

from fortran360.jcl import (
    Compiler,
    CompileOptions,
    LinkOptions,
    ExecuteOptions,
    generate_compile_only_jcl,
    generate_compile_link_jcl,
    generate_compile_link_go_jcl,
)
from fortran360.submit import MVSConnection, MVSConfig, JobSubmissionError
from fortran360.parser import parse_fortran_output


class TestCompileOnlyJCL(unittest.TestCase):
    """Tests for compile-only JCL generation."""

    def test_default_options(self):
        """Test JCL with default options."""
        source = "      END"
        jcl = generate_compile_only_jcl(source, "TESTJOB")

        self.assertIn("//TESTJOB JOB", jcl)
        self.assertIn("PGM=IEYFORT", jcl)  # Default is FORTRAN G
        self.assertIn("SOURCE", jcl)
        self.assertIn("NODECK", jcl)
        self.assertIn("LOAD", jcl)

    def test_fortran_h_compiler(self):
        """Test JCL with FORTRAN H compiler."""
        options = CompileOptions(compiler=Compiler.H)
        jcl = generate_compile_only_jcl("      END", "TESTJOB", options)

        self.assertIn("PGM=IEKAA00", jcl)

    def test_deck_option(self):
        """Test JCL with object deck output."""
        options = CompileOptions(object_deck=True)
        jcl = generate_compile_only_jcl("      END", "TESTJOB", options)

        self.assertIn("DECK", jcl)
        self.assertNotIn("NODECK", jcl)


class TestCompileLinkJCL(unittest.TestCase):
    """Tests for compile+link JCL generation."""

    def test_basic_clg(self):
        """Test basic compile+link JCL."""
        source = "      END"
        jcl = generate_compile_link_jcl(source, "CLTEST", "MYPROG")

        # Should have FORT step
        self.assertIn("//FORT", jcl)
        self.assertIn("PGM=IEYFORT", jcl)

        # Should have LKED step
        self.assertIn("//LKED", jcl)
        self.assertIn("PGM=IEWL", jcl)

        # Should pass object module between steps
        self.assertIn("&&OBJMOD", jcl)

        # Should create load module
        self.assertIn("&&GOSET(MYPROG)", jcl)

    def test_conditional_execution(self):
        """Test that link step has condition code check."""
        jcl = generate_compile_link_jcl("      END", "CLTEST")

        # Link should only run if compile didn't fail badly
        self.assertIn("COND=(8,LT,FORT)", jcl)


class TestCompileLinkGoJCL(unittest.TestCase):
    """Tests for compile+link+go JCL generation."""

    def test_basic_clg(self):
        """Test basic compile+link+go JCL."""
        source = "      END"
        jcl = generate_compile_link_go_jcl(source, "CLGTEST")

        # Should have all three steps
        self.assertIn("//FORT", jcl)
        self.assertIn("//LKED", jcl)
        self.assertIn("//GO", jcl)

        # GO step should reference linked module
        self.assertIn("PGM=*.LKED.SYSLMOD", jcl)

    def test_with_input_data(self):
        """Test CLG with input data for program."""
        options = ExecuteOptions(input_data="123\n456\n789")
        jcl = generate_compile_link_go_jcl(
            "      END", "CLGTEST",
            execute_options=options
        )

        self.assertIn("//SYSIN    DD *", jcl)
        self.assertIn("123", jcl)
        self.assertIn("456", jcl)

    def test_execution_options(self):
        """Test CLG with execution options."""
        options = ExecuteOptions(region="512K", time=10)
        jcl = generate_compile_link_go_jcl(
            "      END", "CLGTEST",
            execute_options=options
        )

        self.assertIn("REGION=512K", jcl)
        self.assertIn("TIME=10", jcl)


class TestCLGIntegration(unittest.TestCase):
    """
    Integration tests for CLG that require Hercules/MVS.
    """

    @classmethod
    def setUpClass(cls):
        cls.conn = MVSConnection()
        cls.mvs_available = cls.conn.is_available()

    @unittest.skipUnless(
        MVSConnection().is_available(),
        "MVS not available - skipping integration test"
    )
    def test_compile_link_go_hello_world(self):
        """Test full CLG with a Hello World program."""
        source = "\n".join([
            "C     HELLO WORLD - CLG TEST",
            "      WRITE(6,100)",
            "  100 FORMAT(' HELLO FROM FORTRAN 360!')",
            "      STOP",
            "      END",
        ])

        jcl = generate_compile_link_go_jcl(source, "CLGTEST", "HELLO")

        # Submit the job
        files = self.conn.get_printer_files()
        pre_size = files[0].stat().st_size if files else 0

        self.conn.submit_jcl(jcl)

        # Wait for output
        output = self.conn.wait_for_output("CLGTEST", pre_size)

        self.assertIsNotNone(output, "Job output not received")

        # Check for successful compilation
        self.assertIn("FORTRAN IV", output)

        # Check for link step
        self.assertIn("IEWL", output)

        # Check for program output
        self.assertIn("HELLO FROM FORTRAN 360!", output)

    @unittest.skipUnless(
        MVSConnection().is_available(),
        "MVS not available - skipping integration test"
    )
    def test_compile_link_go_with_calculation(self):
        """Test CLG with a program that does calculation."""
        source = "\n".join([
            "C     CALCULATION TEST",
            "      INTEGER I, TOTAL",
            "      TOTAL = 0",
            "      DO 10 I = 1, 10",
            "         TOTAL = TOTAL + I",
            "   10 CONTINUE",
            "      WRITE(6,100) TOTAL",
            "  100 FORMAT(' SUM 1-10 = ', I5)",
            "      STOP",
            "      END",
        ])

        jcl = generate_compile_link_go_jcl(source, "CALCTEST", "CALC")

        files = self.conn.get_printer_files()
        pre_size = files[0].stat().st_size if files else 0

        self.conn.submit_jcl(jcl)
        output = self.conn.wait_for_output("CALCTEST", pre_size)

        self.assertIsNotNone(output, "Job output not received")

        # Sum of 1-10 = 55
        self.assertIn("55", output)


if __name__ == "__main__":
    unittest.main()
