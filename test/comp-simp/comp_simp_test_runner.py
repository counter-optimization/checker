import unittest
import csv
import sys
import typing
from typing import Callable, List, Any
from pathlib import Path

import test_common
import checker

class CompSimpTestCase:
    def __init__(self):
        self.expected_values = {}
        self.filename = None
        self.funcname = None
        self.expr_of_interest = None
        self.addr_of_interest = None
        
    def run_checker(self):
        if not self.filename or not self.funcname:
            sys.exit(1)

        # if the file already exists, unlink it
        checker_csv_file_output = Path(self.get_csv_file_name())
        if checker_csv_file_output.exists():
            checker_csv_file_output.unlink()
            
        test_common.run_checker(filename=self.filename,
                                funcname=self.funcname)

    def is_row_of_interest(self, row: List[str]):
        if not self.expr_of_interest or not self.addr_of_interest:
            sys.exit(1)
        else:
            return self.expr_of_interest in row and \
                self.addr_of_interest in row

    def set_expected_csv_value(self, col_name: str, exp_value: Any):
        self.expected_values[col_name] = str(exp_value)

    def get_output_file_stem(self) -> str:
        if not self.filename or not self.funcname:
            sys.exit(1)
        return checker.output_filename_stem(target_filename=self.filename,
                                            target_funcname=self.funcname)

    # gets the name of the csv file generated for this testcase by the
    # checker
    def get_csv_file_name(self) -> str:
        return self.get_output_file_stem() + ".csv"

class CompSimpTestCaseRunner(unittest.TestCase):
    def check_csv_for_expected_values(self, testcase: CompSimpTestCase):
        csv_file = Path(testcase.get_csv_file_name())
        self.assertTrue(csv_file.exists())

        # so this doesn't fail open if no row is found
        num_rows_selected = 0

        with csv_file.open() as f:
            reader = csv.reader(f)
            for row in reader:
                if testcase.is_row_of_interest(row):
                    num_rows_selected += 1
                    for col_name, exp_value in testcase.expected_values.items():
                        actual = test_common.get_csv_value_from_col_name(col_name, row)
                        self.assertIsNotNone(actual)
                        self.assertEqual(exp_value, actual,
                                         msg=f"Non matching value for col: {col_name}")

        # so this doesn't fail open if no row is found
        self.assertEqual(num_rows_selected, 1,
                         msg="Probably couldn't find the row of interest in csv file")
        
    def test_addident(self):
        addident_test_case = CompSimpTestCase()
        addident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "addident.o"
        addident_test_case.funcname = "addident"
        addident_test_case.addr_of_interest = "0x40000d"
        addident_test_case.expr_of_interest = "Add32(t31,t6)"
        addident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="2")
        addident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="True")
        addident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        addident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=addident_test_case)

    def test_mulident(self):
        mulident_test_case = CompSimpTestCase()
        mulident_test_case.addr_of_interest = "0x40000d"
        mulident_test_case.expr_of_interest = "Mul32(t5,t29)"
        mulident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "mulident.o"
        mulident_test_case.funcname = "mulident"
        mulident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="2")
        mulident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="True")
        mulident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        mulident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=mulident_test_case)

    def test_addconstant(self):
        addconstant_test_case = CompSimpTestCase()
        addconstant_test_case.addr_of_interest = "0x400007"
        addconstant_test_case.expr_of_interest = "Add32(t23,0x0000000e)"
        addconstant_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "addconstant.o"
        addconstant_test_case.funcname = "addconstant"
        addconstant_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                     exp_value="1")
        addconstant_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                     exp_value="None")
        addconstant_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                     exp_value="14")
        addconstant_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=addconstant_test_case)

    def test_mulpowtwo(self):
        mulpowtwo_test_case = CompSimpTestCase()
        mulpowtwo_test_case.addr_of_interest = "0x40000d"
        mulpowtwo_test_case.expr_of_interest = "Mul32(t5,t29)"
        mulpowtwo_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "mulpowtwo.o"
        mulpowtwo_test_case.funcname = "mulpowtwo"
        mulpowtwo_test_case.set_expected_csv_value(col_name="numPowerOfTwoOperands",
                                                     exp_value="2")
        mulpowtwo_test_case.set_expected_csv_value(col_name="firstOperandPowerOfTwo",
                                                     exp_value="True")
        mulpowtwo_test_case.set_expected_csv_value(col_name="secondOperandPowerOfTwo",
                                                     exp_value="True")
        mulpowtwo_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=mulpowtwo_test_case)

    def test_mulzero(self):
        mulzero_test_case = CompSimpTestCase()
        mulzero_test_case.addr_of_interest = "0x40000d"
        mulzero_test_case.expr_of_interest = "Mul32(t5,t29)"
        mulzero_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "mulzero.o"
        mulzero_test_case.funcname = "mulzero"
        mulzero_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                     exp_value="2")
        mulzero_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                     exp_value="True")
        mulzero_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                     exp_value="True")
        mulzero_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=mulzero_test_case)

    def test_mulnospecialvalues(self):
        mulnospecialvalues_test_case = CompSimpTestCase()
        mulnospecialvalues_test_case.addr_of_interest = "0x400015"
        mulnospecialvalues_test_case.expr_of_interest = "Mul32(t5,t25)"
        mulnospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "mulnospecialvalues.o"
        mulnospecialvalues_test_case.funcname = "mulnospecialvalues"
        # check no zero elements
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                     exp_value="0")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                     exp_value="False")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                     exp_value="False")

        # check no constant operands (in the sense of the insn doesn't use an immediate)
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                     exp_value="0")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                     exp_value="None")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                     exp_value="None")

        # check no identity elements
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                     exp_value="0")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                     exp_value="False")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                     exp_value="False")

        # check no power of two elements
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="numPowerOfTwoOperands",
                                                     exp_value="0")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandPowerOfTwo",
                                                     exp_value="False")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandPowerOfTwo",
                                                     exp_value="False")
        
        mulnospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=mulnospecialvalues_test_case)

    def test_addnospecialvalues(self):
        addnospecialvalues_test_case = CompSimpTestCase()
        addnospecialvalues_test_case.addr_of_interest = "0x400015"
        addnospecialvalues_test_case.expr_of_interest = "Add32(t27,t6)"
        addnospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "addnospecialvalues.o"
        addnospecialvalues_test_case.funcname = "addnospecialvalues"
        # check no zero elements
        addnospecialvalues_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                     exp_value="0")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                     exp_value="False")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                     exp_value="False")

        # check no constant operands (in the sense of the insn doesn't use an immediate)
        addnospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                     exp_value="0")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                     exp_value="None")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                     exp_value="None")

        # check no identity elements
        addnospecialvalues_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                     exp_value="0")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                     exp_value="False")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                     exp_value="False")

        # check no power of two elements
        addnospecialvalues_test_case.set_expected_csv_value(col_name="numPowerOfTwoOperands",
                                                     exp_value="0")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandPowerOfTwo",
                                                     exp_value="False")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandPowerOfTwo",
                                                     exp_value="False")
        
        addnospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=addnospecialvalues_test_case)

if '__main__' == __name__:
    unittest.main()
