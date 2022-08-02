import claripy

import unittest
import csv
import sys
import typing
from typing import Callable, List, Any
from pathlib import Path

from typeguard.importhook import install_import_hook
install_import_hook('test_common')

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

    def expect_no_special_values(self):
        self.set_expected_csv_value(col_name="numZeroElementOperands",
                                    exp_value="0")
        self.set_expected_csv_value(col_name="firstOperandZeroElem",
                                    exp_value="False")
        self.set_expected_csv_value(col_name="secondOperandZeroElem",
                                    exp_value="False")

        # check no constant operands (in the sense of the insn doesn't use an immediate)
        self.set_expected_csv_value(col_name="numConstantOperands",
                                    exp_value="0")
        self.set_expected_csv_value(col_name="firstOperandConst",
                                    exp_value="None")
        self.set_expected_csv_value(col_name="secondOperandConst",
                                    exp_value="None")

        # check no identity elements
        self.set_expected_csv_value(col_name="numIdentityOperands",
                                    exp_value="0")
        self.set_expected_csv_value(col_name="firstOperandIdentity",
                                    exp_value="False")
        self.set_expected_csv_value(col_name="secondOperandIdentity",
                                    exp_value="False")

        # check no power of two elements
        self.set_expected_csv_value(col_name="numPowerOfTwoOperands",
                                    exp_value="0")
        self.set_expected_csv_value(col_name="firstOperandPowerOfTwo",
                                    exp_value="False")
        self.set_expected_csv_value(col_name="secondOperandPowerOfTwo",
                                    exp_value="False")

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

        selected_rows = []

        with csv_file.open() as f:
            reader = csv.reader(f)
            for row in reader:
                if testcase.is_row_of_interest(row):
                    selected_rows.append(row)
                    num_rows_selected += 1
                    for col_name, exp_value in testcase.expected_values.items():
                        actual = test_common.get_csv_value_from_col_name(col_name, row)
                        self.assertIsNotNone(actual)
                        self.assertEqual(exp_value, actual,
                                         msg=f"Non matching value for col: {col_name}")

        if testcase.funcname == 'xorident':
            with open('xorident.log', mode='w') as f:
                writer = csv.writer(f)
                writer.writerows(selected_rows)

        # so this doesn't fail open if no row is found
        self.assertEqual(num_rows_selected, 1,
                         msg="Probably couldn't find the row of interest in csv file")

    def int_to_bv_str(self, x: int, width: int = 32) -> str:
        return str(claripy.BVV(x, width))

    def test_ornospecialvalues(self):
        ornospecialvalues_test_case = CompSimpTestCase()
        ornospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "ornospecialvalues.o"
        ornospecialvalues_test_case.funcname = "ornospecialvalues"
        ornospecialvalues_test_case.addr_of_interest = "0x400015"
        ornospecialvalues_test_case.expr_of_interest = "Or32(t7,t6)"
        ornospecialvalues_test_case.expect_no_special_values()
        ornospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                           exp_value="2")
        ornospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                           exp_value=self.int_to_bv_str(17))
        ornospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                           exp_value=self.int_to_bv_str(34))
        ornospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=ornospecialvalues_test_case)

    def test_shlident(self):
        shlident_test_case = CompSimpTestCase()
        shlident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "shlident.o"
        shlident_test_case.funcname = "shlident"
        shlident_test_case.addr_of_interest = "0x400010"
        shlident_test_case.expr_of_interest = "Shl64(t8,t43)"
        shlident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="1")
        shlident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="False")
        shlident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        shlident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=shlident_test_case)

    def test_shrident(self):
        shrident_test_case = CompSimpTestCase()
        shrident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "shrident.o"
        shrident_test_case.funcname = "shrident"
        shrident_test_case.addr_of_interest = "0x400010"
        shrident_test_case.expr_of_interest = "Sar64(t8,t11)"
        shrident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="1")
        shrident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="False")
        shrident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        shrident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=shrident_test_case)

    def test_orident(self):
        orident_test_case = CompSimpTestCase()
        orident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "orident.o"
        orident_test_case.funcname = "orident"
        orident_test_case.addr_of_interest = "0x40000d"
        orident_test_case.expr_of_interest = "Or32(t7,t6)"
        orident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="2")
        orident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="True")
        orident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        orident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=orident_test_case)

    def test_xorident(self):
        xorident_test_case = CompSimpTestCase()
        xorident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "xorident.o"
        xorident_test_case.funcname = "xorident"
        xorident_test_case.addr_of_interest = "0x40000d"
        xorident_test_case.expr_of_interest = "Xor32(t7,t6)"
        xorident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="2")
        xorident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="True")
        xorident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        xorident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=xorident_test_case)

    def test_xornospecialvalues(self):
        xornospecialvalues_test_case = CompSimpTestCase()
        xornospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "xornospecialvalues.o"
        xornospecialvalues_test_case.funcname = "xornospecialvalues"
        xornospecialvalues_test_case.addr_of_interest = "0x400015"
        xornospecialvalues_test_case.expr_of_interest = "Xor32(t7,t6)"
        xornospecialvalues_test_case.expect_no_special_values()
        xornospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                            exp_value="2")
        xornospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                            exp_value=self.int_to_bv_str(17))
        xornospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                            exp_value=self.int_to_bv_str(34))
        xornospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=xornospecialvalues_test_case)

    def test_andident(self):
        andident_test_case = CompSimpTestCase()
        andident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "andident.o"
        andident_test_case.funcname = "andident"
        andident_test_case.addr_of_interest = "0x40000d"
        andident_test_case.expr_of_interest = "And32(t7,t6)"
        andident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="2")
        andident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="True")
        andident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        andident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=andident_test_case)

    def test_andzero(self):
        andzero_test_case = CompSimpTestCase()
        andzero_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "andzero.o"
        andzero_test_case.funcname = "andzero"
        andzero_test_case.addr_of_interest = "0x40000d"
        andzero_test_case.expr_of_interest = "And32(t7,t6)"
        andzero_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                  exp_value="2")
        andzero_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                  exp_value="True")
        andzero_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                  exp_value="True")
        andzero_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=andzero_test_case)

    def test_andnospecialvalues(self):
        andnospecialvalues_test_case = CompSimpTestCase()
        andnospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "andnospecialvalues.o"
        andnospecialvalues_test_case.funcname = "andnospecialvalues"
        andnospecialvalues_test_case.addr_of_interest = "0x400015"
        andnospecialvalues_test_case.expr_of_interest = "And32(t7,t6)"
        andnospecialvalues_test_case.expect_no_special_values()
        andnospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                            exp_value="2")
        andnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                            exp_value=self.int_to_bv_str(34))
        andnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                            exp_value=self.int_to_bv_str(17))
        andnospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=andnospecialvalues_test_case)
        
    def test_subident(self):
        subident_test_case = CompSimpTestCase()
        subident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "subident.o"
        subident_test_case.funcname = "subident"
        subident_test_case.addr_of_interest = "0x40000d"
        subident_test_case.expr_of_interest = "Sub32(t7,t6)"
        subident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="1")
        subident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="False")
        subident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        subident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=subident_test_case)

    def test_subnospecialvalues(self):
        subnospecialvalues_test_case = CompSimpTestCase()
        subnospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "subnospecialvalues.o"
        subnospecialvalues_test_case.funcname = "subnospecialvalues"
        subnospecialvalues_test_case.addr_of_interest = "0x400015"
        subnospecialvalues_test_case.expr_of_interest = "Sub32(t7,t6)"
        subnospecialvalues_test_case.expect_no_special_values()
        subnospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                            exp_value="2")
        subnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                            exp_value=self.int_to_bv_str(34))
        subnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                            exp_value=self.int_to_bv_str(17))
        subnospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=subnospecialvalues_test_case) 
        
    def test_addident(self):
        addident_test_case = CompSimpTestCase()
        addident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "addident.o"
        addident_test_case.funcname = "addident"
        addident_test_case.addr_of_interest = "0x40000d"
        addident_test_case.expr_of_interest = "Add32(t7,t6)"
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
        mulident_test_case.expr_of_interest = "Mul32(t5,t6)"
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

    def test_divident(self):
        divident_test_case = CompSimpTestCase()
        divident_test_case.addr_of_interest = "0x40000e"
        divident_test_case.expr_of_interest = "DivModS64to32(t8,t7)"
        divident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "divident.o"
        divident_test_case.funcname = "divident"
        divident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="1")
        divident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="False")
        divident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        divident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=divident_test_case)

    def test_udivident(self):
        udivident_test_case = CompSimpTestCase()
        udivident_test_case.addr_of_interest = "0x40000f"
        udivident_test_case.expr_of_interest = "DivModU64to32(t11,t10)"
        udivident_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "udivident.o"
        udivident_test_case.funcname = "udivident"
        udivident_test_case.set_expected_csv_value(col_name="numIdentityOperands",
                                                  exp_value="1")
        udivident_test_case.set_expected_csv_value(col_name="firstOperandIdentity",
                                                  exp_value="False")
        udivident_test_case.set_expected_csv_value(col_name="secondOperandIdentity",
                                                  exp_value="True")
        udivident_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=udivident_test_case)

    def test_addconstant(self):
        addconstant_test_case = CompSimpTestCase()
        addconstant_test_case.addr_of_interest = "0x40000a"
        addconstant_test_case.expr_of_interest = "Add32(t6,t5)"
        addconstant_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "addconstant.o"
        addconstant_test_case.funcname = "addconstant"
        addconstant_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                     exp_value="1")
        addconstant_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                     exp_value="None")
        addconstant_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                     exp_value=self.int_to_bv_str(14))
        addconstant_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=addconstant_test_case)

    def test_mulpowtwo(self):
        mulpowtwo_test_case = CompSimpTestCase()
        mulpowtwo_test_case.addr_of_interest = "0x40000d"
        mulpowtwo_test_case.expr_of_interest = "Mul32(t5,t6)"
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

    def test_divpowtwo(self):
        divpowtwo_test_case = CompSimpTestCase()
        divpowtwo_test_case.addr_of_interest = "0x40000e"
        divpowtwo_test_case.expr_of_interest = "DivModS64to32(t8,t7)"
        divpowtwo_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "divpowtwo.o"
        divpowtwo_test_case.funcname = "divpowtwo"
        divpowtwo_test_case.set_expected_csv_value(col_name="numPowerOfTwoOperands",
                                                     exp_value="2")
        divpowtwo_test_case.set_expected_csv_value(col_name="firstOperandPowerOfTwo",
                                                     exp_value="True")
        divpowtwo_test_case.set_expected_csv_value(col_name="secondOperandPowerOfTwo",
                                                     exp_value="True")
        divpowtwo_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=divpowtwo_test_case)

    def test_mulzero(self):
        mulzero_test_case = CompSimpTestCase()
        mulzero_test_case.addr_of_interest = "0x40000d"
        mulzero_test_case.expr_of_interest = "Mul32(t5,t6)"
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

    def test_divzero(self):
        divzero_test_case = CompSimpTestCase()
        divzero_test_case.addr_of_interest = "0x40000e"
        divzero_test_case.expr_of_interest = "DivModS64to32(t8,t7)"
        divzero_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "divzero.o"
        divzero_test_case.funcname = "divzero"
        divzero_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                     exp_value="1")
        divzero_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                     exp_value="True")
        divzero_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                     exp_value="False")
        divzero_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=divzero_test_case)

    def test_shlzero(self):
        shlzero_test_case = CompSimpTestCase()
        shlzero_test_case.addr_of_interest = "0x400010"
        shlzero_test_case.expr_of_interest = "Shl64(t8,t11)"
        shlzero_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "shlzero.o"
        shlzero_test_case.funcname = "shlzero"
        shlzero_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                     exp_value="1")
        shlzero_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                     exp_value="True")
        shlzero_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                     exp_value="False")
        shlzero_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=shlzero_test_case)

    def test_shrzero(self):
        shrzero_test_case = CompSimpTestCase()
        shrzero_test_case.addr_of_interest = "0x400010"
        shrzero_test_case.expr_of_interest = "Shr64(t8,t11)"
        shrzero_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "shrzero.o"
        shrzero_test_case.funcname = "shrzero"
        shrzero_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                     exp_value="1")
        shrzero_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                     exp_value="True")
        shrzero_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                     exp_value="False")
        shrzero_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=shrzero_test_case)

    def test_mulconcretezero(self):
        mulconcretezero_test_case = CompSimpTestCase()
        mulconcretezero_test_case.addr_of_interest = "0x400010"
        mulconcretezero_test_case.expr_of_interest = "Mul32(t8,t9)"
        mulconcretezero_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "mulconcretezero.o"
        mulconcretezero_test_case.funcname = "mulconcretezero"
        mulconcretezero_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                         exp_value="1")
        mulconcretezero_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                         exp_value="None")
        mulconcretezero_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                         exp_value=self.int_to_bv_str(0))
        mulconcretezero_test_case.set_expected_csv_value(col_name="numZeroElementOperands",
                                                         exp_value="1")
        mulconcretezero_test_case.set_expected_csv_value(col_name="firstOperandZeroElem",
                                                         exp_value="False")
        mulconcretezero_test_case.set_expected_csv_value(col_name="secondOperandZeroElem",
                                                         exp_value="True")
        mulconcretezero_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=mulconcretezero_test_case)

    def test_mulnospecialvalues(self):
        mulnospecialvalues_test_case = CompSimpTestCase()
        mulnospecialvalues_test_case.addr_of_interest = "0x400015"
        mulnospecialvalues_test_case.expr_of_interest = "Mul32(t5,t6)"
        mulnospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "mulnospecialvalues.o"
        mulnospecialvalues_test_case.funcname = "mulnospecialvalues"
        mulnospecialvalues_test_case.expect_no_special_values()
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                            exp_value="2")
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                            exp_value=self.int_to_bv_str(34))
        mulnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                            exp_value=self.int_to_bv_str(17))
        mulnospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=mulnospecialvalues_test_case)

    def test_addnospecialvalues(self):
        addnospecialvalues_test_case = CompSimpTestCase()
        addnospecialvalues_test_case.addr_of_interest = "0x400015"
        addnospecialvalues_test_case.expr_of_interest = "Add32(t7,t6)"
        addnospecialvalues_test_case.filename = test_common.COMP_SIMP_TEST_DIR / "addnospecialvalues.o"
        addnospecialvalues_test_case.funcname = "addnospecialvalues"
        addnospecialvalues_test_case.expect_no_special_values()
        addnospecialvalues_test_case.set_expected_csv_value(col_name="numConstantOperands",
                                                            exp_value="2")
        addnospecialvalues_test_case.set_expected_csv_value(col_name="firstOperandConst",
                                                            exp_value=self.int_to_bv_str(17))
        addnospecialvalues_test_case.set_expected_csv_value(col_name="secondOperandConst",
                                                            exp_value=self.int_to_bv_str(34))
        addnospecialvalues_test_case.run_checker()
        self.check_csv_for_expected_values(testcase=addnospecialvalues_test_case)

if '__main__' == __name__:
    unittest.main()
