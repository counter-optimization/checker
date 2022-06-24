import unittest
import csv
import typing
from pathlib import Path

import test_common
import checker

class CompSimpTestCases(unittest.TestCase):
    # @classmethod
    # def setUpClass(cls):
    #     assert(test_common.build_test_obj_files())

    # @classmethod
    # def tearDownClass(cls):
    #     assert(test_common.remove_test_obj_files())

    def test_addident(self):
        filename = test_common.COMP_SIMP_TEST_DIR / "addident.o"
        funcname = "addident"
        addr_of_interest = "0x40000d"
        expr_of_interest = "\"Add32(t31,t6)\""
        expected_values = {"numIdentityOperands": "2",
                           "firstOperandIdentity": "True",
                           "secOperandIdentity": "True"}
        
        test_common.run_checker(filename=filename, funcname=funcname)

        output_file_stem = checker.output_filename_stem(target_filename=filename,
                                                        target_funcname=funcname)
        csv_file_name = output_file_stem + ".csv"
        csv_file = Path(csv_file_name)
        assert(csv_file.exists())

        with csv_file.open() as f:
            reader = csv.reader(f)
            for row in reader:
                if expr_of_interest in row:
                    for col_name, exp_value in expected_values.items():
                        actual = test_common.get_csv_value_from_col_name(col_name, row)
                        self.assertIsNotNone(actual)
                        self.assertEqual(exp_value, actual)
        

if '__main__' == __name__:
    unittest.main()
