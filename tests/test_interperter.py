import pytest
from lexical_analysis import LexicalAnalysis
from parser import Parser
from semantical_analysis import SemanticalAnalysis
from unittest.mock import MagicMock
from token_utils import Digit, ArithmeticOperator, Utils
from token_utils import Token
from run import run
from tests.test_utils import FILE_NAME

FIRST_INPUT = 'VAR a = VAR d = 5'
SECOND_INPUT = 'a'
EXPECTED_RESULT = 5

THIRD_INPUT = 'VAR b = 3'
FORTH_INPUT = 'b'
EXPECTED_RESULT_B = 3

FIFTH_INPUT = 'VAR c = a+b+d'
SIXTH_INPUT = 'c'
EXPECTED_RESULT_C = 13


def test_variable_assignment():
    test_cases = [
        (FIRST_INPUT, EXPECTED_RESULT),
        (SECOND_INPUT, EXPECTED_RESULT),
        (THIRD_INPUT, EXPECTED_RESULT_B),
        (FORTH_INPUT, EXPECTED_RESULT_B),
        (FIFTH_INPUT, EXPECTED_RESULT_C),
        (SIXTH_INPUT, EXPECTED_RESULT_C)
    ]

    for input_code, expected_result in test_cases:
        result, error = run(input_code, FILE_NAME)
        assert result.value == expected_result
