import pytest
from lexical_analysis import LexicalAnalysis
from parser import Parser
from semantical_analysis import SemanticalAnalysis
from unittest.mock import MagicMock
from token_utils import Digit, ArithmeticOperator, Utils
from token_utils import Token
from run import run

FILE_NAME = '<test>'
FIRST_INPUT = 'VAR a = VAR d = 5'
SECOND_INPUT = 'a'
EXPECTED_RESULT = 5

THIRD_INPUT = 'VAR b = 3'
FORTH_INPUT = 'b'
EXPECTED_RESULT_B = 3

FIFTH_INPUT = 'VAR c = a+b+d'
SIXTH_INPUT = 'c'
EXPECTED_RESULT_C = 13


@pytest.mark.parametrize('text,expected', [
    ('1+3', '[INT:1, PLUS, INT:3, END]'),
    ('1/0', '[INT:1, DIVIDE, INT:0, END]'),
    ('-1*2', '[MINUS, INT:1, MULTIPLY, INT:2, END]'),
    ('2*-1', '[INT:2, MULTIPLY, MINUS, INT:1, END]'),
    ('9/3', '[INT:9, DIVIDE, INT:3, END]'),
    ('0+2', '[INT:0, PLUS, INT:2, END]'),
    ('10+1', '[INT:10, PLUS, INT:1, END]'),
    ('VAR a = 5', '[KEYWORD:VAR, IDENTIFIER:a, EQUALS, INT:5, END]'),
    ('VAR b = 7', '[KEYWORD:VAR, IDENTIFIER:b, INT:7, END]'),
])
def test_lexer(text, expected):
    lexer = LexicalAnalysis(text, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert str(tokens) == expected


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
