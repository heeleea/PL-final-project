import pytest

from error import IllegalCharError
from tests.test_utils import FILE_NAME
from lexical_analysis import LexicalAnalysis
from token_utils import Digit, ArithmeticOperator, Utils, ComparisonOperator, InWords, Punctuation


@pytest.mark.parametrize('input,expected', [
    ('1', Digit.INT.name),
    ('1.33', Digit.FLOAT.name)
])
def test_number_types(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == expected
    assert tokens[1].type == Utils.END.name


@pytest.mark.parametrize('input,expected', [
    ('11.3.1', Digit.INT.name),
])
def test_non_verified_floats(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert isinstance(error, IllegalCharError)


@pytest.mark.parametrize('input,expected', [
    ('+', ArithmeticOperator.PLUS.name),
    ('-', ArithmeticOperator.MINUS.name),
    ('/', ArithmeticOperator.DIVIDE.name),
    ('*', ArithmeticOperator.MULTIPLY.name),
    ('=', ArithmeticOperator.EQUALS.name),
    ('^', ArithmeticOperator.POWER.name)
])
def test_arithmetic_operators(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == expected
    assert tokens[1].type == Utils.END.name


@pytest.mark.parametrize('input,expected', [
    ('=', ComparisonOperator.EQUALS.name),
    ('==', ComparisonOperator.COMPARISON.name),
    ('!=', ComparisonOperator.NOT_EQUALS.name),
    ('<', ComparisonOperator.LESS_THAN.name),
    ('>', ComparisonOperator.GREATER_THAN.name),
    ('<=', ComparisonOperator.LESS_THAN_EQUALS.name),
    ('>=', ComparisonOperator.GREATER_THAN_EQUALS.name),
])
def test_arithmetic_operators(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == expected
    assert tokens[1].type == Utils.END.name


@pytest.mark.parametrize('input,expected', [
    ('NOT', ComparisonOperator.NOT.name),
    ('OR', ComparisonOperator.OR.name),
    ('AND', ComparisonOperator.AND.name)
])
def test_logic_operators(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == InWords.KEYWORDS.name
    assert tokens[0].value == expected
    assert tokens[1].type == Utils.END.name


@pytest.mark.parametrize('input,expected', [
    ('VAR', InWords.VAR.name),
    ('IF', InWords.IF.name),
    ('THEN', InWords.THEN.name),
    ('ELIF', InWords.ELIF.name),
    ('ELSE', InWords.ELSE.name),
    ('FOR', InWords.FOR.name),
    ('TO', InWords.TO.name),
    ('STEP', InWords.STEP.name),
    ('WHILE', InWords.WHILE.name)
])
def test_detect_inwords(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == InWords.KEYWORDS.name
    assert tokens[0].value == expected
    assert tokens[1].type == Utils.END.name


@pytest.mark.parametrize('input,expected', [
    ('(', Punctuation.LEFT_PARENTHESIS.name),
    (')', Punctuation.RIGHT_PARENTHESIS.name)
])
def test_detect_punctuation(input,expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == expected
    assert tokens[1].type == Utils.END.name
