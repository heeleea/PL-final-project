import pytest

from lexical_analysis import LexicalAnalysis
from error import IllegalCharError, ExpectedCharError
from constans.token_names import Digit, ArithmeticOperator, Utils, ComparisonOperator, InWords, Punctuation
from entities.token import Token

FILE_NAME = '<test>'


@pytest.mark.parametrize('input,expected', [
    ('1', Digit.INT.name),
    ('1.33', Digit.FLOAT.name)
])
def test_number_types(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == expected
    assert tokens[1].type == Utils.EOF.name


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
    assert tokens[1].type == Utils.EOF.name


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
    assert tokens[1].type == Utils.EOF.name


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
    assert tokens[1].type == Utils.EOF.name


@pytest.mark.parametrize('input,expected', [
    ('VAR', InWords.VAR.name),
    ('IF', InWords.IF.name),
    ('THEN', InWords.THEN.name),
    ('ELIF', InWords.ELIF.name),
    ('ELSE', InWords.ELSE.name),
    ('FOR', InWords.FOR.name),
    ('TO', InWords.TO.name),
    ('STEP', InWords.STEP.name),
    ('WHILE', InWords.WHILE.name),
    ('FUNC', InWords.FUNC.name)
])
def test_detect_inwords(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == InWords.KEYWORDS.name
    assert tokens[0].value == expected
    assert tokens[1].type == Utils.EOF.name


@pytest.mark.parametrize('input,expected', [
    ('(', Punctuation.LEFT_PARENTHESIS.name),
    (')', Punctuation.RIGHT_PARENTHESIS.name),
    ('[', Punctuation.LEFT_SQUARE.name),
    (']', Punctuation.RIGHT_SQUARE.name),
    (',', Punctuation.COMMA.name),
    ('~', Punctuation.FUNCTION_ASSIGNMENT.name),
    ('""', Punctuation.STRING.name)
])
def test_detect_punctuation(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == expected
    assert tokens[1].type == Utils.EOF.name


@pytest.mark.parametrize('text,length,expected', [
    ('1+3', 4, [(1, Digit.INT.name), (None, ArithmeticOperator.PLUS.name), (3, Digit.INT.name), (None, Utils.EOF.name)]),
    ('1/0', 4, [(1, Digit.INT.name), (None, ArithmeticOperator.DIVIDE.name), (0, Digit.INT.name), (None, Utils.EOF.name)]),
    ('-1*2', 5, [(None, ArithmeticOperator.MINUS.name), (1, Digit.INT.name), (None, ArithmeticOperator.MULTIPLY.name), (2, Digit.INT.name), (None, Utils.EOF.name)]),
    ('2*-1', 5, [(2, Digit.INT.name), (None, ArithmeticOperator.MULTIPLY.name), (None, ArithmeticOperator.MINUS.name), (1, Digit.INT.name), (None, Utils.EOF.name)]),
    ('9/3', 4, [(9, Digit.INT.name), (None, ArithmeticOperator.DIVIDE.name), (3, Digit.INT.name), (None, Utils.EOF.name)]),
    ('0+2', 4, [(0, Digit.INT.name), (None, ArithmeticOperator.PLUS.name), (2, Digit.INT.name), (None, Utils.EOF.name)]),
    ('10+1', 4, [(10, Digit.INT.name), (None, ArithmeticOperator.PLUS.name), (1, Digit.INT.name), (None, Utils.EOF.name)]),
    ('VAR a = 5', 5,  [(InWords.VAR.name, InWords.KEYWORDS.name), ('a', InWords.IDENTIFIER.name), (None, ArithmeticOperator.EQUALS.name), (5, Digit.INT.name), (None, Utils.EOF.name)]),
    ('VAR b = 7', 4, [(InWords.VAR.name, InWords.KEYWORDS.name), ('b', InWords.IDENTIFIER.name), (None, ArithmeticOperator.EQUALS.name), (7, Digit.INT.name), (None, Utils.EOF.name)]),
    ('FUNC add(a,b) ~ a + b', 12, [(InWords.FUNC.value, InWords.KEYWORDS.name), ('add', InWords.IDENTIFIER.name), (None, Punctuation.LEFT_PARENTHESIS.name),
                                   ('a', InWords.IDENTIFIER.name), (None, Punctuation.COMMA.name), ('b', InWords.IDENTIFIER.name), (None, Punctuation.RIGHT_PARENTHESIS.name),
                                   (None, Punctuation.FUNCTION_ASSIGNMENT.name), ('a', InWords.IDENTIFIER.name), (None, ArithmeticOperator.PLUS.name), ('b', InWords.IDENTIFIER.name), (None, Utils.EOF.name)]),
    ('[]', 3, [(None, Punctuation.LEFT_SQUARE.name), (None, Punctuation.RIGHT_SQUARE.name), (None, Utils.EOF.name)]),
    ('[1,2,3]', 8, [(None, Punctuation.LEFT_SQUARE.name), (1, Digit.INT.name), (None, Punctuation.COMMA.name), (2, Digit.INT.name), (None, Punctuation.COMMA.name), (3, Digit.INT.name), (None, Punctuation.RIGHT_SQUARE.name), (None, Utils.EOF.name)])
])
def test_create_token_stream(text, length, expected):
    lexer = LexicalAnalysis(text, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    for i in range(length):
        assert isinstance(tokens[i], Token)
        assert expected[i][1] == tokens[i].type
        assert expected[i][0] == tokens[i].value


@pytest.mark.parametrize("input,expected_string,expected_type", [
    ("a string to test with", "a string to test with", InWords.STRING.name),
    ("a string\\nwith\\tescapes", "a string\nwith\tescapes", InWords.STRING.name)

])
def test_detect_string(input, expected_string, expected_type):
    lexer = LexicalAnalysis(f'"{input}"', FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert tokens[0].type == expected_type
    assert tokens[0].value == expected_string


@pytest.mark.parametrize('input', ['$', '@', '#', '%', '&'])
def test_detect_illegal_char(input):
    lexer = LexicalAnalysis(input, FILE_NAME)
    _, error = lexer.create_token_stream()

    assert isinstance(error, IllegalCharError)
    assert error.details == f"'{input}'"


@pytest.mark.parametrize("input,expected", [
    ('"some text', "\" is missing at the end of the string"),  # negative testing for detect string
    ('!', "'=' (after '!')")  # negative testing for detect not equals
])
def test_expected_char_error(input, expected):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    assert isinstance(error, ExpectedCharError)
    assert error.details == expected
