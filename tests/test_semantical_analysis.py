import pytest

from semantical_analysis import Number
from symbol_table import SymbolTable
from test_utils import FILE_NAME
from run import run


@pytest.fixture
def setup_env():
    symbol_table = SymbolTable()
    symbol_table.set("TRUE", Number(1))
    symbol_table.set("FALSE", Number(0))
    symbol_table.set("NULL", Number(0))
    return symbol_table


@pytest.mark.parametrize('expression,expected', [
    ('1+2', 3),
    ('1-2', -1),
    ('4-1', 3),
    ('6/3', 2),
    ('1/2', 0.5),
    ('3*3', 9),
    ('3^2', 9),
])
def test_arithmetic_operators(expression, expected):
    result, error = run(expression, FILE_NAME)
    assert result.value == expected


@pytest.mark.parametrize('expression,expected', [
    ('1 == 1', 1),
    ('1 == 2', 0),
    ('1 != 2', 1),
    ('2 != 2', 0),
    ('1 < 2', 1),
    ('2 < 1', 0),
    ('2 > 1', 1),
    ('1 > 2', 0),
    ('1 <= 1', 1),
    ('1 <= 0', 0),
    ('2 >= 1', 1),
    ('1 >= 2', 0)
])
def test_comparison_operators(expression, expected):
    result, error = run(expression, FILE_NAME)
    assert result.value == expected


@pytest.mark.parametrize('expression,expected', [
    ('TRUE AND FALSE', 0),
    ('TRUE AND TRUE', 1),
    ('FALSE AND FALSE', 0),
    ('TRUE OR FALSE', 1),
    ('FALSE OR FALSE', 0),
    ('NOT TRUE', 0),
    ('NOT FALSE', 1)
])
def test_logical_operators(expression, expected, setup_env):
    result, error = run(expression, FILE_NAME)
    assert result.value == expected


@pytest.mark.parametrize('expression,expected', [
    ('"Hello " * 3', "Hello Hello Hello "),
    ('"string part 1" + " and string part 2"', "string part 1 and string part 2"),
    ('"im a string"', "im a string")
])
def test_stings_operations(expression, expected):
    result, error = run(expression, FILE_NAME)
    assert result.value == expected
