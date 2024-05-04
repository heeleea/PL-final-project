import pytest

from semantical_analysis import Number, List
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


@pytest.mark.parametrize("expression,instance,expected", [
    ("[1,2,3] + 4", List, (1, 2, 3, 4)),
    ("[1,2,3] * [3,4,5]", List, (1, 2, 3, 3, 4, 5)),
    ("[1,2,3] - 1", List, (1, 3)),
    ("[1,2,3] - 0", List, (2, 3)),
    ("[1,2,3] - -1", List, (1, 2)),
    ("[1,2,3] - -2", List, (1, 3)),
    ("[1,2,3] / 0", Number, 1),
    ("[1,2,3] / 1", Number, 2),
    ("[1,2,3] / -1", Number,  3)
])
def test_list_operations(expression, instance, expected):
    result, error = run(expression, FILE_NAME)
    assert isinstance(result, instance)

    if isinstance(result, List):
        for i in range(len(expected)):
            assert isinstance(result.elements[i], Number)
            assert result.elements[i].value == expected[i]

    elif isinstance(result, Number):
        assert result.value == expected
