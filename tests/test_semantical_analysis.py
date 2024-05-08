import math

import pytest
from unittest.mock import patch

from entities.number import Number
from entities.list import List
from entities.symbol_table import SymbolTable
from test_utils import FILE_NAME
from run import run
from entities.context import Context
from entities.string import String
from entities.built_in_functions import BuiltInFunctions


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
    assert result.elements[0].value == expected


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
    assert result.elements[0].value == expected


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
    assert result.elements[0].value == expected


@pytest.mark.parametrize('expression,expected', [
    ('"Hello " * 3', "Hello Hello Hello "),
    ('"string part 1" + " and string part 2"', "string part 1 and string part 2"),
    ('"im a string"', "im a string")
])
def test_stings_operations(expression, expected):
    result, error = run(expression, FILE_NAME)
    assert result.elements[0].value == expected


@pytest.mark.parametrize("expression,,expected", [
    ("[1,2,3] + 4", (1, 2, 3, 4)),
    ("[1,2,3] * [3,4,5]", (1, 2, 3, 3, 4, 5)),
    ("[1,2,3] - 1", (1, 3)),
    ("[1,2,3] - 0", (2, 3)),
    ("[1,2,3] - -1", (1, 2)),
    ("[1,2,3] - -2", (1, 3)),
    ("[1,2,3] / 0", 1),
    ("[1,2,3] / 1", 2),
    ("[1,2,3] / -1", 3)
])
def test_list_operations(expression, expected):
    result, error = run(expression, FILE_NAME)

    if isinstance(result, List):
        if isinstance(result.elements[0], Number):
            assert result.elements[0].value == expected
        else:
            elements = result.elements[0].elements
            for i in range(len(elements)):
                assert isinstance(elements[i], Number)
                assert elements[i].value == expected[i]


def test_loop_with_list():
    text = "FOR i=1 TO 9 THEN 2^i"
    result, error = run(text, FILE_NAME)

    elements = result.elements[0].elements

    for i in range(len(elements)):
        assert isinstance(elements[i], Number)
        assert elements[i].value == pow(2, i+1)


@pytest.mark.parametrize("expression,expected", [
    ("NULL", 0),
    ("FALSE", 0),
    ("TRUE", 1),
    ("MATH_PI", math.pi),
    ("IS_NUM(4)", 1),
    ("IS_NUM($)", 0),
    ("IS_STR(\"aaaaa\")", 1),
    ("IS_STR(1)", 0)
])
def test_built_in_functions(expression, expected):
    result, error = run(expression, FILE_NAME)

    if result:
        if isinstance(result, List):
            if len(result.elements) == 1:
                assert result.elements[0].value == expected


@pytest.mark.parametrize("expression,expected", [
    ("PRINT(\"Hello\")", "Hello\n")
])
def test_print_build_in_functions(expression, expected, capfd):
    _, _ = run(expression, FILE_NAME)
    out, err = capfd.readouterr()
    assert out == expected


@pytest.mark.parametrize("expression,expected", [
    ("PRINT_RETURN(\"Hello\")", "Hello")
])
def test_print_return_build_in_functions(expression, expected):
    result, _ = run(expression, FILE_NAME)
    assert str(result.elements[0]) == expected


@pytest.mark.parametrize("input,expected", [
    ("APPEND(list,4)", [1, 2, 3, 4]),
    ("POP(list,2)", [1, 2]),
    ("EXTEND(list,[4,5,6])", [1, 2, 3, 4, 5, 6])
])
def test_list_built_in_functions(input, expected):
    result, _ = run("VAR list=[1,2,3]", FILE_NAME)
    result, _ = run(input, FILE_NAME)
    result, error = run("list", FILE_NAME)

    elements = result.elements[0].elements

    for i in range(len(elements)):
        assert elements[i].value == expected[i]


@pytest.fixture
def execution_context():
    ctx = Context('<test>')
    ctx.symbol_table = SymbolTable()
    return ctx


def test_input_built_in_function(execution_context):
    test_input = "Hello, World!"
    expected_output = String(test_input)

    with patch('builtins.input', return_value=test_input):
        input_function = BuiltInFunctions('input')
        result = input_function.execute_input(execution_context)

        assert isinstance(result.value, String)
        assert str(result.value) == str(expected_output.value)


def test_input_int_built_in_function(execution_context):
    test_input = "6"
    expected_output = Number(6)

    with patch('builtins.input', return_value=test_input):
        input_function = BuiltInFunctions("input_int")
        result = input_function.execute_input_int(execution_context)

        assert isinstance(result.value, Number)
        assert str(result.value) == str(expected_output.value)
