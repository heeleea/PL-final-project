import pytest

from parser import Parser
from entities.context import Context
from entities.symbol_table import SymbolTable
from tests.test_utils import FILE_NAME
from lexical_analysis import LexicalAnalysis
from semantical_analysis import SemanticalAnalysis, Number
from constans.token_names import ArithmeticOperator, InWords, ComparisonOperator

from nodes.number_node import NumberNode
from nodes.binary_operation_node import BinaryOperationNode
from nodes.unary_operation_node import UnaryOperationNode
from nodes.variable_assign_node import VariableAssignNode
from nodes.if_node import IfNode
from nodes.for_node import ForNode
from nodes.while_node import WhileNode
from nodes.variable_access_node import VariableAccessNode
from nodes.function_definition_node import FunctionDefinitionNode
from nodes.call_node import CallableNode
from nodes.string_node import StringNode
from nodes.list_node import ListNode


def test_number_node_creation():
    lexer = LexicalAnalysis('1', FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]
    assert isinstance(node, NumberNode)


@pytest.mark.parametrize('input,operation', [
    ('1+2', ArithmeticOperator.PLUS.name),
    ('3-1', ArithmeticOperator.MINUS.name),
    ('1*2', ArithmeticOperator.MULTIPLY.name),
    ('4/2', ArithmeticOperator.DIVIDE.name),
    ('4^2', ArithmeticOperator.POWER.name)
])
def test_binary_operation_node_creation(input, operation):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]
    assert isinstance(node, BinaryOperationNode)
    assert isinstance(node.right_node, NumberNode)
    assert isinstance(node.left_node, NumberNode)
    assert node.operation.type == operation


@pytest.mark.parametrize('input,operation', [
    ('VAR i = -5', ArithmeticOperator.MINUS.name),
    ('VAR i = NOT 0', InWords.KEYWORDS.name)
])
def test_unary_operation_node_creation(input, operation):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]
    assert isinstance(node, VariableAssignNode)
    assert isinstance(node.value, UnaryOperationNode)
    assert node.value.operation.type == operation

    if node.value.operation.type == InWords.KEYWORDS.name:
        assert node.value.operation.value == ComparisonOperator.NOT.name


@pytest.mark.parametrize('input', [
    'IF 5 == 5 THEN 123',
    'IF 4 == 4 THEN 124 ELSE 5',
    'IF 4 == 3 THEN 1 ELSE 2',
    'IF 5 == 1 THEN 6',
    'IF 1 THEN 1',
    'IF 0 THEN 1 ELIF 1 THEN 4'
])
def test_if_node_creation(input):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]
    assert isinstance(node, IfNode)


@pytest.mark.parametrize('input,loop_body,end_value,start_value,step', [
    ('FOR i = 0 TO 5 THEN i+2', BinaryOperationNode, 5, 0, None),
    ('FOR i = 1 TO 9 STEP 2 THEN i+2', BinaryOperationNode, 9, 1, 2),
    ('FOR i = 5 TO 17 THEN NOT 0', UnaryOperationNode, 17, 5, None)
])
def test_for_node_creation(input, loop_body, end_value, start_value, step):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]
    assert isinstance(node, ForNode)
    assert isinstance(node.loop_body, loop_body)

    assert node.start_value.token.value == start_value
    assert node.end_value.token.value == end_value

    if node.step is not None:
        assert node.step.token.value == step
    else:
        assert node.step == step


def test_while_node_creation():
    global_symbol_table = SymbolTable()
    global_symbol_table.set("TRUE", Number(1))
    global_symbol_table.set("FALSE", Number(0))
    global_symbol_table.set("NULL", Number(0))

    lexer = LexicalAnalysis('VAR i=0', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    semantical_analysis = SemanticalAnalysis()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    _ = semantical_analysis.transverse(ast.node, context)

    lexer = LexicalAnalysis('WHILE i<5 THEN i+1', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]
    assert isinstance(node, WhileNode)
    assert node.condition
    assert node.loop_body


def test_variable_assign_node():
    lexer = LexicalAnalysis('VAR a=0', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]
    assert isinstance(node, VariableAssignNode)
    assert node.token.type == InWords.IDENTIFIER.name
    assert node.token.value == 'a'


def test_variable_access_creation():
    global_symbol_table = SymbolTable()
    global_symbol_table.set("TRUE", Number(1))
    global_symbol_table.set("FALSE", Number(0))
    global_symbol_table.set("NULL", Number(0))

    lexer = LexicalAnalysis('VAR result=1', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    semantical_analysis = SemanticalAnalysis()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    _ = semantical_analysis.transverse(ast.node, context)

    lexer = LexicalAnalysis('result', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]

    assert isinstance(node, VariableAccessNode)
    assert node.token.type == InWords.IDENTIFIER.name
    assert node.token.value == 'result'


def test_function_definition_node():
    lexer = LexicalAnalysis("FUNC add(a,b) ~ a + b", FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]

    assert isinstance(node, FunctionDefinitionNode)


def test_call_node():
    global_symbol_table = SymbolTable()
    global_symbol_table.set("TRUE", Number(1))
    global_symbol_table.set("FALSE", Number(0))
    global_symbol_table.set("NULL", Number(0))

    lexer = LexicalAnalysis("FUNC add(a,b) ~ a + b", FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    semantical_analysis = SemanticalAnalysis()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    _ = semantical_analysis.transverse(ast.node, context)

    lexer = LexicalAnalysis("add(1,3)", FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]

    assert isinstance(node, CallableNode)
    assert isinstance(node.callable_node, VariableAccessNode)
    assert isinstance(node.arguments, list)


def test_string_node():
    string = "some text"
    lexer = LexicalAnalysis(f'"{string}"', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]

    assert isinstance(node, StringNode)
    assert node.token.value == string


def test_list_node():
    lexer = LexicalAnalysis("[1,2,3]", FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    node = ast.node.element_node[0]

    assert isinstance(node, ListNode)
    assert isinstance(node.element_node, list)


@pytest.mark.parametrize("input", [
    "1 +",  # Missing operand
    "(",  # Unbalanced parenthesis
    "VAR 123 = 456",  # Invalid identifier name
    "1 == ",  # Missing right-hand operand for comparison
    "IF 1 THEN",  # Missing body for IF statement
    "FOR i = 0 TO",  # Incomplete FOR loop declaration
    "WHILE",  # Incomplete WHILE loop declaration
    "1 + * 2",  # Incorrect operator sequence
    "VAR x = 5 VAR y = 10",  # Missing separator between statements
    "IF THEN 1",  # Missing condition for IF statement
    "1 + (2 * 3",  # Missing closing parenthesis
])
def test_parser_errors(input):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parser = Parser(tokens)
    ast = parser.create_ast()

    assert ast.error is not None, "Expected an error but got none."
