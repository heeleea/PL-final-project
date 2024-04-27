import pytest
from parser import Parser
from tests.test_utils import FILE_NAME
from run import run
from token_utils import ArithmeticOperator, InWords, ComparisonOperator
from lexical_analysis import LexicalAnalysis
from ast_nodes import NumberNode, BinaryOperationNode, UnaryOperationNode, VariableAssignNode, IfNode, ForNode, WhileNode


@pytest.mark.parametrize('input', ['1'])
def test_number_node_creation(input):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ats()

    assert isinstance(ast, NumberNode)


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
    ast = parse.create_ats()

    assert isinstance(ast.node, BinaryOperationNode)
    assert isinstance(ast.node.right_node, NumberNode)
    assert isinstance(ast.node.left_node, NumberNode)
    assert ast.node.operation.type == operation
    assert ast.advance_count == 3


@pytest.mark.parametrize('input,operation', [
    ('VAR i = -5', ArithmeticOperator.MINUS.name),
    ('VAR i = NOT 0', InWords.KEYWORDS.name)
])
def test_unary_operation_node_creation(input, operation):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ats()

    assert isinstance(ast.node, VariableAssignNode)
    assert isinstance(ast.node.value, UnaryOperationNode)
    assert ast.node.value.operation.type == operation

    if ast.node.value.operation.type == InWords.KEYWORDS.name:
        assert ast.node.value.operation.value == ComparisonOperator.NOT.name


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
    ast = parse.create_ats()

    assert isinstance(ast.node, IfNode)


@pytest.mark.parametrize('input,loop_body,end_value,start_value,step', [
    ('FOR i = 0 TO 5 THEN i+2', BinaryOperationNode, 5, 0, None),
    ('FOR i = 1 TO 9 STEP 2 THEN i+2', BinaryOperationNode, 9, 1, 2),
    ('FOR i = 5 TO 17 THEN NOT 0', UnaryOperationNode, 17, 5, None)
])
def test_for_node_creation(input, loop_body, end_value, start_value, step):
    lexer = LexicalAnalysis(input, FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ats()

    assert isinstance(ast.node, ForNode)
    assert isinstance(ast.node.loop_body, loop_body)

    assert ast.node.start_value.token.value == start_value
    assert ast.node.end_value.token.value == end_value

    if ast.node.step is not None:
        assert ast.node.step.token.value == step
    else:
        assert ast.node.step == step

#invalid syntax errors (negative testing)
#testing each node type creation (positive testing)