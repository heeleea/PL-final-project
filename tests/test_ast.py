import pytest

from parser import Parser
from context import Context
from symbol_table import SymbolTable
from tests.test_utils import FILE_NAME
from lexical_analysis import LexicalAnalysis
from semantical_analysis import SemanticalAnalysis, Number
from token_utils import ArithmeticOperator, InWords, ComparisonOperator
from ast_nodes import NumberNode, BinaryOperationNode, UnaryOperationNode, VariableAssignNode, IfNode, ForNode, WhileNode, VariableAccessNode, FunctionDefinitionNode, CallableNode, StringNode, ListNode


def test_number_node_creation():
    lexer = LexicalAnalysis('1', FILE_NAME)
    tokens, error = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    assert isinstance(ast.node, NumberNode)


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
    ast = parse.create_ast()

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
    ast = parse.create_ast()

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
    ast = parse.create_ast()

    assert isinstance(ast.node, ForNode)
    assert isinstance(ast.node.loop_body, loop_body)

    assert ast.node.start_value.token.value == start_value
    assert ast.node.end_value.token.value == end_value

    if ast.node.step is not None:
        assert ast.node.step.token.value == step
    else:
        assert ast.node.step == step


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

    assert isinstance(ast.node, WhileNode)
    assert ast.node.condition
    assert ast.node.loop_body


def test_variable_assign_node():
    lexer = LexicalAnalysis('VAR a=0', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    assert isinstance(ast.node, VariableAssignNode)
    assert ast.node.token.type == InWords.IDENTIFIER.name
    assert ast.node.token.value == 'a'


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

    assert isinstance(ast.node, VariableAccessNode)
    assert ast.node.token.type == InWords.IDENTIFIER.name
    assert ast.node.token.value == 'result'


def test_function_definition_node():
    lexer = LexicalAnalysis("FUNC add(a,b) ~ a + b", FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    assert isinstance(ast.node, FunctionDefinitionNode)


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

    assert isinstance(ast.node, CallableNode)
    assert isinstance(ast.node.callable_node, VariableAccessNode)
    assert isinstance(ast.node.arguments, list)


def test_string_node():
    string = "some text"
    lexer = LexicalAnalysis(f'"{string}"', FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    assert isinstance(ast.node, StringNode)
    assert ast.node.token.value == string


def test_list_node():
    lexer = LexicalAnalysis("[1,2,3]", FILE_NAME)
    tokens, _ = lexer.create_token_stream()

    parse = Parser(tokens)
    ast = parse.create_ast()

    assert isinstance(ast.node, ListNode)
    assert isinstance(ast.node.element_node, list)


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
