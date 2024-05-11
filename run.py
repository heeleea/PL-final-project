from parser import Parser
from entities.context import Context
from lexical_analysis import LexicalAnalysis
from semantical_analysis import SemanticalAnalysis
from built_ins.variables import NumberRunner
from built_ins.functions import BuiltInFunctionRunner
from entities.symbol_table import SymbolTable
from entities.list import List
from constans.token_names import Punctuation

global_symbol_table = SymbolTable()
global_symbol_table.set("TRUE", NumberRunner.true)
global_symbol_table.set("FALSE", NumberRunner.false)
global_symbol_table.set("NULL", NumberRunner.null)
global_symbol_table.set("MATH_PI", NumberRunner.math_PI)
global_symbol_table.set("PRINT", BuiltInFunctionRunner.print)
global_symbol_table.set("PRINT_RETURN", BuiltInFunctionRunner.print_return)
global_symbol_table.set("INPUT", BuiltInFunctionRunner.input)
global_symbol_table.set("INPUT_INT", BuiltInFunctionRunner.input_int)
global_symbol_table.set("CLEAR", BuiltInFunctionRunner.clear)
global_symbol_table.set("CLS", BuiltInFunctionRunner.clear)
global_symbol_table.set("IS_NUM", BuiltInFunctionRunner.is_number)
global_symbol_table.set("IS_STR", BuiltInFunctionRunner.is_string)
global_symbol_table.set("IS_LIST", BuiltInFunctionRunner.is_list)
global_symbol_table.set("IS_FUN", BuiltInFunctionRunner.is_function)
global_symbol_table.set("APPEND", BuiltInFunctionRunner.append)
global_symbol_table.set("POP", BuiltInFunctionRunner.pop)
global_symbol_table.set("EXTEND", BuiltInFunctionRunner.extend)


def get_multiline_input():
    lines = []
    first_line = True

    while True:
        if first_line:
            line = input("> ")
            first_line = False
        else:
            line = input()

        if line == "":
            break

        lines.append(line)

    return '\n'.join(lines)


def execute(input, file_name):
    if '\n' or Punctuation.BLOCK.value in input:
        return run_all(input, file_name)
    return run(input, file_name)


def run_all(input, file_name):
    blocks = input.split(Punctuation.BLOCK.value) if Punctuation.BLOCK.value in input else [input]

    for block in blocks:
        if block.strip() == "":
            continue

        result, error = run(block.strip(), file_name)
        if error:
            return None, error
        if result:
            print_result(result)

    return None, None


def run(input, file_name):
    print(f"\n{input.strip()}")

    lexer = LexicalAnalysis(input, file_name)
    tokens, error = lexer.create_token_stream()
    if error:
        return None, error

    parser = Parser(tokens)
    ast = parser.create_ast()
    if ast.error:
        return None, ast.error

    semantical_analysis = SemanticalAnalysis()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = semantical_analysis.transverse(ast.node, context)

    return result.value, result.error


def print_result(result):
    if isinstance(result, List):
        if len(result.elements) == 1:
            print(repr(result.elements[0]))
        else:
            print(repr(result.elements))
    else:
        print(result)
