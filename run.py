from parser import Parser
from context import Context
from lexical_analysis import LexicalAnalysis
from semantical_analysis import SemanticalAnalysis, NumberRunner, BuiltInFunctionRunner
from symbol_table import SymbolTable


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


def run(input, file_name):
    lexer = LexicalAnalysis(input, file_name)
    tokens, error = lexer.create_token_stream()
    if error:
        return None, error

    parse = Parser(tokens)
    ast = parse.create_ast()
    if ast.error:
        return None, ast.error

    semantical_analysis = SemanticalAnalysis()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = semantical_analysis.transverse(ast.node, context)

    return result.value, result.error
