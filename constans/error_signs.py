from constans.token_names import ArithmeticOperator, Punctuation, Digit, InWords, ComparisonOperator, Utils

EXPECTED_IDENTIFIER =(InWords.IDENTIFIER.name)
EXPECTED_END = (Utils.END.name)
EXPECTED_BLOCK = (Punctuation.BLOCK.value)
EXPECTED_EQUALS = (ComparisonOperator.EQUALS.value)
EXPECTED_THEN = (InWords.THEN.name)
EXPECTED_FOR = (InWords.FOR.name)
EXPECTED_TO = (InWords.TO.name)
EXPECTED_WHILE = (InWords.WHILE.name)
EXPECTED_COMMA = (Punctuation.COMMA.value)
EXPECTED_FUNC = (InWords.FUNC.name)
EXPECTED_START_PARENTHESIS = (Punctuation.LEFT_PARENTHESIS.value)
EXPECTED_CLOSE_PARENTHESIS = (Punctuation.RIGHT_PARENTHESIS.value)
EXPECTED_FUNCTION_ASSIGNMENT = (Punctuation.FUNCTION_ASSIGNMENT.value)
EXPECTED_LIST_CLOSERS = (Punctuation.LEFT_SQUARE.value)
EXPECTED_COMMA_AND_START_PARENTHESIS = (Punctuation.COMMA.value, Punctuation.LEFT_PARENTHESIS.value)
EXPECTED_COMMA_AND_END_PARENTHESIS = (Punctuation.COMMA.value, Punctuation.RIGHT_PARENTHESIS.value)

CREATE_AST = (ArithmeticOperator.PLUS.value, ArithmeticOperator.MINUS.value, ArithmeticOperator.MULTIPLY.value, ArithmeticOperator.DIVIDE.value)
ATOM_MAIN = (Digit.INT.value, Digit.FLOAT.value, InWords.IDENTIFIER.name, ArithmeticOperator.PLUS.value, ArithmeticOperator.MINUS.value, Punctuation.LEFT_PARENTHESIS.value, InWords.IF.name, InWords.FOR.name, InWords.WHILE.name, InWords.FUNC.name)
CALL_MAIN = (Punctuation.RIGHT_PARENTHESIS.value, InWords.VAR.value, InWords.IF.value, InWords.FOR.value,InWords.WHILE.value,InWords.FUNC.value, Digit.INT.value, Digit.FLOAT.name, InWords.IDENTIFIER.name)
EXPRESSION_MAIN = (Digit.INT.value, Digit.FLOAT.value, InWords.IDENTIFIER.name, InWords.VAR.name, ArithmeticOperator.PLUS.value, ArithmeticOperator.MINUS.value, Punctuation.LEFT_PARENTHESIS.value)
COMPARISON_EXPRESSIONS_MAIN = (Digit.INT.value, Digit.FLOAT.value, InWords.IDENTIFIER.name, ArithmeticOperator.PLUS.value, ArithmeticOperator.MINUS.value, Punctuation.LEFT_PARENTHESIS.value)
LIST_EXPRESSION = (Punctuation.RIGHT_SQUARE.value, InWords.VAR.value, InWords.IF.value, InWords.FOR.value, InWords.WHILE.value, InWords.FUNC.value, Digit.INT.value, Digit.FLOAT.name, InWords.IDENTIFIER.name)
LIST_MAIN = (Punctuation.COMMA.value, Punctuation.RIGHT_SQUARE.value)
FUNC_DEFINITION_NEW_LINE = (Punctuation.FUNCTION_ASSIGNMENT.value, InWords.NEWLINE.name)


