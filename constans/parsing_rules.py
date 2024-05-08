from constans.token_names import Digit, ArithmeticOperator, Punctuation, InWords, ComparisonOperator

# rule sets that uses the enum values
NUMBER_TYPES = {Digit.INT.value, Digit.FLOAT.value}
STRING_TYPES = {InWords.STRING.value}
EXPRESSION_STARTERS = {Punctuation.LEFT_PARENTHESIS.value}
EXPRESSION_CLOSERS = {Punctuation.RIGHT_PARENTHESIS.value}
LIST_CLOSERS = {Punctuation.RIGHT_SQUARE.value}

# rule that uses the enum names
ADDITIVE_OPERATORS_NAMES = {ArithmeticOperator.PLUS.name, ArithmeticOperator.MINUS.name}
MULTIPLICATIVE_OPERATORS_NAMES = {ArithmeticOperator.MULTIPLY.name, ArithmeticOperator.DIVIDE.name}
EXPRESSION_STARTERS_NAMES = {Punctuation.LEFT_PARENTHESIS.name}
EXPRESSION_CLOSERS_NAMES = {Punctuation.RIGHT_PARENTHESIS.name}
LIST_STARTERS_NAMES = {Punctuation.LEFT_SQUARE.name}
LIST_CLOSERS_NAMES = {Punctuation.RIGHT_SQUARE.name}
IDENTIFIERS_NAMES = {InWords.IDENTIFIER.name}
COMPARISON_EXPRESSION_NAMES = {ComparisonOperator.COMPARISON.name, ComparisonOperator.NOT_EQUALS.name, ComparisonOperator.LESS_THAN.name, ComparisonOperator.GREATER_THAN.name, ComparisonOperator.LESS_THAN_EQUALS.name, ComparisonOperator.GREATER_THAN_EQUALS.name}
ARITHMETIC_NAMES = {ArithmeticOperator.PLUS.name, ArithmeticOperator.MINUS.name}
LOOP_NAMES = {InWords.FOR.name, InWords.WHILE.name}

# rule sets of tuples (edge case)
EXPRESSION_NAMES = {(InWords.KEYWORDS.name, ComparisonOperator.AND.value), (InWords.KEYWORDS.name, ComparisonOperator.OR.value)}
POWER_NAMES = {ArithmeticOperator.POWER.name, }


