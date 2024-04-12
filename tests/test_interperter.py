import pytest
from lexical_analysis import LexicalAnalysis
from parser import Parser
from semantical_analysis import SemanticalAnalysis
from unittest.mock import MagicMock
from token_utils import TokenDigit, TokenOperation, TokenUtils
from token_utils import Token


@pytest.mark.parametrize('text,expected', [
    ('1+3', '[INT:1, PLUS, INT:3, END]'),
    ('1/0', '[INT:1, DIVIDE, INT:0, END]'),
    ('-1*2', '[MINUS, INT:1, MULTIPLY, INT:2, END]'),
    ('2*-1', '[INT:2, MULTIPLY, MINUS, INT:1, END]'),
    ('9/3', '[INT:9, DIVIDE, INT:3, END]'),
    ('0+2', '[INT:0, PLUS, INT:2, END]'),
    ('10+1', '[INT:10, PLUS, INT:1, END]')
])
def test_lexer(text, expected):
    lexer = LexicalAnalysis(text, '<test>')
    tokens, error = lexer.create_token_stream()

    assert str(tokens) == expected


