
class Token:
    def __init__(self, token_type, value=None, start_position=None, end_position=None):
        self.type = token_type
        self.value = value

        if start_position:
            self.start_position = start_position.get_copy()
            self.end_position = start_position.get_copy()
            self.end_position.advance()

        if end_position:
            self.end_position = end_position

    def matches(self, token_type, value):
        return self.type == token_type and self.value == value

    def __repr__(self):
        if self.value or self.value == 0:
            return f'{self.type}:{self.value}'

        return f'{self.type}'


