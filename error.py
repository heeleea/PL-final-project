import copy


class Position:
    def __init__(self, index, line, column):
        self.index = index
        self.line = line
        self.column = column

    def advance(self, current_char):
        self.index += 1
        self.column += 1

        if current_char == '/n':
            self.line += 1
            self.column = 0

        return self

    def get_position(self):
        return copy.copy(self)


class Error:
    def __init__(self, error_name, error_details, start_position, end_position):
        self.error_name = error_name
        self.error_details = error_details
        self.start_position = start_position
        self.end_position = end_position

    def to_string(self):
        result = f'Error: {self.error_name}: {self.error_details}'
        return result


class IllegalCharError(Error):
    def __init__(self, error_details, start_position, end_position):
        super().__init__('Illegal Character', error_details, start_position, end_position)
