import copy


class Position:
    def __init__(self, index, line, column, file_name, file_text):
        self.index = index
        self.line = line
        self.column = column
        self.file_name = file_name
        self.file_text = file_text

    def advance(self, current_char=None):
        self.index += 1
        self.column += 1

        if current_char == '\n':
            self.line += 1
            self.column = 0

        return self

    def get_position(self):
        return Position(self.index, self.line, self.column, self.file_name, self.file_text)


class Error:
    def __init__(self, name, details, start_position, end_position):
        self.name = name
        self.details = details
        self.start_position = start_position
        self.end_position = end_position

    def to_string(self):
        result = f"Error: {self.name}: {self.details}\n"
        result += f"File: {self.start_position.file_name}, Line {self.start_position.line + 1}"
        add_arrows = print_error_with_arrows(self.start_position.file_text, self.start_position, self.end_position)
        result += f"\n\n{add_arrows}"
        return result


class IllegalCharError(Error):
    def __init__(self, error_details, start_position, end_position):
        super().__init__('Illegal Character', error_details, start_position, end_position)
