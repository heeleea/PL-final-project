def print_error_with_arrows(text, start_position, end_position):
    result = ''

    start_index = max(text.rfind('\n', 0, start_position.index), 0)
    end_index = text.find('\n', start_index + 1)

    if end_index < 0:
        end_index = len(text)

    line_count = end_position.line - start_position.line + 1

    for i in range(line_count):
        line = text[start_index:end_index]
        start_column = start_position.column if i == 0 else 0
        end_column = end_position.column if i == line_count - 1 else len(line) - 1

        result += line + '\n'
        result += ' ' * start_column + '^' * (end_column - start_column)

        start_index = end_index
        end_index = text.find('\n', start_index + 1)

        if end_index < 0:
            end_index = len(text)

    return result.replace('\t', '')
