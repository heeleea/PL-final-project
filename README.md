# Programming Language Course Final Project By Heelee Amitai, Amit Hadad

## Documentation
 The BNF, grammer and 50 lines of code are in `docs` directory
 Part B of the assignment is in the directory `PartB`
 


## Installation
To install, clone this repository, no dependencies needed:
```bash
git clone git@github.com:heeleea/PL-final-project.git
pip install -r requirements.txt
```

## Run the shell
```bash
cd {YOUR_PATH}/PL-final-project
./main
```
The shell will open in terminal

The shell can accept line by line and also blocks of multiline code

For example of 50 lines of multiline code check `doces/50lines`

#### Pay attention - when writing to the shell there is a need to press enter twice

For grammer check `docs/grammer`


## Tests
```bash
python3 -m pytest
```

## Project structure
```
├── built_ins
│   ├── functions.py
│   └── variables.py
├── constans
│   ├── error_signs.py
│   ├── parsing_rules.py
│   └── token_names.py
├── docs
│   ├── 50lines
│   ├── BNF
│   └── grammer
├── entities
│   ├── base_function.py
│   ├── basic_position.py
│   ├── built_in_functions.py
│   ├── context.py
│   ├── list.py
│   ├── number.py
│   ├── position.py
│   ├── string.py
│   ├── symbol_table.py
│   ├── token.py
│   └── value.py
├── error.py
├── lexical_analysis.py
├── main.py
├── nodes
│   ├── binary_operation_node.py
│   ├── call_node.py
│   ├── for_node.py
│   ├── function_definition_node.py
│   ├── if_node.py
│   ├── list_node.py
│   ├── number_node.py
│   ├── string_node.py
│   ├── unary_operation_node.py
│   ├── variable_access_node.py
│   ├── variable_assign_node.py
│   └── while_node.py
├── parser.py
├── PartB
│   ├── ex10.py
│   ├── ex11.py
│   ├── ex12.py
│   ├── ex13.py
│   ├── ex9.py
│   └── lazy evaluation(ex 14).docx
├── print_utils.py
├── README.md
├── run.py
├── semantical_analysis.py
├── tests
│   ├── test_ast.py
│   ├── test_lexer.py
│   ├── test_semantical_analysis.py
│   └── test_utils.py
└── validators
    ├── parser_validator.py
    └── run_time_validator.py


```