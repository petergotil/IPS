# IPS - Compiler Project

This is a compiler implementation for the IPS (Implementering af Programmerings Sprog) course at UCPH.

## Project Structure

```
IPS/
├── src/                    # Source code
│   ├── AST/                # Abstract Syntax Tree definitions
│   ├── Lexer/              # Lexical analysis (tokenization)
│   ├── Parser/             # Syntax analysis (parsing)
│   ├── TypeChecker/        # Semantic analysis
│   ├── CodeGen/            # Code generation (LLVM IR)
│   ├── Compiler.fs         # Main compiler pipeline
│   └── Program.fs          # Command-line interface
├── tests/                  # Test files
├── examples/               # Example programs
└── IPS.fsproj              # Project file
```

## Components

1. **Lexer**: Transforms source code into tokens
2. **Parser**: Converts tokens into an Abstract Syntax Tree
3. **TypeChecker**: Performs semantic analysis on the AST
4. **CodeGen**: Generates LLVM IR from the AST
5. **Compiler**: Orchestrates the compilation process and uses clang to compile LLVM IR to machine code

## Building and Running

### Prerequisites

- .NET SDK 6.0 or later
- F# compiler
- LLVM toolchain (including clang)

### Build the Project

```bash
dotnet build
```

### Run the Compiler

```bash
dotnet run --project IPS.fsproj [options] input-file
```

### Options

- `-o <file>`: Specify output file
- `-v`: Enable verbose output
- `-h, --help`: Display help message

## Example

```bash
# Compile a program to an executable
dotnet run --project IPS.fsproj -v examples/test.mylang -o test

# Run the compiled program
./test
```

## Language Features

- Variables and assignments
- Integer, string, and boolean literals
- Arithmetic and comparison operators
- Functions with parameters and return values
- Control flow: if-else, while loops
- Built-in print function

## Example Program

```
// A simple program
function main() {
  var x = 10;
  var y = 20;
  var result = x + y;
  print(result);
  return 0;
}
```

## Compilation Process

1. Source code is tokenized by the lexer
2. Tokens are parsed into an Abstract Syntax Tree
3. AST is type-checked for semantic correctness
4. LLVM IR code is generated from the validated AST
5. LLVM IR is compiled to native machine code using clang
6. Resulting executable is produced

The compiler now produces native executables directly by generating LLVM IR as an intermediate representation and then using the LLVM toolchain to compile it to machine code.