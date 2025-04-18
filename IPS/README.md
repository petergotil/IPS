# IPS - Kompilatorprojekt

Dette er en implementering af en kompilator for kurset IPS (Implementering af Programmerings Sprog) på Københavns Universitet.

## Projektstruktur

```
IPS/
├── src/                    # Kildekode
│   ├── AST/                # Abstract Syntax Tree definitioner
│   ├── Lexer/              # Leksikalsk analyse (tokenisering)
│   ├── Parser/             # Syntaksanalyse (parsing)
│   ├── TypeChecker/        # Semantisk analyse
│   ├── CodeGen/            # Kodegenerering (LLVM IR)
│   ├── Compiler.fs         # Hoved-kompilator pipeline
│   └── Program.fs          # Kommandolinjegrænsefladen
├── tests/                  # Testfiler
├── examples/               # Eksempelprogrammer
└── IPS.fsproj              # Projektfil
```

## Komponenter

1. **Lexer**: Transformerer kildekoden til tokens
2. **Parser**: Konverterer tokens til et Abstract Syntax Tree (AST)
3. **TypeChecker**: Udfører semantisk analyse på AST'en
4. **CodeGen**: Genererer LLVM IR fra AST'en
5. **Compiler**: Styrer kompileringsprocessen og bruger clang til at kompilere LLVM IR til maskinkode

## Installation og forudsætninger

### Forudsætninger

- .NET SDK 6.0 eller nyere
- F# kompilator
- LLVM værktøjskæde (inklusive clang)

### Trin til installation

1. Klone repositoriet:
   ```bash
   git clone https://github.com/petergotil/IPS.git
   cd IPS
   ```

2. Byg projektet:
   ```bash
   dotnet build
   ```

## Anvendelse

### Kør kompilatoren

```bash
dotnet run --project IPS.fsproj [muligheder] input-fil
```

### Kommandolinjeparametre

- `-o <fil>`: Angiv output-fil
- `-v`: Aktivér udførlig output
- `-h, --help`: Vis hjælpebesked

### Eksempel på anvendelse

```bash
# Kompilér et program til en eksekverbar fil
dotnet run --project IPS.fsproj -v examples/test.mylang -o test

# Kør det kompilerede program
./test
```

## Sprogets funktioner

- Variabler og tildelinger
- Heltals-, streng- og boolske litteraler
- Aritmetiske og sammenligningsoperatorer
- Funktioner med parametre og returværdier
- Kontrolstrukturer: if-else, while-løkker
- Indbygget print-funktion

## Eksempel på et program

```
// Et simpelt program
function main() {
  var x = 10;
  var y = 20;
  var result = x + y;
  print(result);
  return 0;
}
```

## Kompileringsprocessen

1. Kildekoden tokeniseres af lexer'en
2. Tokens parses til et Abstract Syntax Tree
3. AST'en typetjekkes for semantisk korrekthed
4. LLVM IR-kode genereres fra den validerede AST
5. LLVM IR kompileres til nativ maskinkode ved hjælp af clang
6. Den resulterende eksekverbare fil produceres

## Fejlfinding

Hvis du støder på problemer under kompilering, kan du aktivere verbose-tilstand med `-v` flaget, hvilket giver mere detaljeret output om hver fase af kompileringen.

## Udvikling og bidrag

1. Fork repositoriet
2. Opret en feature branch: `git checkout -b min-nye-feature`
3. Commit dine ændringer: `git commit -am 'Tilføj en ny feature'`
4. Push til branchen: `git push origin min-nye-feature`
5. Indsend en pull request

## Test

For at køre testsuiten:

```bash
dotnet test
```

Tests er organiseret for at verificere hver komponent i kompilatoren (lexer, parser, typechecker, osv.).

## Licens

Dette projekt er licenseret under MIT-licensen - se LICENSE-filen for detaljer.