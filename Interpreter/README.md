This is a document with description of final version of language (Sollang).

The grammar of language was changed (syntax is very similar to the one in declaration, but some adjustments were made
e.g. to functions declaration which require that return statement is at the end of function so that no return-completeness
checks are needed) due to implementation decisions.

Overview of changes in grammar in comparison to the declaration (list includes the most important changes):
  - function body structure
  - print as separate statement for easier interpret
  - return must be at the end of function
  - declarations of variables and functions must be in the beginning of a block

The grammar can be found in Sollang.cf file.

The implementation of interpreter consists of 3 files:

TypeChecker.hs - type checker of the language

Interpreter.hs - interpreter of the language (logic)

SollangInterpreter.hs - entry logic for interpreter (reading file / input according to the number
of arguments passed to execution; displaying final result)

Due to implementation decisions, the final scope slightly changed:

WAS (declaration)

  Na 15 punktów
+ 01 (trzy typy)
+ 02 (literały, arytmetyka, porównania)
+ 03 (zmienne, przypisanie)
+ 04 (print)
+ 05 (while, if)
+ 06 (funkcje lub procedury, rekurencja)
+ 07 (przez zmienną / przez wartość / in/out)
  08 (zmienne read-only i pętla for) (chose to do 7 instead of this one)
  Na 20 punktów
+ 09 (przesłanianie i statyczne wiązanie)
+ 10 (obsługa błędów wykonania)
+ 11 (funkcje zwracające wartość)
  Na 30 punktów
+ 12 (4) (statyczne typowanie)
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
  15 (2) (krotki z przypisaniem)
+ 16 (1) (break, continue)
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  18 (3) (generatory)

Razem: 25 (30 max)


IS (implementation):

  Na 15 punktów
+ 01 (trzy typy)
+ 02 (literały, arytmetyka, porównania)
+ 03 (zmienne, przypisanie)
+ 04 (print)
+ 05 (while, if)
+ 06 (funkcje lub procedury, rekurencja)
+ 07 (przez zmienną / przez wartość / in/out)
  08 (zmienne read-only i pętla for) (chose to do 7 instead of this one)
  Na 20 punktów
+ 09 (przesłanianie i statyczne wiązanie)
+ 10 (obsługa błędów wykonania)
+ 11 (funkcje zwracające wartość)
  Na 30 punktów
+ 12 (4) (statyczne typowanie)
+ 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
  15 (2) (krotki z przypisaniem)
  16 (1) (break, continue)
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
  18 (3) (generatory)

Razem: 26 ((30 max)

In general, I did not implement break/continue instructions in my language,
but during implementation of point 09 it turned out that implementing nested procedures
will result in implementing nested functions in general (in my language procedures
are simply functions with void return type). This is why the point 13 is marked.


Interpreter overview

Type checker checks the structure of program. It detects following flaws:

- bad types (e.g. 3 < "asd")

- duplicate functions within block (with respect to name) - ensures that no two functions with the same
  name are declared, so for example following will fail:

  "
    function int f() {
      function void p() {
      	return;
      }

      function int p() { <- duplicate identifier 'p'
      	return 3;
      }

      return 3;
    }
  "

  but the following will not fail:

  "
    function int f() {
      function void p() {
      	return;
      }

      function int f() { <- OK, overrides the definition of outer f
      	return 3;
      }

      return 3;
    }
  "

- duplicate variables within block (with respect to name) - similar to the functions, but considers variables

- usage of undeclared identifiers (variables, function)

- wrong arguments passing to functions (regards reference-passing of arguments). Checks that for function
  taking reference to its argument, only variables are placed in appropriate places of call to the function

- number of arguments passed to function call - ensures that every needed parameter is provided to the function call

- proper definition of main function (firstly: is defined, secondly: takes no arguments, thirdly: returns int)



The general structure of program is as follows:

vars {} <- global variables section, the variables
           are accessible throughout the program
           (unless statically overridden by local variable
           with the same name - and maybe other type)


function void f() { <- example declaration of function
	int x; <- local variables

	function void p() { <- nested function
		...
	}

	<statements> <- statements of function

    return; <- return statement - always at the end; for void-returning function is pure 'return' without any value
}

function int main() { <- main function
	f(); <- simple statement - call f

    return 0;
}



**** Running the interpreter ******

First, execute make - it will compile source code, creating binary named interpreter.

To interpret specific file, execute ./interpreter <source_code_file_name>

To interpret custom code from stdin, just execute ./interpreter; then type
the source code into terminal and send Ctrl-D comibination. The interpreter
will interpret the code.
