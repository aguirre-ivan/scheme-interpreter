# Scheme Interpreter

Interprete de Scheme realizado para la materia de [Lenguajes Formales 95.48](http://wiki.foros-fiuba.com.ar/materias:75:14), correspondiente a la cursada 2021.

Con este trabajo práctico, se espera adquirir conocimientos profundos sobre el proceso de interpretación de programas y el funcionamiento de los intérpretes de lenguajes de programación y que, a la vez, se ponga en práctica los conceptos del paradigma de *Programación Funcional* vistos en la materia.

# Interprete para completar

Una vez completado el [archivo base](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/scheme_base.clj), se puede correr [scheme.clj](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/scheme.clj) de la siguiente forma:

```
user => (load-file "scheme.clj")
user => (repl)
```

# Pruebas

Debe correr las pruebas como se indica en el [enunciado](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/EnunciadoScheme2021.pdf).

- Deberá poder cargarse el archivo [jarras.scm](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/jarras.scm) que resuelve el problema de obtener 4  litros de líquido utilizando dos jarras lisas sin escala, una de 5 litros y otra de 8 litros.
Se corre de la siguiente manera:
```
Para cargar:
> (load "jarras")
Para ejecutar:
> (breadth-first bc)
Por ejemplo, probar para pasar de (0 0) (0 4)
```

- Al cargar el archivo [demo.scm](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/demo.scm) debe cumplir con la salida esperada.

# Proyecto con Leiningen

En el archivo [scheme-interpreter.zip](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/scheme-interpreter.zip) se encuentra el trabajo realizado siguiendo los pasos para crear un proyecto en Clojure usando [Leiningen](https://leiningen.org), con sus respectivos tests.
