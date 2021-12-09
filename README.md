# Scheme Interpreter

Interprete de Scheme realizado para la materia de [Lenguajes Formales 95.48](http://wiki.foros-fiuba.com.ar/materias:75:14), correspondiente a la cursada 2021.
Con este trabajo práctico, se espera que los estudiantes adquieran conocimientos profundos sobre el proceso de interpretación de programas y el funcionamiento de los intérpretes de lenguajes de programación y que, a la vez, pongan en práctica los conecptos del paradigma de *Programación Funcional* vistos en la materia.

# Interprete para completar

Una vez completado el [archivo base](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/scheme_base.clj), se puede correr [scheme.clj](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/scheme.clj) de la siguiente forma:

```
user => (load-file "scheme.clj")
user => (repl)
```

# Pruebas

Debe correr las pruebas como se indica en el [enunciado](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/EnunciadoScheme2021.pdf)

- Deberá poder cargarse el [jarras.scm](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/jarras.scm) que resuelve el problema de obtener 4  litros de líquido utilizando dos jarras lisas sin escala, una de 5 litros y otra de 8 litros.
- Al cargar el archivo [demo.scm](https://github.com/aguirre-ivan/scheme-interpreter/blob/main/demo.scm) debe cumplir con la salida esperada.