# Proyecto Práctico TLP 

**Participantes**
- Santiago Abelardo Salcedo Rodríguez  
- Dylan Ramírez Blanquicet  
- Juan Camilo Castaño Chavarriaga

---

## Descripción General del Proyecto

El siguiente proyecto contiene un procesamiento de un lenguaje de programación diseñado para construir un motor gráfico
que admita distintos juegos basados en un lenguaje escrito por nosotros mismos, que serán mostrados en terminal y permitirán ser interactuados.

Más información sobre la estructuración del lenguaje, la implementación y la estructura se encuentra en la Documentacion Técnica incluida en el repositorio.

---

## Arquitectura

- `main.cpp` implementa lexer, parser, y el arbol con la tabla de simbolos para el lenguaje brik.
- un `.brik` produce un `.json` semántico
- `runtime.exe` interpreta únicamente el `.json`, sin recompilar
- el motor ejecuta según las instrucciones del `.json`

Para lograr la portabilidad total, el runtime fue compilado con **linkeo estático** de la libreria Curses, la cual es la que permite la manipulacion
de la terminal según las necesidades del proyecto. Este linkeo permite saltarnos la necesidad de tener que instalar dependencias externas.

---

## Ejecución rápida sin compilación

Se entrega una carpeta que contiene:
runtime.exe
tetris.json
snake.json

Desde Windows PowerShell, dentro del directorio donde están estos archivos, se ejecuta alguno de los siguientes comandos, según el juego que se desee inicializar: 

.\runtime.exe .\tetris.json
.\runtime.exe .\snake.json

También es posible ejecutar el proyecto con los archivos dados desde el github. Tras clonar el repositorio, de nuevo ubicándose con una terminal para windows en la ubicación del proyecto, ejecute alguno de los comandos anteriormente mencionados.

---

## Compilación de un nuevo juego

En el caso de que un nuevo `.brik` fuese añadido, el procedimiento para conseguir su `.json` es muy similar al de ejecutar el proyecto.
Inicialmente se debe abrir con alguna terminal de comandos la ubicación del proyecto. Luego se escribe el comando

.\compiler.exe .\newGame.brik

el cual generará en el código un archivo `.json` nombrado `game.json` por defecto. Este nuevo `.json` puede ser usado luego en conjunción con el runtime como fue explicado anteriormente.
