#  Proyecto Práctico TLP — Analizador de Lenguaje Brik
#  Participantes: 
#  Santiago Abelardo Salcedo Rodriguez
#  Dylan Ramirez Blanquicet
#  Juan Camilo Castaño Chavarriaga
Este proyecto esta implementado en *C++17* para un lenguaje de descripción de juegos llamado *Brik*, utilizado para definir la semántica de videojuegos como **Tetris* y *Snake*.
En repositorio se encuentra los archivos tetris.brik y snake.brik junto con main.cpp el cual contiene todo lo necesario para hacer el parser y lexer. tambien un archivo .ast con el arbol semantico


La implementacion del proyecto depende de una libreria llamada curses la cual permite manipular y crear imágenes o texto en la terminal como si fuera una pantalla 2D.
Al esta librería no hacer parte del conjunto estandar de librerias de C++, debe ser descargada de manera externa.

Para facilitar la ejecucion del proyecto proponemos multiples formas de ejecucion.

1. La forma mas sencilla es desde una carpeta que sera entregada aparte, que contiene el archivo runtime.exe, snake.json y tetris.json. 
Debe abrirse 
