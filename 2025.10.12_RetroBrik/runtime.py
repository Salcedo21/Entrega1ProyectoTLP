# runtime.py
# Motor de juego para BrickScript (Version Final y Depurada)
# Uso: python runtime.py <archivo_juego.json>

import sys
import json
import os
import time
import random
import msvcrt

class Juego:
    def __init__(self, datos_juego):
        self.datos_juego = datos_juego
        self.tipo_juego = self.datos_juego.get('tipo_juego', 'TETRIS')
        config = self.datos_juego.get('config', {})
        self.ancho = config.get('grid_size', [10, 20])[0]
        self.alto = config.get('grid_size', [10, 20])[1]
        self.grid = [[0 for _ in range(self.ancho)] for _ in range(self.alto)]
        self.puntuacion = 0
        self.juego_terminado = False
        
        if self.tipo_juego == 'TETRIS':
            self.pieza_actual = None
            self.pieza_x, self.pieza_y, self.pieza_rotacion = 0, 0, 0
            self.velocidad_gravedad = 0.4
        
        if self.tipo_juego == 'SNAKE':
            self.serpiente_cuerpo = []
            self.serpiente_direccion = (1, 0)
            self.posicion_comida = None
            self.velocidad_gravedad = 0.15
        
        self.timer_gravedad = 0
        self.ejecutar_evento('ON_START')

    def run(self):
        tiempo_anterior = time.time()
        while not self.juego_terminado:
            delta_tiempo = time.time() - tiempo_anterior
            tiempo_anterior = time.time()
            self.manejar_input()
            self.timer_gravedad += delta_tiempo
            if self.timer_gravedad > self.velocidad_gravedad:
                self.timer_gravedad = 0
                self.ejecutar_evento('ON_TICK')
            self.dibujar()
            time.sleep(0.05)
        self.mostrar_game_over()

    def manejar_input(self):
        if msvcrt.kbhit():
            key = msvcrt.getch()
            if key == '\xe0':
                key = msvcrt.getch()
                if self.tipo_juego == 'TETRIS':
                    if key == 'H': self.ejecutar_evento('ON_KEY_UP')
                    elif key == 'P': self.ejecutar_evento('ON_KEY_DOWN')
                    elif key == 'K': self.ejecutar_evento('ON_KEY_LEFT')
                    elif key == 'M': self.ejecutar_evento('ON_KEY_RIGHT')
                elif self.tipo_juego == 'SNAKE':
                    if key == 'H': self.snake_cambiar_direccion('UP')
                    elif key == 'P': self.snake_cambiar_direccion('DOWN')
                    elif key == 'K': self.snake_cambiar_direccion('LEFT')
                    elif key == 'M': self.snake_cambiar_direccion('RIGHT')
            elif key == 'q':
                self.juego_terminado = True

    def dibujar(self):
        os.system('cls')
        grid_display = [list(fila) for fila in self.grid]
        
        if self.tipo_juego == 'TETRIS' and self.pieza_actual:
            matriz_pieza = self.pieza_actual[self.pieza_rotacion]
            for y_offset, fila in enumerate(matriz_pieza):
                for x_offset, celda in enumerate(fila):
                    if celda == 1:
                        if self.pieza_y + y_offset < len(grid_display) and self.pieza_x + x_offset < len(grid_display[0]):
                           grid_display[self.pieza_y + y_offset][self.pieza_x + x_offset] = 2

        if self.tipo_juego == 'SNAKE':
            for i, segmento in enumerate(self.serpiente_cuerpo):
                x, y = segmento
                if 0 <= y < self.alto and 0 <= x < self.ancho:
                    grid_display[y][x] = 3 if i == 0 else 2
            if self.posicion_comida:
                x, y = self.posicion_comida
                if 0 <= y < self.alto and 0 <= x < self.ancho:
                    grid_display[y][x] = 4

        buffer_pantalla = ["#" * (self.ancho * 2 + 4)]
        for y in range(self.alto):
            linea = "# "
            for x in range(self.ancho):
                celda = grid_display[y][x]
                if celda == 0: linea += "  "
                elif celda == 1: linea += "[]"
                elif celda == 2: linea += "[]"
                elif celda == 3: linea += "OO"
                elif celda == 4: linea += "@@"
            linea += " #"
            if y == 2: linea += "    PUNTUACION: " + str(self.puntuacion)
            if y == 4: linea += "    CONTROLES:"
            if y == 5: linea += "     Flechas"
            if y == 6: linea += "     'q': Salir"
            buffer_pantalla.append(linea)
        buffer_pantalla.append("#" * (self.ancho * 2 + 4))
        print ("\n".join(buffer_pantalla))

    def ejecutar_evento(self, nombre_evento):
        if nombre_evento in self.datos_juego['events']:
            for accion in self.datos_juego['events'][nombre_evento]:
                verbo, objeto = accion.get('accion'), accion.get('objeto')
                
                if verbo == 'INCREASE_SCORE': self.puntuacion += int(objeto)
                if verbo == 'GAME_OVER': self.juego_terminado = True

                if self.tipo_juego == 'TETRIS':
                    if verbo == 'SPAWN': self.tetris_spawn_pieza()
                    if verbo == 'MOVE': self.tetris_mover_pieza(accion['params'][0])
                    if verbo == 'ROTATE': self.tetris_rotar_pieza()
                
                if self.tipo_juego == 'SNAKE':
                    if verbo == 'SPAWN' and objeto == 'PLAYER': self.snake_spawn_jugador(accion)
                    if verbo == 'SPAWN' and objeto == 'FOOD': self.snake_spawn_comida()
                    if verbo == 'MOVE' and objeto == 'PLAYER': self.snake_mover_jugador()
                    if verbo == 'GROW': self.snake_crecer()

    def tetris_spawn_pieza(self):
        nombre_pieza = random.choice(self.datos_juego['shapes'].keys())
        self.pieza_actual = self.datos_juego['shapes'][nombre_pieza]
        self.pieza_x, self.pieza_y, self.pieza_rotacion = self.ancho / 2 - 2, 0, 0
        if self.tetris_verificar_colision(self.pieza_x, self.pieza_y, self.pieza_rotacion):
            self.juego_terminado = True

    def tetris_mover_pieza(self, direccion):
        if not self.pieza_actual: return
        dx, dy = 0, 0
        if direccion == 'LEFT': dx = -1
        elif direccion == 'RIGHT': dx = 1
        elif direccion == 'DOWN': dy = 1
        if not self.tetris_verificar_colision(self.pieza_x + dx, self.pieza_y + dy, self.pieza_rotacion):
            self.pieza_x += dx
            self.pieza_y += dy
        elif dy > 0:
            self.tetris_fijar_pieza()

    def tetris_rotar_pieza(self):
        if not self.pieza_actual: return
        nueva_rotacion = (self.pieza_rotacion + 1) % len(self.pieza_actual)
        if not self.tetris_verificar_colision(self.pieza_x, self.pieza_y, nueva_rotacion):
            self.pieza_rotacion = nueva_rotacion

    def tetris_fijar_pieza(self):
        matriz_pieza = self.pieza_actual[self.pieza_rotacion]
        for y_offset, fila in enumerate(matriz_pieza):
            for x_offset, celda in enumerate(fila):
                if celda == 1:
                    if 0 <= self.pieza_y + y_offset < self.alto and 0 <= self.pieza_x + x_offset < self.ancho:
                        self.grid[self.pieza_y + y_offset][self.pieza_x + x_offset] = 1
        self.pieza_actual = None
        self.tetris_limpiar_lineas()
        self.ejecutar_evento('ON_START')

    def tetris_verificar_colision(self, x, y, rotacion):
        if not self.pieza_actual: return False
        matriz_pieza = self.pieza_actual[rotacion]
        for y_offset, fila in enumerate(matriz_pieza):
            for x_offset, celda in enumerate(fila):
                if celda == 1:
                    nuevo_x, nuevo_y = x + x_offset, y + y_offset
                    if not (0 <= nuevo_x < self.ancho and 0 <= nuevo_y < self.alto and self.grid[nuevo_y][nuevo_x] == 0):
                        return True
        return False

    def tetris_limpiar_lineas(self):
        nuevo_grid = [fila for fila in self.grid if not all(fila)]
        lineas_limpias = self.alto - len(nuevo_grid)
        if lineas_limpias > 0:
            self.grid = [[0] * self.ancho for _ in range(lineas_limpias)] + nuevo_grid
            for _ in range(lineas_limpias): self.ejecutar_evento('ON_LINE_CLEAR')
    
    def snake_spawn_jugador(self, accion):
        coords = accion['params'][0] if accion['params'] else [self.ancho / 2, self.alto / 2]
        self.serpiente_cuerpo = [(coords[0], coords[1])]
        self.serpiente_direccion = (1, 0)
        
    def snake_spawn_comida(self):
        while True:
            x, y = random.randint(0, self.ancho - 1), random.randint(0, self.alto - 1)
            if (x, y) not in self.serpiente_cuerpo:
                self.posicion_comida = (x, y)
                break
                
    def snake_mover_jugador(self):
        if not self.serpiente_cuerpo: return
        cabeza_x, cabeza_y = self.serpiente_cuerpo[0]
        dir_x, dir_y = self.serpiente_direccion
        nueva_cabeza = (cabeza_x + dir_x, cabeza_y + dir_y)

        if not (0 <= nueva_cabeza[0] < self.ancho and 0 <= nueva_cabeza[1] < self.alto):
            self.ejecutar_evento('ON_COLLISION_WALL')
            return
            
        if nueva_cabeza in self.serpiente_cuerpo[:-1]:
            self.ejecutar_evento('ON_COLLISION_SELF')
            return

        self.serpiente_cuerpo.insert(0, nueva_cabeza)
        
        if nueva_cabeza == self.posicion_comida:
            self.ejecutar_evento('ON_EAT_FOOD')
        else:
            self.serpiente_cuerpo.pop()

    def snake_cambiar_direccion(self, direccion):
        if direccion == 'UP' and self.serpiente_direccion[1] != 1:
            self.serpiente_direccion = (0, -1)
        elif direccion == 'DOWN' and self.serpiente_direccion[1] != -1:
            self.serpiente_direccion = (0, 1)
        elif direccion == 'LEFT' and self.serpiente_direccion[0] != 1:
            self.serpiente_direccion = (-1, 0)
        elif direccion == 'RIGHT' and self.serpiente_direccion[0] != -1:
            self.serpiente_direccion = (1, 0)

    def snake_crecer(self):
        pass

    def mostrar_game_over(self):
        os.system('cls')
        print ("\n\n" + " " * 10 + "=================\n" + " " * 10 + "  JUEGO TERMINADO\n" + " " * 10 + "=================\n" + " " * 10 + " Puntuacion Final: " + str(self.puntuacion) + "\n\n" + " " * 10 + "Presiona cualquier tecla para salir.")
        msvcrt.getch()

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print ("Uso: python runtime.py <archivo_juego.json>")
        sys.exit(1)
    archivo_juego = sys.argv[1]
    try:
        with open(archivo_juego, 'r') as f:
            datos_juego = json.load(f)
    except IOError:
        print ("Error: No se pudo encontrar el archivo " + archivo_juego)
        sys.exit(1)
    juego = Juego(datos_juego)
    juego.run()