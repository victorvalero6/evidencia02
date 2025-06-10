📖 Proyecto: Procesamiento de Recetas en Racket
Este programa está diseñado para leer y procesar 100 recetas, realizar diversas transformaciones sobre los ingredientes e instrucciones, y generar archivos HTML individuales para cada receta. Además, compara el rendimiento entre la versión secuencial y la versión paralelizada utilizando thread.

🔧 Funcionalidades principales
📂 Lee automáticamente 100 recetas desde archivos .txt.

⚙️ Escala las porciones según el valor definido en options.txt.

📌 Filtra recetas por palabra clave si se especifica.

🧪 Convierte unidades de ingredientes entre sistema métrico (gramos) y cups.

🌡 Convierte temperaturas entre Celsius y Fahrenheit en las instrucciones.

🔥 Calcula las calorías totales y por porción.

🧾 Genera un archivo .html por receta con todo el contenido transformado y estilizado mediante CSS.

⚡ Ejecuta el procesamiento de recetas en modo secuencial y paralelo, midiendo:

Tiempo total de ejecución (ms)

Speedup

Eficiencia basada en el número de hilos

📁 Archivos importantes
main.rkt → Versión secuencial y paralelizada

options.txt → Archivo de configuración (porciones, sistema de unidades, temperatura, filtro, etc.)

unidades.rkt / parser.rkt → Funciones auxiliares para conversión y análisis de texto

Carpeta data/ → Contiene las 100 recetas (Receta1.txt a Receta100.txt)

Carpeta recetasSalidaSecuencial/ → Archivos .html generados secuencialmente

Carpeta recetasSalidaParalelo/ → Archivos .html generados en paralelo

style.css → Hoja de estilos para los archivos .html

<img width="165" alt="Screenshot 2025-06-09 at 11 37 34 p m" src="https://github.com/user-attachments/assets/8de4910b-2f54-4609-b0da-18889e2a715a" />

En el archivo options.txt es donde podras modificar las recetas, otro tipo de modificacion puede ser esta: 

<img width="157" alt="Screenshot 2025-06-09 at 11 38 59 p m" src="https://github.com/user-attachments/assets/c1a7f747-d00c-487c-a393-4dd10536ee7e" />

Para correr el codigo asegura de presionar run en main.rkt.

