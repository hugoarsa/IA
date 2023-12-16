import codecs
import sys

def replace_zero_width_space(file_origin, file_target):
    # Leer el archivo de origen
    with codecs.open(file_origin, "r", encoding="utf-8") as file:
        text = file.read()

    # Buscar y reemplazar el carácter conflictivo
    if '\u200b' in text:
        print("Se han detectado caracteres zero-width space. Realizando los cambios...")

        # Encontrar las posiciones de los caracteres zero-width space
        positions = [(i, text[i]) for i in range(len(text)) if text[i] == '\u200b']

        # Reemplazar el carácter zero-width space
        cleaned_text = text.replace('\u200b', '')

        # Escribir el texto procesado en el archivo de destino
        with open(file_target, "w", encoding="utf-8") as file:
            file.write(cleaned_text)

        # Mostrar la posición del carácter conflictivo
        print("Se han realizado los cambios en las siguientes posiciones:")
        for pos in positions:
            line = text.count('\n', 0, pos[0]) + 1
            column = pos[0] - text.rfind('\n', 0, pos[0]) if '\n' in text[:pos[0]] else pos[0] + 1
            print(f"Carácter '{pos[1]}' en la línea {line}, columna {column}")

        print(f"Se ha creado el archivo '{file_target}' con el texto procesado.")
    else:
        print("No se han detectado caracteres zero-width space en el archivo.")

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Uso: python codigo.py --origin archivo_origen --target archivo_destino")
    else:
        origin_index = sys.argv.index("--origin") + 1
        target_index = sys.argv.index("--target") + 1

        file_origin = sys.argv[origin_index]
        file_target = sys.argv[target_index]

        replace_zero_width_space(file_origin, file_target)