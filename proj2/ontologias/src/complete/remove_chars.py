import argparse

def remove_zero_width_space(input_file, output_file):
    # Leer el texto desde el archivo de entrada
    with open(input_file, "r", encoding="utf-8") as file:
        text = file.read()

    # Procesar el texto para eliminar los caracteres zero-width space
    cleaned_text = text.replace("\u200b", "")
    
    # Verificar si se realizaron cambios en el texto
    changes_made = cleaned_text != text

    # Escribir el texto procesado en un archivo .ttl
    with open(output_file, "w", encoding="utf-8") as file:
        file.write(cleaned_text)

    if changes_made:
        print(f"Se ha creado el archivo '{output_file}' con cambios realizados.")
    else:
        print("No se encontraron caracteres zero-width space en el archivo.")

def main():
    parser = argparse.ArgumentParser(description="Eliminar caracteres zero-width space de un archivo de texto")
    parser.add_argument("--origin", type=str, help="Ruta del archivo de entrada codificado en UTF-8")
    parser.add_argument("--target", type=str, help="Ruta del archivo de salida en el mismo directorio que el programa")
    args = parser.parse_args()

    if args.origin and args.target:
        remove_zero_width_space(args.origin, args.target)
    else:
        print("Debes especificar la ruta del archivo de origen (--origin) y la ruta del archivo de destino (--target).")

if __name__ == "__main__":
    main()