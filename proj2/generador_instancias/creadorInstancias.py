import random
from faker import Faker
import difflib

fake = Faker()

def generate_publish_date():
    return fake.date_this_decade()

def generate_num_pages():
    return random.randint(50, 1000)

def generate_genre():
    genres = ["Novela", "Poesia", "Cuento", "Ensayo", "Drama", "Ciencia ficcion", "Fantasia", "Misterio", "Terror",
              "Aventura", "Historico", "Biografia", "Autobiografia", "Epistolar", "Satirico", "Romance",
              "Realismo magico", "Distopia", "Elegia", "Epopeya"]
    return random.choice(genres)

def generate_copies_sold():
    return random.randint(1000, 1000000)

def generate_best_seller():
    return random.choice([True, False])

def generate_language():
    languages = ["Arabe", "Bengali", "Espanol", "Frances", "Hindi", "Ingles", "Mandarin", "Portugues", "Ruso", "Urdu", "Aleman", "Italiano", "Frances"]
    return random.choice(languages)

def generate_alive():
    return random.choice([True, False])

def generate_age():
    return random.randint(20, 90)

def get_similar_genres(genre, all_genres):
    """
    Devuelve géneros similares al género dado.
    """
    similar_genres = difflib.get_close_matches(genre, all_genres, n=2, cutoff=0.6)
    return similar_genres

def generate_nationality_language_mapping():
    nationality_language_mapping = {
        "Estadounidense": "Ingles",
        "Chino": "Mandarin",
        "Indio": "Hindi",
        "Ruso": "Ruso",
        "Brasileno": "Portugues",
        "Frances": "Frances",
        "Espanol": "Espanol",
        "Aleman": "Aleman",
        "Britanico": "Ingles",
        "Italiano": "Italiano",
        "Egipcio": "Arabe",
        "Bangladesi": "Bengali",
        "Paquistani": "Urdu",
        # Puedes agregar más gentilicios según tus necesidades
    }
    return nationality_language_mapping

def generate_author():
    alive = generate_alive()
    age = generate_age()

    nationality_language_mapping = generate_nationality_language_mapping()

    nationality = random.choice(list(nationality_language_mapping.keys()))
    language = nationality_language_mapping[nationality]

    return {
        "favorite_genre": generate_genre(),
        "name": fake.name(),
        "age": age,
        "nationality": nationality,
        "language": language,
        "books_written": set(),  # Usar un conjunto para evitar duplicados
        "alive": alive
    }

def generate_book():
    genre = generate_genre()

    # Define diccionarios de prefijos y sufijos relacionados con cada género
    genre_prefixes = {
        "Novela": ["La historia", "En busca", "El misterio"],
        "Poesia": ["Versos del alma", "Cantos de", "Rimas y"],
        "Cuento": ["Cuentos de hadas", "Historias cortas", "El viaje de"],
        "Ensayo": ["Reflexiones sobre", "Ensayos sobre", "Análisis de"],
        "Drama": ["Drama de", "Tragedia en", "Vidas entrelazadas"],
        "Ciencia ficcion": ["Viaje interestelar", "Mundos futuros", "Máquinas y"],
        "Fantasia": ["El reino de", "Aventuras mágicas de", "Criaturas de"],
        "Misterio": ["El caso de", "Investigación en", "Secretos ocultos en"],
        "Terror": ["Noches de", "Pesadillas en", "El horror de"],
        "Aventura": ["Aventuras de", "Explorando", "Viaje épico de"],
        "Historico": ["Época de", "Historias de", "Retrato histórico de"],
        "Biografia": ["Vida de", "Biografía de", "Retrato íntimo de"],
        "Autobiografia": ["Autobiografía de", "Mis memorias", "En primera persona"],
        "Epistolar": ["Cartas de", "Correspondencia de", "Comunicación entre"],
        "Satirico": ["Sátira de", "Humor en", "Crítica satírica de"],
        "Romance": ["Historia de amor en", "Amores prohibidos en", "Romance de"],
        "Realismo magico": ["Realismo mágico en", "Surrealismo de", "Lo insólito en"],
        "Distopia": ["Distopía de", "Futuro distópico de", "Colapso en"],
        "Elegia": ["Elegía por", "Lamento en", "Recuerdo de"],
        "Epopeya": ["Epopeya de", "Hazañas épicas de", "Odisea en"],
    }

    genre_suffixes = {
        "Novela": ["", "en el tiempo", "sin fin"],
        "Poesia": ["poetico", "en verso", "para el corazón"],
        "Cuento": ["infantil", "de hadas", "mágico"],
        "Ensayo": ["reflexivo", "analítico", "crítico"],
        "Drama": ["trágico", "emocional", "de conflictos"],
        "Ciencia ficcion": ["futurista", "tecnológico", "extraterrestre"],
        "Fantasia": ["mágico", "fantástico", "de criaturas"],
        "Misterio": ["misterioso", "oculto", "detectivesco"],
        "Terror": ["aterrador", "de pesadillas", "sobrenatural"],
        "Aventura": ["épico", "de exploración", "emocionante"],
        "Historico": ["histórico", "de época", "basado en hechos reales"],
        "Biografia": ["íntimo", "personal", "biográfico"],
        "Autobiografia": ["personal", "en primera persona", "autobiográfico"],
        "Epistolar": ["correspondencia", "cartas", "escrito en cartas"],
        "Satirico": ["satírico", "humorístico", "crítico"],
        "Romance": ["romántico", "de amor", "enamorados"],
        "Realismo magico": ["mágico", "surrealista", "lo insólito"],
        "Distopia": ["distópico", "futurista", "de sociedades colapsadas"],
        "Elegia": ["lamentoso", "melancólico", "en memoria de"],
        "Epopeya": ["épico", "heroico", "de hazañas"],
    }

    # Genera un título utilizando prefijos y sufijos relacionados con el género
    name = f"{random.choice(genre_prefixes.get(genre, ['']))} {random.choice(genre_suffixes.get(genre, ['']))}"

    return {
        "genre": genre,
        "language": generate_language(),
        "best_seller": generate_best_seller(),
        "copies_sold": generate_copies_sold(),
        "publish_date": generate_publish_date(),
        "name": name,
        "num_pages": generate_num_pages()
    }

# Crear libros
num_books = 20
books = [generate_book() for _ in range(num_books)]
books_aux = books

# Crear autores
num_authors = 3
authors = [generate_author() for _ in range(num_authors)]

# Asignar libros a autores
for author in authors:
    num_books = len(books)
    if num_books != 0:
        num_books_assigned = random.randint(1, num_books)  # Asignar un número aleatorio de libros al autor
        books_assigned = random.sample(books, num_books_assigned)  # Seleccionar libros aleatorios

        # Filtrar libros asignados para que sean del mismo género o géneros similares
        similar_genres = get_similar_genres(author['favorite_genre'], [book['genre'] for book in books_assigned])
        books_assigned = [book for book in books_assigned if (book['genre'] == author['favorite_genre'] or book['genre'] in similar_genres) and (book['language'] == "Ingles" or book['language'] == author['language'])]

        # Eliminar los libros asignados de la lista general de libros
        books = [book for book in books if book not in books_assigned]

        # Actualizar la lista de libros escritos por el autor
        author["books_written"].update(book["name"] for book in books_assigned)

def generate_author_manually():
    alive = input("Is the author alive? (True/False): ").lower() == 'true'
    age = int(input("Enter author's age: "))
    name = input("Enter author's name: ")
    nationality_language_mapping = generate_nationality_language_mapping()

    nationality = input("Enter author's nationality: ")
    language = nationality_language_mapping.get(nationality, "Ingles")
    favorite = input("Enter author's favorite genre: ")

    return {
        "favorite_genre": favorite,
        "name": name
        "age": age,
        "nationality": nationality,
        "language": language,
        "books_written": set(),  # Usar un conjunto para evitar duplicados
        "alive": alive
    }

def generate_book_manually():
    genre = input("Enter book genre: ")

    # Genera un título utilizando prefijos y sufijos relacionados con el género
    name = input("Enter book title: ")
    date = input("Enter book's release date: ")

    return {
        "genre": genre,
        "language": input("Enter book language: "),
        "best_seller": input("Is the book a bestseller? (True/False): ").lower() == 'true',
        "copies_sold": int(input("Enter number of copies sold: ")),
        "publish_date": generate_publish_date(),
        "name": name,
        "num_pages": int(input("Enter number of pages: "))
    }




def main():
    print("Choose an option:")
    print("1. Automatic Generation")
    print("2. Manual Entry")

    option = input("Enter your choice (1 or 2): ")

    if option == '1':
        # Automatic Generation
        num_books = int(input("Enter the number of books to generate: "))
        books = [generate_book() for _ in range(num_books)]
        books_aux = books

        num_authors = int(input("Enter the number of authors to generate: "))
        authors = [generate_author() for _ in range(num_authors)]

    elif option == '2':
        # Manual Entry
        num_books = int(input("Enter the number of books to generate manually: "))
        books = [generate_book_manually() for _ in range(num_books)]
        books_aux = books


    # Imprimir información

    for book in books_aux:
        print(f"([{book['name']}] of Libro")
        print(f"    (contieneGenero [MAIN::{str(book['genre']).replace('[','').replace(']','')}])")
        print(f"    (estaEscritoEn [MAIN::{str(book['language']).replace('[','').replace(']','')}])")
        print(f"    (best_seller {book['best_seller']})")
        print(f"    (ejemplares_vendidos {book['copies_sold']})")
        print(f"    (fecha_salida {book['publish_date']})")
        print(f"    (nombre {book['name']})")
        print(f"    (numero_paginas {book['num_pages']})")
        print(")")

    for author in authors:
        print(f"([{author['name']}] of Autor")
        print(f"    (haEscrito {' '.join([f'[MAIN::{book}]' for book in author['books_written']])})")
        print(f"    (vivo {author['alive']})")
        print(f"    (edad {author['age']})")
        print(f"    (nacionalidad \"{author['nationality']}\")")
        print(f"    (nombre {author['name']})")
        print(")")

main()
