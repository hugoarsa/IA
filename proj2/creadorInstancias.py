import random
from faker import Faker

fake = Faker()

def generate_publish_date():
    return fake.date_this_decade()


def generate_num_pages():
    return random.randint(50, 1000)

def generate_complexity():
    complexities = ["Low", "Medium", "High"]
    return random.choice(complexities)


def generate_genre():
    genres = ["Fiction", "Mystery", "Science Fiction", "Romance", "Thriller", "Fantasy"]
    return random.choice(genres)


def generate_narrative_style():
    styles = ["First Person", "Third Person Limited", "Epistolary", "Stream of Consciousness"]
    return random.choice(styles)


def generate_best_seller():
    return random.choice([True, False])


def generate_author():
    return {
        "name": fake.name(),
        "age": random.randint(25, 80),
        "nationality": fake.country()
    }

def generate_book():
    return {
        "publish_date": generate_publish_date(),
        "contemporary": random.choice([True, False]),
        "translated": random.choice([True, False]),
        "num_pages": generate_num_pages(),
        "complexity": generate_complexity(),
        "genre": generate_genre(),
        "narrative_style": generate_narrative_style(),
        "best_seller": generate_best_seller(),
        "author": generate_author()
    }


for _ in range(5):
    book = generate_book()
    print("\nBook:")
    for key, value in book.items():
        print(f"{key}: {value}")s
