import sqlite3
from customtkinter import *
import tkinter
from tkinter import messagebox

DB_ADMIN = "main_data.db"

# Initialize the database
def initialize_db():
    connection = sqlite3.connect(DB_ADMIN)
    cursor = connection.cursor()
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS products (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            name TEXT NOT NULL,
            code TEXT NOT NULL,
            available INTEGER NOT NULL,
            price REAL NOT NULL
        )
    """)
    connection.commit()
    connection.close()

# Fetch all products from the database
def fetch_products():
    connection = sqlite3.connect(DB_ADMIN)
    cursor = connection.cursor()
    cursor.execute("SELECT * FROM products")
    products = cursor.fetchall()
    connection.close()

    # Convert each product tuple into a dictionary
    product_list = []
    for product in products:
        product_dict = {
            "id": product[0],
            "name": product[1],
            "code": product[2],
            "available": product[3],
            "price": product[4]
        }
        product_list.append(product_dict)

    return product_list

# Update product quantity in the database
def update_product_stock_in_db(product):
    # Update the stock in the database
    connection = sqlite3.connect(DB_ADMIN)
    cursor = connection.cursor()
    cursor.execute("UPDATE products SET available = ? WHERE id = ?", (product["available"], product["id"]))
    connection.commit()
    connection.close()


# Add a new product to the database
def add_product_to_db(name, code, available, price):
    connection = sqlite3.connect(DB_ADMIN)
    cursor = connection.cursor()
    cursor.execute("INSERT INTO products (name, code, available, price) VALUES (?, ?, ?, ?)",
                   (name, code, available, price))
    connection.commit()
    connection.close()


initialize_db()
