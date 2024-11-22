import sqlite3
import hashlib
from PIL import Image
from io import BytesIO


DB_ADMIN = "app_data.db"

def initialize_db():
    """Initialize the database, creating tables if they don't already exist or modifying if necessary."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            
            # Create the admin table if it does not exist
            c.execute('''CREATE TABLE IF NOT EXISTS admin (
                            username TEXT UNIQUE,
                            password_hash TEXT,
                            pin_hash TEXT,
                            plaintext_password TEXT,
                            plaintext_pin TEXT
                        )''')
            
            # Add profile_image_path column if it doesn't exist
            c.execute('''ALTER TABLE admin ADD COLUMN profile_image_path TEXT''')
            
            conn.commit()
            print("Database initialized or already exists.")
    except sqlite3.OperationalError as e:
        # Catch an error if column already exists and provide relevant feedback
        if "duplicate column name" in str(e).lower():
            print("Column profile_image_path already exists.")
        else:
            print(f"Error initializing the database: {e}")
    except sqlite3.Error as e:
        print(f"Error initializing the database: {e}")



def is_admin_setup():
    """Check if admin credentials have been set up."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute("SELECT COUNT(*) FROM admin")
            result = c.fetchone()[0]
        return result > 0
    except sqlite3.Error as e:
        print(f"Error checking admin setup: {e}")
        return False


def setup_admin(username, password, pin):
    """Setup admin credentials (username, password, and pin)."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            # Hash the password and pin
            hashed_password = hashlib.sha256(password.encode()).hexdigest()
            hashed_pin = hashlib.sha256(pin.encode()).hexdigest()
            
            # Insert both hashed and plaintext passwords and PINs
            c.execute("INSERT INTO admin (username, password_hash, pin_hash, plaintext_password, plaintext_pin) VALUES (?, ?, ?, ?, ?)",
                      (username, hashed_password, hashed_pin, password, pin))
            conn.commit()
            print("Admin credentials set up successfully.")
    except sqlite3.IntegrityError:
        print("Error: Username already exists. Please choose a different username.")
    except sqlite3.Error as e:
        print(f"Error setting up admin: {e}")


def verify_login(username, password):
    """Verify the username and password during login."""
    hashed_password = hashlib.sha256(password.encode()).hexdigest()  # Hash input password
    print(f"Verifying login for username: {username}, hashed password: {hashed_password}")  # Debugging line

    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute("SELECT * FROM admin WHERE username = ? AND password_hash = ?", (username, hashed_password))
            result = c.fetchone()  # Fetch the user data
            print(f"Database query result: {result}")  # Debugging line

        return result is not None  # Return True if found, False otherwise
    except sqlite3.Error as e:
        print(f"Error verifying login: {e}")
        return False




def verify_pin_hash(input_pin):
    """Verify the PIN during PIN entry step."""
    hashed_pin = hashlib.sha256(input_pin.encode()).hexdigest()  # Hash input PIN

    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute("SELECT * FROM admin WHERE pin_hash = ?", (hashed_pin,))
            result = c.fetchone()  # Fetch user data based on hashed pin
        return result is not None  # Return True if found, False otherwise
    except sqlite3.Error as e:
        print(f"Error verifying PIN: {e}")
        return False


def fetch_admin_credentials():
    """Fetch the most recent admin username, actual password, and PIN from the database."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            # Fetch the most recently updated admin credentials
            c.execute("SELECT username, plaintext_password, plaintext_pin FROM admin ORDER BY ROWID DESC LIMIT 1")
            result = c.fetchone()
            if result:
                username, password, pin = result  # Retrieve the values
                return {"username": username, "password": password, "pin": pin}
            else:
                print("No admin credentials found.")
                return None
    except sqlite3.Error as e:
        print(f"Error fetching admin credentials: {e}")
        return None


def update_database(new_username, new_password, new_pin):
    """Update the admin's username, password, and pin in the database."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()

            # Hash the new password and pin
            new_password_hash = hashlib.sha256(new_password.encode()).hexdigest()
            new_pin_hash = hashlib.sha256(new_pin.encode()).hexdigest()

            print(f"Hashing new password: {new_password} -> {new_password_hash}")
            print(f"Hashing new PIN: {new_pin} -> {new_pin_hash}")

            # Update the admin's credentials (overwrite the most recent admin record)
            c.execute("""
                UPDATE admin
                SET username = ?, password_hash = ?, pin_hash = ?, plaintext_password = ?, plaintext_pin = ?
                WHERE ROWID = (SELECT ROWID FROM admin ORDER BY ROWID DESC LIMIT 1)
            """, (new_username, new_password_hash, new_pin_hash, new_password, new_pin))

            conn.commit()
            print("Admin credentials updated successfully.")

            # Fetch the most recent admin credentials to confirm the update
            c.execute("SELECT * FROM admin ORDER BY ROWID DESC LIMIT 1")
            result = c.fetchone()
            print(f"Most recent admin record: {result}")  # This will show the updated admin credentials

    except sqlite3.Error as e:
        print(f"Error updating admin credentials: {e}")


def save_profile_image_to_db(username, file_path):
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute('''
                UPDATE admin
                SET profile_image_path = ?
                WHERE username = ?
            ''', (file_path, username))
            conn.commit()  # Save changes to the database
            print(f"Profile image path '{file_path}' saved for user '{username}'.")
    except sqlite3.Error as e:
        print(f"Error saving profile image to the database: {e}")



def retrieve_profile_image_from_db(username):
    """Retrieve the profile image from the username table."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute('''
                SELECT profile_image_path
                FROM admin
                WHERE username = ?
            ''', (username,))
            result = c.fetchone()

        if result:
            img_path = result[0]
            if img_path:
                # Load the image from the file path if it exists
                img = Image.open(img_path)
                return img
            else:
                print("No profile image path found.")
                return None
    except sqlite3.Error as e:
        print(f"Error retrieving profile image: {e}")
        return None



# Call this function at the start of the program to initialize the DB
initialize_db()
