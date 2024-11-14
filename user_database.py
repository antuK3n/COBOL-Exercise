import sqlite3
import hashlib

DB_ADMIN = "app_data.db"

def initialize_db():
    """Initialize the database, creating tables if they don't already exist."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            # Create the admin table to store plaintext password and PIN
            c.execute('''CREATE TABLE IF NOT EXISTS admin (
                            username TEXT UNIQUE,
                            password_hash TEXT,
                            pin_hash TEXT,
                            plaintext_password TEXT,
                            plaintext_pin TEXT
                        )''')
            conn.commit()
            print("Database initialized or already exists.")
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
            # Insert both hashed and plaintext passwords and PINs
            hashed_password = hashlib.sha256(password.encode()).hexdigest()
            hashed_pin = hashlib.sha256(pin.encode()).hexdigest()
            
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
    hashed_password = hashlib.sha256(password.encode()).hexdigest()

    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute("SELECT * FROM admin WHERE username = ? AND password_hash = ?", (username, hashed_password))
            result = c.fetchone()
        return result is not None
    except sqlite3.Error as e:
        print(f"Error verifying login: {e}")
        return False


def verify_pin_hash(input_pin):
    """Verify the PIN during PIN entry step."""
    hashed_pin = hashlib.sha256(input_pin.encode()).hexdigest()

    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute("SELECT * FROM admin WHERE pin_hash = ?", (hashed_pin,))
            result = c.fetchone()
        return result is not None
    except sqlite3.Error as e:
        print(f"Error verifying PIN: {e}")
        return False


def fetch_admin_credentials():
    """Fetch the admin username, actual password, and PIN from the database."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute("SELECT username, plaintext_password, plaintext_pin FROM admin")
            result = c.fetchone()
            if result:
                username, password, pin = result
                return {"username": username, "password": password, "pin": pin}
            else:
                print("No admin credentials found.")
                return None
    except sqlite3.Error as e:
        print(f"Error fetching admin credentials: {e}")
        return None


# Call this function at the start of the program to initialize the DB
initialize_db()