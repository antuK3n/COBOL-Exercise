import sqlite3
import hashlib

DB_ADMIN = "app_data.db"

def initialize_db():
    """Initialize the database, creating tables if they don't already exist."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            # Create the admin table if it doesn't exist
            c.execute('''CREATE TABLE IF NOT EXISTS admin (
                            username TEXT UNIQUE,
                            password_hash TEXT,
                            pin_hash TEXT
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
    hashed_password = hashlib.sha256(password.encode()).hexdigest()  # Could use a stronger method here
    hashed_pin = hashlib.sha256(pin.encode()).hexdigest()  # Same here for the pin

    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute("INSERT INTO admin (username, password_hash, pin_hash) VALUES (?, ?, ?)",
                      (username, hashed_password, hashed_pin))
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

# Call this function at the start of the program to initialize the DB
initialize_db()