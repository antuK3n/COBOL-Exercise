import csv
import sqlite3
import hashlib
from pathlib import Path
from customtkinter import *
import tkinter
from PIL import Image
import subprocess
import pywinstyles
import functools

# Database file
DB_ADMIN = "app_data.db"

# Global variable to track which step we are in (login or pin)
step = "login"  # Options: "login", "pin"

def initialize_db():
    """Initialize the SQLite database."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            c.execute('''CREATE TABLE IF NOT EXISTS admin (
                            username TEXT UNIQUE,
                            password_hash TEXT,
                            pin_hash TEXT,
                            plaintext_password TEXT,
                            plaintext_pin TEXT,
                            profile_image_path TEXT
                        )''')
            conn.commit()
            print("Database initialized.")
    except sqlite3.Error as e:
        print(f"Error initializing the database: {e}")

def verify_csv_credentials(username, password):
    """Verify username and password against the CSV file."""
    try:
        with open("USERS.csv", "r") as csv_file:
            csv_reader = csv.reader(csv_file)
            rows = list(csv_reader)

            # CSV format: name, password, pin, full_name, email, contact (row by row)
            for i in range(0, len(rows), 6):
                csv_username = rows[i][0] if len(rows[i]) > 0 else None
                csv_password = rows[i+1][0] if len(rows[i+1]) > 0 else None
                csv_pin = rows[i+2][0] if len(rows[i+2]) > 0 else "000000"

                if username == csv_username and password == csv_password:
                    return True, {"username": csv_username, "password": csv_password, "pin": csv_pin}
        return False, None
    except FileNotFoundError:
        print("USERS.csv file not found!")
        return False, None

def setup_admin(username, password, pin):
    """Add an admin to the SQLite database."""
    try:
        with sqlite3.connect(DB_ADMIN) as conn:
            c = conn.cursor()
            hashed_password = hashlib.sha256(password.encode()).hexdigest()
            hashed_pin = hashlib.sha256(pin.encode()).hexdigest()
            c.execute("INSERT INTO admin (username, password_hash, pin_hash, plaintext_password, plaintext_pin) VALUES (?, ?, ?, ?, ?)",
                      (username, hashed_password, hashed_pin, password, pin))
            conn.commit()
            print("Admin credentials added to the database.")
    except sqlite3.IntegrityError:
        print("User already exists in the database.")
    except sqlite3.Error as e:
        print(f"Error adding admin: {e}")

def verify_pin_hash(input_pin):
    """Verify the PIN during the second step."""
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

def create_login_window():
    """Create the login window using customtkinter."""
    global step

    # Create the main login window
    login_app = CTk()
    login_app.title("OSPoS")
    login_app.resizable(False, False)

    # Set desired window size before centering
    window_width = 600
    window_height = 500
    login_app.geometry(f"{window_width}x{window_height}")

    # Calculate the position to center the window on the screen
    screen_width = login_app.winfo_screenwidth()
    screen_height = login_app.winfo_screenheight()
    x = (screen_width - window_width) // 2
    y = (screen_height - window_height) // 2

    # Center the window on the screen
    login_app.geometry(f"{window_width}x{window_height}+{x}+{y}")

    # Create the main frame
    frame = CTkFrame(master=login_app, fg_color="#141b35")
    frame.pack(fill="both", expand=True)

    # Add the title label (OSPoS)
    title_label = CTkLabel(
        master=frame,
        text="OSPoS",
        font=("Work Sans", 24, "bold"),
        text_color="white"
    )
    title_label.place(relx=0.5, rely=0.1, anchor=tkinter.CENTER)

    # Load and display the logo image
    image_path = Path("Logo.png").resolve()
    if image_path.exists():
        image = CTkImage(Image.open(image_path), size=(800, 800))
        CTkLabel(master=frame, image=image, text="").place(relx=0.9, rely=0.5, anchor=tkinter.CENTER)
    else:
        print("Logo image not found!")

    # Create the rounded box for login inputs
    rounded_box = CTkFrame(
        master=frame, 
        fg_color="#34405a", 
        corner_radius=20, 
        width=280, 
        height=320, 
    )
    rounded_box.place(relx=0.5, rely=0.5, anchor=tkinter.CENTER)

    def on_login():
        username = username_entry.get()
        password = password_entry.get()

        # Check if the credentials match in the CSV
        is_valid_csv, csv_data = verify_csv_credentials(username, password)
        if is_valid_csv:
            print("User verified in CSV. Adding to database.")
            setup_admin(csv_data["username"], csv_data["password"], csv_data["pin"])
            show_pin_step()
        else:
            print("Invalid username or password.")

    def show_pin_step():
        global step
        step = "pin"
        for widget in [input_label, username_entry, password_label, password_entry, login_button]:
            widget.place_forget()

        for i, pin_entry in enumerate(pin_entries):
            pin_entry.place(relx=0.17 + (i * 0.133), rely=0.5, anchor=tkinter.CENTER)

        pin_entries[0].focus()
        pin_label.place(relx=0.5, rely=0.65, anchor=tkinter.CENTER)
        submit_pin_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)

    def verify_pin():
        pin = ''.join(pin_entry.get() for pin_entry in pin_entries)
        if verify_pin_hash(pin):
            print("PIN verified successfully.")
            login_app.destroy()  # Close the login window

            # Launch main.py
            main_script = os.path.join(os.path.dirname(__file__), "main.py")
            if os.path.exists(main_script):
                subprocess.Popen([sys.executable, main_script])
            else:
                print("main.py not found!")
        else:
            print("Invalid PIN.")
            for pin_entry in pin_entries:
                pin_entry.delete(0, tkinter.END)
            pin_entries[0].focus()

    # Username label and entry
    input_label = CTkLabel(master=rounded_box, text="Username", text_color="white", font=("Public sans", 13.5, "bold"))
    input_label.place(relx=0.21, rely=0.375, anchor=tkinter.CENTER)

    username_entry = CTkEntry(master=rounded_box, width=230, height=35, text_color="black", fg_color="#cdd3df")
    username_entry.place(relx=0.5, rely=0.465, anchor=tkinter.CENTER)

    # Password label and entry
    password_label = CTkLabel(master=rounded_box, text="Password", text_color="white", font=("Public sans", 13.5, "bold"))
    password_label.place(relx=0.2, rely=0.565, anchor=tkinter.CENTER)

    password_entry = CTkEntry(master=rounded_box, width=230, height=35, show="*", text_color="black", fg_color="#cdd3df")
    password_entry.place(relx=0.5, rely=0.65, anchor=tkinter.CENTER)

    # Login button
    login_button = CTkButton(master=rounded_box, text="Login", command=on_login, fg_color="#7BA774", hover_color="#6E9770")
    login_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)

    # PIN entry fields
    pin_entries = []
    for i in range(6):
        pin_entry = CTkEntry(master=rounded_box, width=35, height=35, text_color="black", justify="center", fg_color="#cdd3df")
        pin_entries.append(pin_entry)
        pin_entry.place_forget()

    # PIN label and submit button
    pin_label = CTkLabel(master=rounded_box, text="Enter Pin Code", text_color="white", font=("Public sans", 13.5, "bold"))
    pin_label.place_forget()

    submit_pin_button = CTkButton(master=rounded_box, text="Submit", command=verify_pin, fg_color="#7BA774", hover_color="#6E9770")
    submit_pin_button.place_forget()

    login_app.mainloop()

if __name__ == "__main__":
    initialize_db()
    create_login_window()
