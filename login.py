from pathlib import Path
from customtkinter import *
import tkinter
from PIL import Image
import pywinstyles
from database import initialize_db, is_admin_setup, setup_admin, verify_login, verify_pin_hash
from main import create_main_window
import functools
import re

# Global variable to track which step we are in (login or pin)
step = "login"  # Options: "login", "pin"

def handle_ctrl_backspace(event):
    cursor_pos = event.widget.index(tkinter.INSERT)
    text = event.widget.get()
    left_pos = cursor_pos
    while left_pos > 0 and not text[left_pos - 1].isspace():
        left_pos -= 1
    event.widget.delete(left_pos, cursor_pos)

def create_login_window():
    global step  # Track the current step
    
    login_app = CTk()
    login_app.title("OSPoS")
    login_app.resizable(False, False)

    # Set desired window size before centering
    window_width = 600
    window_height = 500
    login_app.geometry(f"{window_width}x{window_height}")
    login_app.resizable(False, False)

    # Calculate the position to center the window on the screen
    screen_width = login_app.winfo_screenwidth()
    screen_height = login_app.winfo_screenheight()
    x = (screen_width - window_width) // 2
    y = (screen_height - window_height) // 2

    # Center the window on the screen
    login_app.geometry(f"{window_width}x{window_height}+{x}+{y}")

    frame = CTkFrame(master=login_app, fg_color="#141b35")
    frame.pack(fill="both", expand=True)

    image_path = Path("Logo.png").resolve()
    if image_path.exists():
        image = CTkImage(Image.open(image_path), size=(800, 800))
        CTkLabel(master=frame, image=image, text="").place(relx=0.9, rely=0.5, anchor=tkinter.CENTER)
    else:
        print("Logo image not found!")

    rounded_box = CTkFrame(master=frame, fg_color="#34405a", corner_radius=20, width=280, height=320, bg_color="#000001")
    rounded_box.place(relx=0.5, rely=0.5, anchor=tkinter.CENTER)
    pywinstyles.set_opacity(rounded_box, color="#000001")

    def on_login():
        username = username_entry.get()
        password = password_entry.get()

        if verify_login(username, password):
            show_pin_step()
        else:
            print("Invalid username or password.")

    def on_key_release(event, current_idx):
        if event.char.isdigit():  # If the key pressed is a digit
            pin_entries[current_idx].delete(0, tkinter.END)
            pin_entries[current_idx].insert(0, event.char)
            if current_idx < len(pin_entries) - 1:
                pin_entries[current_idx + 1].focus_set()
        elif event.keysym == "BackSpace":
            if pin_entries[current_idx].get() == '' and current_idx > 0:
                pin_entries[current_idx - 1].focus_set()
            else:
                pin_entries[current_idx].delete(0, tkinter.END)
        elif event.keysym == "Return":
            verify_pin()

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
            login_app.destroy()
            create_main_window()  # Proceed to the main window
        else:
            # Clear all pin entry fields and reset focus to the first field
            for pin_entry in pin_entries:
                pin_entry.delete(0, tkinter.END)
            pin_entries[0].focus()  # Set focus to the first pin entry box


    title_label = CTkLabel(master=rounded_box, text="OS", text_color="white", font=("Work sans", 70, "italic"))
    title_label.place(relx=0.23, rely=0.159, anchor=tkinter.CENTER)
    CTkLabel(master=rounded_box, text="POS", text_color="white", font=("Public sans", 65, "bold")).place(relx=0.66, rely=0.15, anchor=tkinter.CENTER)
    CTkLabel(master=rounded_box, text="P O I N T  O F  S A L E  S Y S T E M", text_color="white", font=("Public sans", 14.8)).place(relx=0.49, rely=0.3, anchor=tkinter.CENTER)

    input_label = CTkLabel(master=rounded_box, text="Username", text_color="white", font=("Public sans", 13.5, "bold"))
    input_label.place(relx=0.21, rely=0.375, anchor=tkinter.CENTER)

    username_entry = CTkEntry(master=rounded_box, width=230, height=35, text_color="black", font=("Public sans", 12, "bold"), fg_color="#cdd3df")
    username_entry.place(relx=0.5, rely=0.465, anchor=tkinter.CENTER)
    username_entry.bind('<Control-BackSpace>', handle_ctrl_backspace)

    password_label = CTkLabel(master=rounded_box, text="Password", text_color="white", font=("Public sans", 13.5, "bold"))
    password_label.place(relx=0.2, rely=0.565, anchor=tkinter.CENTER)

    password_entry = CTkEntry(master=rounded_box, width=230, height=35, show="*", text_color="black", font=("Public sans", 12, "bold"), fg_color="#cdd3df")
    password_entry.place(relx=0.5, rely=0.65, anchor=tkinter.CENTER)
    password_entry.bind('<Control-BackSpace>', handle_ctrl_backspace)

    pin_entries = []
    for i in range(6):
        pin_entry = CTkEntry(
            master=rounded_box, 
            width=35, 
            height=35, 
            font=("Public sans", 16, "bold"), 
            fg_color="#cdd3df", 
            justify="center",
            text_color="black"
        )
        pin_entry.bind('<KeyRelease>', functools.partial(on_key_release, current_idx=i))
        pin_entries.append(pin_entry)
        pin_entry.place_forget()

    pin_label = CTkLabel(master=rounded_box, text="Enter Pin Code", text_color="white", font=("Public sans", 13.5, "bold"))
    pin_label.place_forget()

    login_button = CTkButton(master=rounded_box, text="Login", fg_color="#7BA774", hover_color="#6E9770", text_color="black", corner_radius=8, width=230, height=38, bg_color="#000001", command=on_login)
    login_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)

    submit_pin_button = CTkButton(master=rounded_box, text="Submit", fg_color="#7BA774", hover_color="#6E9770", text_color="black", corner_radius=8, width=230, height=38, bg_color="#000001", command=verify_pin)
    submit_pin_button.place_forget()

    pywinstyles.set_opacity(login_button, color="#000001")
    pywinstyles.set_opacity(submit_pin_button, color="#000001")

    def handle_enter(event):
        if step == "pin":
            verify_pin()
        else:
            on_login()

    login_app.bind('<Return>', handle_enter)
    login_app.mainloop() 

def setup_admin_window():
    global step  # Make sure we can access the global step variable

    setup_app = CTk()
    setup_app.title("Admin Setup")
    
    # Set desired window size before centering
    window_width = 600
    window_height = 500
    setup_app.geometry(f"{window_width}x{window_height}")
    setup_app.resizable(False, False)

    # Calculate the position to center the window on the screen
    screen_width = setup_app.winfo_screenwidth()
    screen_height = setup_app.winfo_screenheight()
    x = (screen_width - window_width) // 2
    y = (screen_height - window_height) // 2

    # Center the window on the screen
    setup_app.geometry(f"{window_width}x{window_height}+{x}+{y}")

    setup_frame = CTkFrame(master=setup_app, fg_color="#141b35")
    setup_frame.pack(fill="both", expand=True)

    image_path = Path("Logo.png").resolve()
    if image_path.exists():
        image = CTkImage(Image.open(image_path), size=(800, 800))
        CTkLabel(master=setup_frame, image=image, text="").place(relx=0.9, rely=0.5, anchor=tkinter.CENTER)
    else:
        print("Logo image not found!")

    setup_rounded_box = CTkFrame(master=setup_frame, fg_color="#34405a", corner_radius=20, width=280, height=320, bg_color="#000001")
    setup_rounded_box.place(relx=0.5, rely=0.5, anchor=tkinter.CENTER)
    pywinstyles.set_opacity(setup_rounded_box, color="#000001")

    def check_entries():
        """Checks if the criteria for username and password are met."""
        username = setup_username_entry.get().strip()
        password = setup_password_entry.get().strip()

        # Validate username
        username_valid = (
            len(username) >= 5 and  # At least 5 characters
            username[0].isalpha() and  # Starts with a letter
            re.match(r'^[A-Za-z0-9]*$', username)  # Only letters and numbers (no special characters)
        )

        # Validate password length
        password_valid = len(password) >= 8

        return username_valid and password_valid

    setup_title_label = CTkLabel(master=setup_rounded_box, text="OS", text_color="white", font=("Work sans", 70, "italic"))
    setup_title_label.place(relx=0.23, rely=0.159, anchor=tkinter.CENTER)
    CTkLabel(master=setup_rounded_box, text="POS", text_color="white", font=("Public sans", 65, "bold")).place(relx=0.66, rely=0.15, anchor=tkinter.CENTER)
    CTkLabel(master=setup_rounded_box, text="P O I N T  O F  S A L E  S Y S T E M", text_color="white", font=("Public sans", 14.8)).place(relx=0.49, rely=0.3, anchor=tkinter.CENTER)

    setup_input_label = CTkLabel(master=setup_rounded_box, text="Enter New Username", text_color="white", font=("Public sans", 13.5, "bold"))
    setup_input_label.place(relx=0.34, rely=0.375, anchor=tkinter.CENTER)

    setup_username_entry = CTkEntry(master=setup_rounded_box, width=230, height=35, text_color="black", font=("Public sans", 12, "bold"), fg_color="#cdd3df")
    setup_username_entry.place(relx=0.5, rely=0.465, anchor=tkinter.CENTER)
    setup_username_entry.bind('<Control-BackSpace>', handle_ctrl_backspace)

    setup_password_label = CTkLabel(master=setup_rounded_box, text="Enter New Password", text_color="white", font=("Public sans", 13.5, "bold"))
    setup_password_label.place(relx=0.34, rely=0.565, anchor=tkinter.CENTER)

    setup_password_entry = CTkEntry(master=setup_rounded_box, width=230, height=35, show="*", text_color="black", font=("Public sans", 12, "bold"), fg_color="#cdd3df")
    setup_password_entry.place(relx=0.5, rely=0.65, anchor=tkinter.CENTER)
    setup_password_entry.bind('<Control-BackSpace>', handle_ctrl_backspace)

    # Define the on_key_release function before it is used
    def on_key_release(event, current_idx):
        if event.char.isdigit():  # If the key pressed is a digit
            setup_pin_entries[current_idx].delete(0, tkinter.END)
            setup_pin_entries[current_idx].insert(0, event.char)
            if current_idx < len(setup_pin_entries) - 1:
                setup_pin_entries[current_idx + 1].focus_set()
        elif event.keysym == "BackSpace":
            if setup_pin_entries[current_idx].get() == '' and current_idx > 0:
                setup_pin_entries[current_idx - 1].focus_set()
            else:
                setup_pin_entries[current_idx].delete(0, tkinter.END)
        elif event.keysym == "Return":
            pass  # Add logic here for Return key if needed

    setup_pin_entries = []  # List to store the pin entry widgets
    for i in range(6):
        setup_pin_entry = CTkEntry(
            master=setup_rounded_box, 
            width=35, 
            height=35, 
            font=("Public sans", 16, "bold"), 
            fg_color="#cdd3df", 
            justify="center",
            text_color="black"
        )
        setup_pin_entry.bind('<KeyRelease>', functools.partial(on_key_release, current_idx=i))
        setup_pin_entries.append(setup_pin_entry)

    def show_pin_step():

        if not check_entries():
            # Optionally, display an error message to inform the user.
            warning_label = CTkLabel(master=setup_rounded_box, text="Invalid username or password", text_color="red", font=("Public sans", 12, "bold"))
            warning_label.place(relx=0.5, rely=0.75, anchor=tkinter.CENTER)
            return
    
        # Hide the username and password fields
        for widget in [setup_input_label, setup_username_entry, setup_password_label, setup_password_entry]:
            widget.place_forget()

        # Show the pin entries
        for i, pin_entry in enumerate(setup_pin_entries):
            pin_entry.place(relx=0.17 + (i * 0.133), rely=0.5, anchor=tkinter.CENTER)

        setup_pin_entries[0].focus()
        pin_label.place(relx=0.5, rely=0.65, anchor=tkinter.CENTER)
        submit_new_pin_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)
    
    setup_button = CTkButton(master=setup_rounded_box, text="Continue", fg_color="#7BA774", hover_color="#6E9770", text_color="black", corner_radius=8, width=230, height=38, bg_color="#000001", command=show_pin_step)
    setup_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)

    # Optionally, you can place a pin label and a submit button for the pin entry
    pin_label = CTkLabel(master=setup_rounded_box, text="Enter New PIN", text_color="white", font=("Public sans", 13.5, "bold"))
    pin_label.place_forget()

    # Button for submitting PIN
    submit_new_pin_button = CTkButton(
        master=setup_rounded_box,
        text="Submit PIN",
        fg_color="#7BA774",
        hover_color="#6E9770",
        text_color="black",
        corner_radius=8,
        width=230,
        height=38,
        bg_color="#000001",
        command=lambda: handle_pin_submission()  # Delegate logic to a function
    )
    submit_new_pin_button.place_forget()

    # Bind the Enter key to the submit button itself
    submit_new_pin_button.bind("<Return>", lambda event: submit_new_pin_button.invoke())

    def handle_pin_submission():
        """Handles submission logic for the PIN."""
        # Collect and process the username, password, and pin inputs
        pin_input = ''.join([entry.get() for entry in setup_pin_entries])  # Join list elements into a string
        setup_admin(
            setup_username_entry.get(), 
            setup_password_entry.get(), 
            pin_input
        )
        complete_setup()

    def complete_setup():
        """Finalizes the setup and transitions to the login window."""
        setup_app.destroy()
        create_login_window()

    def handle_enter(event):
        """Handles Enter key presses to interact with the submit button."""
        focused_widget = setup_app.focus_get()  # Get the currently focused widget

        if step == "pin":
            if focused_widget in setup_pin_entries:
                # If the focused widget is one of the PIN entry fields, submit the PIN
                submit_new_pin_button.invoke()  # Programmatically triggers the button's command
        else:
            show_pin_step()  # Move to PIN input step

    # Bind the Enter key to the handle_enter function
    setup_app.bind("<Return>", handle_enter)

    setup_app.mainloop()

def initialize():
    if not is_admin_setup():
        setup_admin_window()
    else:
        create_login_window()

if __name__ == "__main__":
    initialize()
