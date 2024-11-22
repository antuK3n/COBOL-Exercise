from pathlib import Path
from customtkinter import *
import tkinter
from PIL import Image
import pywinstyles
from user_database import is_admin_setup, setup_admin
import functools
import re
from login import create_login_window

# Global variable to track which step we are in (login or pin)
step = "login"  # Options: "login", "pin"

def handle_ctrl_backspace(event):
    # Handle Ctrl+Backspace to delete the previous word
    cursor_pos = event.widget.index(tkinter.INSERT)
    text = event.widget.get()
    left_pos = cursor_pos
    while left_pos > 0 and not text[left_pos - 1].isspace():
        left_pos -= 1
    event.widget.delete(left_pos, cursor_pos)

def setup_admin_window():
    global step  # Make sure we can access the global step variable

    # Create the main setup window
    setup_app = CTk()
    setup_app.title("Admin Setup")
    setup_app.resizable(False, False)
    
    # Set desired window size before centering
    window_width = 600
    window_height = 500
    setup_app.geometry(f"{window_width}x{window_height}")

    # Calculate the position to center the window on the screen
    screen_width = setup_app.winfo_screenwidth()
    screen_height = setup_app.winfo_screenheight()
    x = (screen_width - window_width) // 2
    y = (screen_height - window_height) // 2

    # Center the window on the screen
    setup_app.geometry(f"{window_width}x{window_height}+{x}+{y}")

    # Create the main frame
    setup_frame = CTkFrame(master=setup_app, fg_color="#141b35")
    setup_frame.pack(fill="both", expand=True)

    # Load and display the logo image
    image_path = Path("Logo.png").resolve()
    if image_path.exists():
        image = CTkImage(Image.open(image_path), size=(800, 800))
        CTkLabel(master=setup_frame, image=image, text="").place(relx=0.9, rely=0.5, anchor=tkinter.CENTER)
    else:
        print("Logo image not found!")

    # Create the rounded box for setup inputs
    setup_rounded_box = CTkFrame(
        master=setup_frame, 
        fg_color="#34405a", 
        corner_radius=20, 
        width=280, 
        height=320, 
        bg_color="#000001"
    )
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

    # Create and place the title label
    setup_title_label = CTkLabel(
        master=setup_rounded_box, 
        text="OS", 
        text_color="white", 
        font=("Work sans", 70, "italic")
    )
    setup_title_label.place(relx=0.23, rely=0.159, anchor=tkinter.CENTER)

    # Create and place the POS label
    CTkLabel(
        master=setup_rounded_box, 
        text="POS", 
        text_color="white", 
        font=("Public sans", 65, "bold")
    ).place(relx=0.66, rely=0.15, anchor=tkinter.CENTER)

    # Create and place the description label
    CTkLabel(
        master=setup_rounded_box, 
        text="P O I N T  O F  S A L E  S Y S T E M", 
        text_color="white", 
        font=("Public sans", 14.8)
    ).place(relx=0.49, rely=0.3, anchor=tkinter.CENTER)

    # Create and place the username input label
    setup_input_label = CTkLabel(
        master=setup_rounded_box, 
        text="Enter New Username", 
        text_color="white", 
        font=("Public sans", 13.5, "bold")
    )
    setup_input_label.place(relx=0.34, rely=0.375, anchor=tkinter.CENTER)

    # Create and place the username entry field
    setup_username_entry = CTkEntry(
        master=setup_rounded_box, 
        width=230, 
        height=35, 
        text_color="black", 
        font=("Public sans", 12, "bold"), 
        fg_color="#cdd3df", 
        placeholder_text="Username (at least 5 characters)", 
        placeholder_text_color="grey"
    )
    setup_username_entry.place(relx=0.5, rely=0.465, anchor=tkinter.CENTER)
    setup_username_entry.bind('<Control-BackSpace>', handle_ctrl_backspace)

    # Create and place the password input label
    setup_password_label = CTkLabel(
        master=setup_rounded_box, 
        text="Enter New Password", 
        text_color="white", 
        font=("Public sans", 13.5, "bold")
    )
    setup_password_label.place(relx=0.34, rely=0.565, anchor=tkinter.CENTER)

    # Create and place the password entry field
    setup_password_entry = CTkEntry(
        master=setup_rounded_box, 
        width=230, 
        height=35, 
        show="*", 
        text_color="black", 
        font=("Public sans", 12, "bold"), 
        fg_color="#cdd3df", 
        placeholder_text="Password (at least 8 characters)", 
        placeholder_text_color="grey"
    )
    setup_password_entry.place(relx=0.5, rely=0.65, anchor=tkinter.CENTER)
    setup_password_entry.bind('<Control-BackSpace>', handle_ctrl_backspace)

    # Define the on_key_release function before it is used
    def on_key_release(event, current_idx):
        # Handle key release events for PIN entry
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

    # Create and configure the PIN entry fields
    setup_pin_entries = []  # List to store the pin entry widgets
    for i in range(6):
        setup_pin_entry = CTkEntry(
            master=setup_rounded_box, 
            width=35, 
            height=35, 
            font=("Public sans", 16, "bold"), 
            fg_color="#cdd3df", 
            justify="center",
            text_color="black",
            show="*"
        )
        setup_pin_entry.bind('<KeyRelease>', functools.partial(on_key_release, current_idx=i))
        setup_pin_entries.append(setup_pin_entry)

    def show_pin_step():
        # Show the PIN entry step
        if not check_entries():
            # Display an error message if username or password is invalid
            if not hasattr(show_pin_step, "warning_label"):
                # Create the warning label only if it doesn't already exist
                show_pin_step.warning_label = CTkLabel(
                    master=setup_rounded_box,
                    text="Invalid username or password",
                    text_color="red",
                    font=("Public sans", 12, "bold")
                )
                show_pin_step.warning_label.place(relx=0.5, rely=0.75, anchor=tkinter.CENTER)
            return  # Exit the function if credentials are invalid
        
        # Hide the username and password fields
        for widget in [setup_input_label, setup_username_entry, setup_password_label, setup_password_entry]:
            widget.place_forget()

        # Hide the warning label if it exists
        if hasattr(show_pin_step, "warning_label"):
            show_pin_step.warning_label.place_forget()  # Remove the warning label

        # Show the pin entries
        for i, pin_entry in enumerate(setup_pin_entries):
            pin_entry.place(relx=0.17 + (i * 0.133), rely=0.5, anchor=tkinter.CENTER)

        setup_pin_entries[0].focus()
        pin_label.place(relx=0.5, rely=0.65, anchor=tkinter.CENTER)
        submit_new_pin_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)
    
    # Create and place the continue button
    setup_button = CTkButton(
        master=setup_rounded_box, 
        text="Continue", 
        fg_color="#7BA774", 
        hover_color="#6E9770", 
        text_color="black", 
        corner_radius=8, 
        width=230, 
        height=38, 
        bg_color="#000001", 
        command=show_pin_step
    )
    setup_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)

    # Optionally, you can place a pin label and a submit button for the pin entry
    pin_label = CTkLabel(
        master=setup_rounded_box, 
        text="Enter New PIN", 
        text_color="white", 
        font=("Public sans", 13.5, "bold")
    )
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
        # Collect the values from all the PIN entry fields
        pin_input = ''.join([entry.get() for entry in setup_pin_entries])  # Join list elements into a string

        # Check if any of the PIN entry fields are empty
        if not all(entry.get() for entry in setup_pin_entries):
            print("Please fill in all PIN fields.")
            return  # Exit the function without processing the submission

        # Proceed with submission if all fields are filled
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
    # Initialize the setup or login window based on admin setup status
    if not is_admin_setup():
        setup_admin_window()
    else:
        create_login_window()

if __name__ == "__main__":
    initialize()
