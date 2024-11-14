from pathlib import Path
from customtkinter import *
import tkinter
from PIL import Image
import pywinstyles
from user_database import verify_login, verify_pin_hash
from main import create_main_window
import functools

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

    rounded_box = CTkFrame(
        master=frame, 
        fg_color="#34405a", 
        corner_radius=20, 
        width=280, 
        height=320, 
        bg_color="#000001"
    )
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

    title_label = CTkLabel(
        master=rounded_box, 
        text="OS", 
        text_color="white", 
        font=("Work sans", 70, "italic")
    )
    title_label.place(relx=0.23, rely=0.159, anchor=tkinter.CENTER)

    CTkLabel(
        master=rounded_box, 
        text="POS", 
        text_color="white", 
        font=("Public sans", 65, "bold")
    ).place(relx=0.66, rely=0.15, anchor=tkinter.CENTER)

    CTkLabel(
        master=rounded_box, 
        text="P O I N T  O F  S A L E  S Y S T E M", 
        text_color="white", 
        font=("Public sans", 14.8)
    ).place(relx=0.49, rely=0.3, anchor=tkinter.CENTER)

    input_label = CTkLabel(
        master=rounded_box, 
        text="Username", 
        text_color="white", 
        font=("Public sans", 13.5, "bold")
    )
    input_label.place(relx=0.21, rely=0.375, anchor=tkinter.CENTER)

    username_entry = CTkEntry(
        master=rounded_box, 
        width=230, 
        height=35, 
        text_color="black", 
        font=("Public sans", 12, "bold"), 
        fg_color="#cdd3df"
    )
    username_entry.place(relx=0.5, rely=0.465, anchor=tkinter.CENTER)
    username_entry.bind('<Control-BackSpace>', handle_ctrl_backspace)

    password_label = CTkLabel(
        master=rounded_box, 
        text="Password", 
        text_color="white", 
        font=("Public sans", 13.5, "bold")
    )
    password_label.place(relx=0.2, rely=0.565, anchor=tkinter.CENTER)

    password_entry = CTkEntry(
        master=rounded_box, 
        width=230, 
        height=35, 
        show="*", 
        text_color="black", 
        font=("Public sans", 12, "bold"), 
        fg_color="#cdd3df"
    )
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

    pin_label = CTkLabel(
        master=rounded_box, 
        text="Enter Pin Code", 
        text_color="white", 
        font=("Public sans", 13.5, "bold")
    )
    pin_label.place_forget()

    login_button = CTkButton(
        master=rounded_box, 
        text="Login", 
        fg_color="#7BA774", 
        hover_color="#6E9770", 
        text_color="black", 
        corner_radius=8, 
        width=230, 
        height=38, 
        bg_color="#000001", 
        command=on_login
    )
    login_button.place(relx=0.5, rely=0.87, anchor=tkinter.CENTER)

    submit_pin_button = CTkButton(
        master=rounded_box, 
        text="Submit", 
        fg_color="#7BA774", 
        hover_color="#6E9770", 
        text_color="black", 
        corner_radius=8, 
        width=230, 
        height=38, 
        bg_color="#000001", 
        command=verify_pin
    )
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

# Example usage (assuming this code is in a file named "main.py")
if __name__ == "__main__":
    create_login_window()