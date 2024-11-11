from customtkinter import *
import tkinter

def create_main_window():
    main_app = CTk()
    main_app.title("OSPoS")

    # Maximize the window on startup
    main_app.state('zoomed')  # For Windows

    # Allow resizing the window
    main_app.resizable(True, True)

    main_frame = CTkFrame(master=main_app, fg_color="#141b35")
    main_frame.pack(fill="both", expand=True)

    CTkLabel(master=main_frame, text="Welcome to the Main Application!", font=("Arial", 20)).place(relx=0.5, rely=0.5, anchor=tkinter.CENTER)

    main_app.mainloop()

# Example usage (assuming this code is in a file named "main.py")
if __name__ == "__main__":
    create_main_window()