from customtkinter import *
import tkinter

def inventory_content(content_frame):
    # Clear existing widgets
    for widget in content_frame.winfo_children():
        widget.destroy()
    
    # Create a container frame for centered content
    center_frame = CTkFrame(content_frame, fg_color="transparent")
    center_frame.place(relx=0.5, rely=0.5, anchor=tkinter.CENTER)
    
    # Create title label
    title_label = CTkLabel(
        center_frame,
        text="Inventory Page",
        font=("Public Sans", 24, "bold"),
        text_color="black"
    )
    title_label.pack(pady=20)

    # Additional content specific to the inventory page
    content_label = CTkLabel(
        center_frame,
        text="Inventory content goes here",
        font=("Public Sans", 16),
        text_color="black"
    )
    content_label.pack(pady=10)