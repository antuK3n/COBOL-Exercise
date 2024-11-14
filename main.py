from customtkinter import *
import tkinter
from PIL import Image, ImageEnhance
from profile_page import profile_content
from pos_page import pos_content
from inventory_page import inventory_content
from reports_page import reports_content
from settings_page import settings_content
from help_page import help_content

def create_main_window():
    main_app = CTk()
    main_app.title("OSPoS")
    main_app.state('zoomed')  # Maximize the window on startup
    main_app.resizable(True, True)

    main_frame = CTkFrame(master=main_app, fg_color="#141b35")
    main_frame.pack(fill="both", expand=True)

    # Top ribbon widget - Red theme
    top_frame = CTkFrame(master=main_frame, fg_color="#141b35")
    top_frame.place(relx=0.5, rely=0, anchor=tkinter.N, relwidth=1.0, relheight=0.07)

    # Create and adjust logo image brightness
    brightened_image = ImageEnhance.Brightness(Image.open("Logo.png")).enhance(3)
    logo_image = CTkImage(light_image=brightened_image, dark_image=brightened_image, size=(90, 90))

    # Add logo and text
    CTkLabel(master=top_frame, image=logo_image, text="", fg_color="transparent").place(relx=0.06, rely=0.6, anchor=tkinter.E)
    CTkLabel(master=top_frame, text="OS", text_color="white", font=("Work sans", 40, "italic")).place(relx=0.075, rely=0.6, anchor=tkinter.CENTER)
    CTkLabel(master=top_frame, text="POS", text_color="white", font=("Public sans", 36, "bold")).place(relx=0.111, rely=0.57, anchor=tkinter.CENTER)
    CTkLabel(master=top_frame, text="Admin", font=("Public Sans", 16), text_color="white").place(relx=0.9, rely=0.5, anchor=tkinter.E)

    # Left navigation widget - Dark theme
    nav_frame = CTkFrame(master=main_frame, fg_color="#141b35")
    nav_frame.place(relx=0, rely=0.55, anchor=tkinter.W, relwidth=0.15, relheight=0.96)

    # Navigation buttons and content pages
    nav_buttons = ["Profile", "POS", "Inventory", "Reports", "Settings", "Help"]
    content_pages = {"Profile": profile_content, "POS": pos_content, "Inventory": inventory_content, "Reports": reports_content, "Settings": settings_content, "Help": help_content}

    # Main content widget - Light theme
    content_frame = CTkFrame(master=main_frame, fg_color="#f0f0f0", corner_radius=30)
    content_frame.place(relx=0.58, rely=0.55, anchor=tkinter.CENTER, relwidth=0.86, relheight=0.96)

    # Function to update content based on button click
    def update_content(page_name):
        for widget in content_frame.winfo_children():
            widget.destroy()
        content_pages[page_name](content_frame)

    for i, text in enumerate(nav_buttons):
        CTkButton(master=nav_frame, text=text, font=("Public Sans", 16), fg_color="#141b35", hover_color="#1d2847", corner_radius=8, height=40, width=180, command=lambda t=text: update_content(t)).place(relx=0.5, rely=0.1 + i * 0.1, anchor=tkinter.CENTER)

    update_content("Profile")  # Initial welcome message
    main_app.mainloop()

if __name__ == "__main__":
    create_main_window()