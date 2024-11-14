from customtkinter import *
import tkinter
from PIL import Image, ImageDraw, ImageTk
from tkinter import filedialog
import pywinstyles
from user_database import fetch_admin_credentials

def profile_content(content_frame):
    # Clear existing widgets
    for widget in content_frame.winfo_children():
        widget.place_forget()
    
    # Define frame properties
    frame_properties = {
        "master": content_frame,
        "fg_color": "#e0e0e0",
        "corner_radius": 20
    }
    

    widgets = []

    # Create and place frames
    top_frame = CTkFrame(**frame_properties)
    top_frame.place(relx=0.495, rely=0.06, anchor=tkinter.CENTER, relwidth=0.965, relheight=0.08)
    widgets.append(top_frame)
    
    profile_frame = CTkFrame(**frame_properties)
    profile_frame.place(relx=0.34, rely=0.51, anchor=tkinter.CENTER, relwidth=0.65, relheight=0.28)
    widgets.append(profile_frame)
    
    userpass_frame = CTkFrame(**frame_properties)
    userpass_frame.place(relx=0.34, rely=0.81, anchor=tkinter.CENTER, relwidth=0.65, relheight=0.28)
    widgets.append(userpass_frame)

    runtime_frame = CTkFrame(master=content_frame, fg_color="#c8f1c5", corner_radius=20)
    runtime_frame.place(relx=0.825, rely=0.384, anchor=tkinter.CENTER, relwidth=0.298, relheight=0.53)
    widgets.append(runtime_frame)

    logindate_frame = CTkFrame(master=content_frame, fg_color="#b4d2ee", corner_radius=20)
    logindate_frame.place(relx=0.825, rely=0.81, anchor=tkinter.CENTER, relwidth=0.298, relheight=0.28)
    widgets.append(logindate_frame)
    
    # Add labels and separators
    label_properties = {
        "font": ("Public sans", 25, "bold"),
        "text_color": "black"
    }

    detail_properties = {
        "font": ("Public sans", 18, "bold"),
        "text_color": "black",
    }

    input_properties = {
        "font": ("Public sans", 18),
        "text_color": "black",
    }

    view_profile_label = CTkLabel(top_frame, text="VIEW PROFILE", **label_properties)
    view_profile_label.place(relx=0.015, rely=0.45, anchor=tkinter.W)
    widgets.append(view_profile_label)

    profile_details_label = CTkLabel(profile_frame, text="Profile Details", **label_properties)
    profile_details_label.place(relx=0.016, rely=0.11, anchor=tkinter.W)
    widgets.append(profile_details_label)
    
    # Apply a CTkImage named Edit.png
    edit_profile = CTkImage(light_image=Image.open("Edit.png"), dark_image=Image.open("Edit.png"), size=(50, 50))
    edit_profile_label = CTkLabel(profile_frame, image=edit_profile, text="", fg_color="transparent")
    edit_profile_label.place(relx=0.98, rely=0.11, anchor=tkinter.E)
    widgets.append(edit_profile_label)
    
    profile_separator = CTkFrame(profile_frame, fg_color="#b6b9bd", height=2)
    profile_separator.place(relx=0.001, rely=0.23, relwidth=0.997, anchor=tkinter.W)
    widgets.append(profile_separator)

    edit_profile_text_label = CTkLabel(profile_frame, text="Edit Profile", **detail_properties)
    edit_profile_text_label.place(relx=0.878, rely=0.3, anchor=tkinter.W)
    widgets.append(edit_profile_text_label)

    name_label = CTkLabel(profile_frame, text="Name:", **detail_properties)
    name_label.place(relx=0.1, rely=0.4, anchor=tkinter.W)
    widgets.append(name_label)

    name_value_label = CTkLabel(profile_frame, text="Cyrus Severino", **input_properties)
    name_value_label.place(relx=0.3, rely=0.4, anchor=tkinter.W)
    widgets.append(name_value_label)

    email_label = CTkLabel(profile_frame, text="Email Address:", **detail_properties)
    email_label.place(relx=0.1, rely=0.6, anchor=tkinter.W)
    widgets.append(email_label)

    email_value_label = CTkLabel(profile_frame, text="cyrusgab828@gmail.com", **input_properties)
    email_value_label.place(relx=0.3, rely=0.6, anchor=tkinter.W)
    widgets.append(email_value_label)

    contact_label = CTkLabel(profile_frame, text="Contact Number:", **detail_properties)
    contact_label.place(relx=0.1, rely=0.8, anchor=tkinter.W)
    widgets.append(contact_label)

    contact_value_label = CTkLabel(profile_frame, text="09199938230", **input_properties)
    contact_value_label.place(relx=0.3, rely=0.8, anchor=tkinter.W)
    widgets.append(contact_value_label)
   
    account_details_label = CTkLabel(userpass_frame, text="Account Details", **label_properties)
    account_details_label.place(relx=0.016, rely=0.11, anchor=tkinter.W)
    widgets.append(account_details_label)

    # Apply a CTkImage named Edit.png
    edit_userpass = CTkImage(light_image=Image.open("Edit.png"), dark_image=Image.open("Edit.png"), size=(50, 50))
    edit_userpass_label = CTkLabel(userpass_frame, image=edit_userpass, text="", fg_color="transparent")
    edit_userpass_label.place(relx=0.98, rely=0.11, anchor=tkinter.E)
    widgets.append(edit_userpass_label)

    userpass_separator = CTkFrame(userpass_frame, fg_color="#b6b9bd", height=2)
    userpass_separator.place(relx=0.001, rely=0.23, relwidth=0.997, anchor=tkinter.W)
    widgets.append(userpass_separator)

    edit_account_label = CTkLabel(userpass_frame, text="Edit Account", **detail_properties)
    edit_account_label.place(relx=0.864, rely=0.3, anchor=tkinter.W)
    widgets.append(edit_account_label)

    admin_credentials = fetch_admin_credentials()

    username = admin_credentials['username']
    password = admin_credentials['password']
    pin = admin_credentials['pin']  

    username_label = CTkLabel(userpass_frame, text="Username:", **detail_properties)
    username_label.place(relx=0.1, rely=0.4, anchor=tkinter.W)
    widgets.append(username_label)

    username_value_label = CTkLabel(userpass_frame, text=f"{username}", **input_properties)
    username_value_label.place(relx=0.3, rely=0.4, anchor=tkinter.W)
    widgets.append(username_value_label)
    
    password_label = CTkLabel(userpass_frame, text="Password:", **detail_properties)
    password_label.place(relx=0.1, rely=0.60, anchor=tkinter.W)
    widgets.append(password_label)

    password_value_label = CTkLabel(userpass_frame, text="*" * len(f"{password}"), **input_properties)
    password_value_label.place(relx=0.3, rely=0.60, anchor=tkinter.W)
    widgets.append(password_value_label)

    pin_label = CTkLabel(userpass_frame, text="Pin Code:", **detail_properties)
    pin_label.place(relx=0.1, rely=0.80, anchor=tkinter.W)
    widgets.append(pin_label)

    pin_value_label = CTkLabel(userpass_frame, text="*" * len(f"{pin}"), **input_properties)
    pin_value_label.place(relx=0.3, rely=0.80, anchor=tkinter.W)
    widgets.append(pin_value_label)


    # Add the circular image placeholder after creating the frames
    def create_circular_image(size=(150, 150)):
        # Create base circular mask with anti-aliasing for smoother edges
        mask = Image.new('L', (size[0] * 2, size[1] * 2), 0)
        draw = ImageDraw.Draw(mask)
        draw.ellipse((0, 0, size[0] * 2, size[1] * 2), fill=255)
        mask = mask.resize(size, Image.Resampling.LANCZOS)

        # Create placeholder with grey background
        placeholder = Image.new('RGB', size, '#e0e0e0')
        draw = ImageDraw.Draw(placeholder)
                
        # Apply circular mask
        output = Image.new('RGB', size, (0, 0, 0, 0))
        output.paste(placeholder, (0, 0))
        output.putalpha(mask)

        # Convert to CTkImage
        return CTkImage(light_image=output, dark_image=output, size=size)

    def update_profile_picture(event):
        nonlocal profile_image_label  # Add this to access the label
        
        file_path = filedialog.askopenfilename(
            filetypes=[("Image files", "*.png *.jpg *.jpeg *.gif *.bmp")]
        )
        
        if file_path:  # Only proceed with deletion and update if a file was selected
            # Destroy the existing label to remove the old image
            profile_image_label.destroy()
            
            try:
                # Open and resize the selected image
                img = Image.open(file_path)
                if img.mode == 'RGBA':
                    img = img.convert('RGB')
                
                # Resize maintaining aspect ratio
                img.thumbnail((150, 150))
                
                # Create a new image with the exact size we want
                background = Image.new('RGB', (150, 150), '#f0f0f0')
                
                # Calculate position to center the image
                offset = ((150 - img.width) // 2, (150 - img.height) // 2)
                background.paste(img, offset)
                
                # Create circular mask with anti-aliasing for smoother edges
                mask = Image.new('L', (150 * 2, 150 * 2), 0)
                draw = ImageDraw.Draw(mask)
                draw.ellipse((0, 0, 150 * 2, 150 * 2), fill=255)
                mask = mask.resize((150, 150), Image.Resampling.LANCZOS)
                
                # Apply the mask to the image
                output = Image.new('RGB', (150, 150), '#f0f0f0')
                output.paste(background, (0, 0))
                output = Image.composite(output, Image.new('RGB', (150, 150), '#f0f0f0'), mask)
                
                # Convert to CTkImage
                photo = CTkImage(light_image=output, dark_image=output, size=(150, 150))
                
            except Exception as e:
                print(f"Error loading image: {e}")
                photo = create_circular_image()
            
            # Create new label with the new image
            profile_image_label = CTkLabel(
                content_frame,
                image=photo,
                text=""
            )
            profile_image_label.image = photo  # Keep reference
            profile_image_label.place(relx=0.07, rely=0.22, anchor=tkinter.CENTER)
            profile_image_label.bind("<Button-1>", update_profile_picture)
            profile_image_label.configure(cursor="hand2")

    # Create and place the profile image label
    initial_photo = create_circular_image()
    profile_image_label = CTkLabel(
        content_frame,
        image=initial_photo,
        text=""
    )
    profile_image_label.image = initial_photo  # Keep reference
    profile_image_label.place(relx=0.07, rely=0.22, anchor=tkinter.CENTER)
    widgets.append(profile_image_label)
    profile_image_label.bind("<Button-1>", update_profile_picture)
    profile_image_label.configure(cursor="hand2")

    name_label = CTkLabel(content_frame, text="Cyrus Severino", font=("Public Sans", 30, "bold"), text_color="black")
    name_label.place(relx=0.14, rely=0.188, anchor=tkinter.W)
    widgets.append(name_label)

    role_label = CTkLabel(content_frame, text="Administrator", font=("Public Sans", 22), text_color="black")
    role_label.place(relx=0.14, rely=0.23, anchor=tkinter.W)
    widgets.append(role_label)

    status_label = CTkLabel(content_frame, text="ACTIVE", font=("Public Sans", 15), text_color="#0dce31")
    status_label.place(relx=0.14, rely=0.262, anchor=tkinter.W)
    widgets.append(status_label)

    set_photo_label = CTkLabel(content_frame, text="Set Photo", font=("Public Sans", 16, "bold"), text_color="black")
    set_photo_label.place(relx=0.047, rely=0.315, anchor=tkinter.W)
    widgets.append(set_photo_label)

    platform_label = CTkLabel(content_frame, text="Platform", font=("Public Sans", 30, "bold"), text_color="black")
    platform_label.place(relx=0.50, rely=0.188, anchor=tkinter.W)
    widgets.append(platform_label)

    os_pos_label = CTkLabel(content_frame, text="OS POS", font=("Public Sans", 22), text_color="black")
    os_pos_label.place(relx=0.50, rely=0.23, anchor=tkinter.W)
    widgets.append(os_pos_label)
    
    return widgets