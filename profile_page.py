from customtkinter import *
import tkinter
from PIL import Image, ImageDraw, ImageTk
from tkinter import filedialog
from user_database import fetch_admin_credentials, update_database, save_profile_image_to_db, retrieve_profile_image_from_db

def validate_pin(P):
    # Allow only digits and ensure the length does not exceed 6
    return P.isdigit() and len(P) <= 6 or P == ""

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
    



    # Create and place frames
    top_frame = CTkFrame(**frame_properties)
    top_frame.place(relx=0.495, rely=0.06, anchor=tkinter.CENTER, relwidth=0.965, relheight=0.08)

    
    profile_frame = CTkFrame(**frame_properties)
    profile_frame.place(relx=0.34, rely=0.51, anchor=tkinter.CENTER, relwidth=0.65, relheight=0.28)

    
    userpass_frame = CTkFrame(**frame_properties)
    userpass_frame.place(relx=0.34, rely=0.81, anchor=tkinter.CENTER, relwidth=0.65, relheight=0.28)
 
    runtime_frame = CTkFrame(master=content_frame, fg_color="#c8f1c5", corner_radius=20)
    runtime_frame.place(relx=0.825, rely=0.384, anchor=tkinter.CENTER, relwidth=0.298, relheight=0.53)
 

    logindate_frame = CTkFrame(master=content_frame, fg_color="#b4d2ee", corner_radius=20)
    logindate_frame.place(relx=0.825, rely=0.81, anchor=tkinter.CENTER, relwidth=0.298, relheight=0.28)

    
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


    profile_details_label = CTkLabel(profile_frame, text="Profile Details", **label_properties)
    profile_details_label.place(relx=0.016, rely=0.11, anchor=tkinter.W)

    
    # Apply a CTkImage named Edit.png
    edit_profile = CTkImage(light_image=Image.open("Edit.png"), dark_image=Image.open("Edit.png"), size=(50, 50))
    edit_profile_label = CTkLabel(profile_frame, image=edit_profile, text="", fg_color="transparent")
    edit_profile_label.place(relx=0.98, rely=0.11, anchor=tkinter.E)
    
    def hide_profile_values(event):
        print("Edit profile clicked")
        name_value_label.place_forget()
        email_value_label.place_forget()
        contact_value_label.place_forget()
        
        name_entry = CTkEntry(profile_frame, fg_color="transparent", border_width=0, width=400, **input_properties)
        name_entry.place(relx=0.294, rely=0.4, anchor=tkinter.W)
        name_entry.insert(0, "Cyrus Severino")
        name_entry.focus_set()  # Set focus to the name_entry to indicate edit mode

        
        email_entry = CTkEntry(profile_frame, fg_color="transparent", border_width=0, width=400, **input_properties)
        email_entry.place(relx=0.294, rely=0.6, anchor=tkinter.W)
        email_entry.insert(0, "cyrusgab828@gmail.com")

        
        contact_entry = CTkEntry(profile_frame, fg_color="transparent", border_width=0, width=400, **input_properties)
        contact_entry.place(relx=0.294, rely=0.8, anchor=tkinter.W)
        contact_entry.insert(0, "09199938230")


        save_button_profile = CTkButton(
            profile_frame, 
            text="Save", 
            fg_color="#7BA774",  # Updated foreground color
            hover_color="#6E9770",  # Updated hover color
            corner_radius=8, 
            width=80
        )
        save_button_profile.place(relx=0.92, rely=0.11, anchor=tkinter.E)

    
    edit_profile_label.bind("<Button-1>", hide_profile_values)
    edit_profile_label.bind("<Enter>", lambda e: edit_profile_label.configure(cursor="hand2"))
    edit_profile_label.bind("<Leave>", lambda e: edit_profile_label.configure(cursor=""))

    
    profile_separator = CTkFrame(profile_frame, fg_color="#b6b9bd", height=2)
    profile_separator.place(relx=0.001, rely=0.23, relwidth=0.997, anchor=tkinter.W)


    edit_profile_text_label = CTkLabel(profile_frame, text="Edit Profile", **detail_properties)
    edit_profile_text_label.place(relx=0.878, rely=0.3, anchor=tkinter.W)


    name_label = CTkLabel(profile_frame, text="Name:", **detail_properties)
    name_label.place(relx=0.1, rely=0.4, anchor=tkinter.W)


    name_value_label = CTkLabel(profile_frame, text="Cyrus Severino", **input_properties)
    name_value_label.place(relx=0.3, rely=0.4, anchor=tkinter.W)


    email_label = CTkLabel(profile_frame, text="Email Address:", **detail_properties)
    email_label.place(relx=0.1, rely=0.6, anchor=tkinter.W)


    email_value_label = CTkLabel(profile_frame, text="cyrusgab828@gmail.com", **input_properties)
    email_value_label.place(relx=0.3, rely=0.6, anchor=tkinter.W)


    contact_label = CTkLabel(profile_frame, text="Contact Number:", **detail_properties)
    contact_label.place(relx=0.1, rely=0.8, anchor=tkinter.W)


    contact_value_label = CTkLabel(profile_frame, text="09199938230", **input_properties)
    contact_value_label.place(relx=0.3, rely=0.8, anchor=tkinter.W)

   
    account_details_label = CTkLabel(userpass_frame, text="Account Details", **label_properties)
    account_details_label.place(relx=0.016, rely=0.11, anchor=tkinter.W)


    # Apply a CTkImage named Edit.png
    edit_userpass = CTkImage(light_image=Image.open("Edit.png"), dark_image=Image.open("Edit.png"), size=(50, 50))
    edit_userpass_label = CTkLabel(userpass_frame, image=edit_userpass, text="", fg_color="transparent")
    edit_userpass_label.place(relx=0.98, rely=0.11, anchor=tkinter.E)
    
    def hide_userpass_values(event):
        # Hide value labels
        username_value_label.place_forget()
        password_value_label.place_forget()
        pin_value_label.place_forget()

        # Disable interaction with edit button
        edit_userpass_label.unbind("<Button-1>")
        edit_userpass_label.unbind("<Enter>")
        edit_userpass_label.unbind("<Leave>")

        # Fetch the current values for username, password, and pin from labels (or database)
        current_username = username_value_label.cget("text")  # Use current label text
        current_password = password_value_label.cget("text")  # Use current label text (masked)
        current_pin = pin_value_label.cget("text")

        # Create entries for user input
        username_entry = CTkEntry(userpass_frame, fg_color="transparent", border_width=0, width=400, **input_properties)
        username_entry.place(relx=0.294, rely=0.4, anchor=tkinter.W)
        username_entry.insert(0, current_username)  # Insert the latest value from the label
        username_entry.focus_set()

        password_entry = CTkEntry(userpass_frame, fg_color="transparent", border_width=0, width=400, **input_properties)
        password_entry.place(relx=0.294, rely=0.6, anchor=tkinter.W)
        password_entry.insert(0, current_password)  # Insert the latest value (masked)

        pin_entry = CTkEntry(
            userpass_frame, 
            fg_color="transparent", 
            border_width=0, 
            width=400, 
            **input_properties,
            validate="key", 
            validatecommand=(userpass_frame.register(validate_pin), '%P')
        )
        pin_entry.place(relx=0.294, rely=0.8, anchor=tkinter.W)
        pin_entry.insert(0, current_pin)  # Clear the pin entry field

        def update_user_info():
            # Get the updated values from the entries
            new_username = username_entry.get().strip()
            new_password = password_entry.get().strip()
            new_pin = pin_entry.get().strip()

            # Validate the input values
            if not new_username or not new_password or not new_pin:
                print("All fields must be filled out.")
                return

            # Update the database with the new values (store actual values, not masked)
            update_database(new_username, new_password, new_pin)

            # Update labels with the new values (use configure to avoid AttributeError)
            username_value_label.configure(text=new_username)
            password_value_label.configure(text=new_password)  # Show the masked password
            pin_value_label.configure(text=new_pin)  # Show the masked PIN

            # Hide entries and save button, show the updated labels
            username_entry.place_forget()
            password_entry.place_forget()
            pin_entry.place_forget()
            save_button_userpass.place_forget()

            # Re-display labels with the updated text
            username_value_label.place(relx=0.3, rely=0.4, anchor=tkinter.W)
            password_value_label.place(relx=0.3, rely=0.6, anchor=tkinter.W)
            pin_value_label.place(relx=0.3, rely=0.8, anchor=tkinter.W)

            # Re-enable interaction with the edit button
            edit_userpass_label.bind("<Button-1>", hide_userpass_values)
            edit_userpass_label.bind("<Enter>", lambda e: edit_userpass_label.configure(cursor="hand2"))
            edit_userpass_label.bind("<Leave>", lambda e: edit_userpass_label.configure(cursor=""))

        save_button_userpass = CTkButton(
            userpass_frame,
            text="Save",
            fg_color="#7BA774",
            hover_color="#6E9770",
            corner_radius=8,
            width=80,
            command=update_user_info
        )
        save_button_userpass.place(relx=0.92, rely=0.11, anchor=tkinter.E)



    edit_userpass_label.bind("<Button-1>", hide_userpass_values)
    edit_userpass_label.bind("<Enter>", lambda e: edit_userpass_label.configure(cursor="hand2"))
    edit_userpass_label.bind("<Leave>", lambda e: edit_userpass_label.configure(cursor=""))


    userpass_separator = CTkFrame(userpass_frame, fg_color="#b6b9bd", height=2)
    userpass_separator.place(relx=0.001, rely=0.23, relwidth=0.997, anchor=tkinter.W)


    edit_account_label = CTkLabel(userpass_frame, text="Edit Account", **detail_properties)
    edit_account_label.place(relx=0.864, rely=0.3, anchor=tkinter.W)


    admin_credentials = fetch_admin_credentials()

    if admin_credentials is None:
        print("Error fetching admin credentials.")
        account_data = {
            'username': "Default Username",
            'password': "default_password",
            'pin': "000000"
        }
    else:
        account_data = {
            'username': admin_credentials['username'],
            'password': admin_credentials['password'],
            'pin': admin_credentials['pin']
        }

    username_label = CTkLabel(userpass_frame, text="Username:", **detail_properties)
    username_label.place(relx=0.1, rely=0.4, anchor=tkinter.W)


    username_value_label = CTkLabel(userpass_frame, text=account_data['username'], **input_properties)
    username_value_label.place(relx=0.3, rely=0.4, anchor=tkinter.W)

    
    password_label = CTkLabel(userpass_frame, text="Password:", **detail_properties)
    password_label.place(relx=0.1, rely=0.60, anchor=tkinter.W)


    password_value_label = CTkLabel(userpass_frame, text=f"{account_data['password']}", **input_properties)
    password_value_label.place(relx=0.3, rely=0.60, anchor=tkinter.W)


    pin_label = CTkLabel(userpass_frame, text="Pin Code:", **detail_properties)
    pin_label.place(relx=0.1, rely=0.80, anchor=tkinter.W)


    pin_value_label = CTkLabel(userpass_frame, text=f"{account_data['pin']}", **input_properties)
    pin_value_label.place(relx=0.3, rely=0.80, anchor=tkinter.W)



    def create_circular_image(size=(150, 150)):
        """Create a default circular image placeholder with transparent background."""
        # Create a mask in 'L' mode (grayscale), 0 means black and 255 means white
        mask = Image.new('L', size, 0)
        draw = ImageDraw.Draw(mask)
        # Draw a white ellipse (circle) in the center of the mask
        draw.ellipse((0, 0, size[0], size[1]), fill=255)

        # Create a background with transparent color (RGBA)
        placeholder = Image.new('RGBA', size, (224, 224, 224, 255))  # Light gray background
        output = Image.new('RGBA', size)

        # Paste the placeholder onto the output using the mask
        output.paste(placeholder, (0, 0), mask)

        # Return the circular placeholder as a CTkImage
        return CTkImage(light_image=output, dark_image=output, size=size)

    def update_profile_picture(event):
        """Update the profile picture when clicked."""
        global profile_image_label  # Ensure it is accessible

        file_path = filedialog.askopenfilename(
            filetypes=[("Image files", "*.png *.jpg *.jpeg *.gif *.bmp")]
        )

        if file_path:
            try:
                img = Image.open(file_path).convert('RGB')
                img.thumbnail((150, 150))

                # Create an RGBA image with a transparent background
                background = Image.new('RGBA', (150, 150), (0, 0, 0, 0))  # Fully transparent background
                offset = ((150 - img.width) // 2, (150 - img.height) // 2)
                background.paste(img, offset)

                # Create a circular mask
                mask = Image.new('L', (150, 150), 0)
                draw = ImageDraw.Draw(mask)
                draw.ellipse((0, 0, 150, 150), fill=255)  # White circle for mask

                # Composite the image with the circular mask to keep only the circle
                output = Image.composite(background, Image.new('RGBA', (150, 150), (0, 0, 0, 0)), mask)

                # Convert to CTkImage for customtkinter label usage
                photo = CTkImage(light_image=output, dark_image=output, size=(150, 150))

                # Save the updated image to the database (not shown here)
                save_profile_image_to_db(account_data['username'], file_path)

                # Update profile_image_label with the circular photo
                profile_image_label.configure(image=photo)
                profile_image_label.image = photo  # Keep reference

            except Exception as e:
                print(f"Error loading image: {e}")
                # Set default circular image placeholder on error
                photo = create_circular_image()
                profile_image_label.configure(image=photo)
                profile_image_label.image = photo

        profile_image_label.configure(cursor="hand2")

    def load_and_display_profile_image():
        """Load the profile image from the database and display it."""
        img = retrieve_profile_image_from_db(account_data['username'])
        if img:
            # Resize the image if necessary
            img.thumbnail((150, 150))
            background = Image.new('RGB', (150, 150), '#f0f0f0')
            offset = ((150 - img.width) // 2, (150 - img.height) // 2)
            background.paste(img, offset)

            mask = Image.new('L', (150, 150), 0)
            draw = ImageDraw.Draw(mask)
            draw.ellipse((0, 0, 150, 150), fill=255)
            output = Image.composite(background, Image.new('RGB', (150, 150), '#f0f0f0'), mask)
            photo = CTkImage(light_image=output, dark_image=output, size=(150, 150))

            # Update the label with the profile image
            profile_image_label.configure(image=photo)
            profile_image_label.image = photo  # Keep reference
        else:
            # If no image is found, display the default circular placeholder
            photo = create_circular_image()
            profile_image_label.configure(image=photo)
            profile_image_label.image = photo  # Keep reference

    # Create the initial profile image placeholder
    initial_photo = create_circular_image()
    global profile_image_label  # Declare as global
    profile_image_label = CTkLabel(
        content_frame,
        image=initial_photo,
        text=""
    )
    profile_image_label.image = initial_photo
    profile_image_label.place(relx=0.07, rely=0.22, anchor=tkinter.CENTER)

    # Load and display the profile image from the database
    load_and_display_profile_image()

    # Bind the click event to update the profile picture
    profile_image_label.bind("<Button-1>", update_profile_picture)
    profile_image_label.configure(cursor="hand2")

    name_label = CTkLabel(content_frame, text="Cyrus Severino", font=("Public Sans", 30, "bold"), text_color="black")
    name_label.place(relx=0.14, rely=0.188, anchor=tkinter.W)


    role_label = CTkLabel(content_frame, text="Administrator", font=("Public Sans", 22), text_color="black")
    role_label.place(relx=0.14, rely=0.23, anchor=tkinter.W)


    status_label = CTkLabel(content_frame, text="ACTIVE", font=("Public Sans", 15), text_color="#0dce31")
    status_label.place(relx=0.14, rely=0.262, anchor=tkinter.W)


    set_photo_label = CTkLabel(content_frame, text="Set Photo", font=("Public Sans", 16, "bold"), text_color="black")
    set_photo_label.place(relx=0.047, rely=0.315, anchor=tkinter.W)


    platform_label = CTkLabel(content_frame, text="Platform", font=("Public Sans", 30, "bold"), text_color="black")
    platform_label.place(relx=0.50, rely=0.188, anchor=tkinter.W)


    os_pos_label = CTkLabel(content_frame, text="OS POS", font=("Public Sans", 22), text_color="black")
    os_pos_label.place(relx=0.50, rely=0.23, anchor=tkinter.W)

    
