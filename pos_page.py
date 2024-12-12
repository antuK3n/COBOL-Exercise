from customtkinter import *
import tkinter as tk
from tkinter import messagebox, Canvas, Scrollbar
from main_database import fetch_products, add_product_to_db

def pos_content(content_frame):
    # Clear existing widgets in the frame
    for widget in content_frame.winfo_children():
        widget.place_forget()

    # Define common frame properties
    frame_properties = {
        "master": content_frame,
        "fg_color": "#e0e0e0",
        "corner_radius": 20
    }

    # Create and place frames
    top_frame = CTkFrame(**frame_properties)
    products_frame = CTkFrame(master=content_frame, fg_color="#f0f0f0", corner_radius=20)
    calculator_frame = CTkFrame(**frame_properties)

    # Place frames
    top_frame.place(relx=0.495, rely=0.06, anchor="center", relwidth=0.965, relheight=0.08)
    products_frame.place(relx=0.305, rely=0.538, anchor="center", relwidth=0.58, relheight=0.825)
    calculator_frame.place(relx=0.79, rely=0.53, anchor="center", relwidth=0.37, relheight=0.84)

    # Add labels
    label_properties = {
        "font": ("Public Sans", 25, "bold"),
        "text_color": "black"
    }
    CTkLabel(top_frame, text="POS DASHBOARD", **label_properties).place(relx=0.015, rely=0.45, anchor="w")

    # Header and products area
    header_frame = CTkFrame(products_frame, height=50, fg_color="#f0f0f0")
    header_frame.pack(fill="x", side="top", pady=0)

    CTkLabel(header_frame, text="Products", **label_properties).place(relx=0.02, rely=0.5, anchor="w")

    # Create container
    container = CTkFrame(products_frame, fg_color="#f0f0f0", border_width=0)
    container.pack(fill="both", expand=True, padx=0, pady=0)

    # Canvas and scrollbar
    canvas = Canvas(container, bg="#f0f0f0", bd=0, highlightthickness=0)
    canvas.pack(side="left", fill="both", expand=True, padx=0, pady=0)

    scrollbar = CTkScrollbar(container, fg_color="#f0f0f0", corner_radius=8)
    scrollbar.pack(side="right", fill="y", padx=0, pady=0)

    canvas.configure(yscrollcommand=scrollbar.set)
    cards_frame = CTkFrame(canvas, fg_color="#f0f0f0", border_width=0)
    canvas.create_window((0, 0), window=cards_frame, anchor="nw")
    scrollbar.configure(command=canvas.yview)

    current_row_index = [1]  # Use a list to store the current_row_index

    def update_scroll_region():
        products = fetch_products()
        columns = 5
        padding = 10
        card_height = 170
        add_card_height = 170
        container_height = 780

        rows = (len(products) + columns - 1) // columns
        total_height = rows * (card_height + padding) + padding + add_card_height

        if total_height > container_height:
            canvas.configure(scrollregion=(0, 0, 0, total_height))
            scrollbar.pack(side="right", fill="y", padx=0, pady=0)
        else:
            canvas.configure(scrollregion=(0, 0, 0, 0))
            scrollbar.pack_forget()

        if not products or total_height <= container_height:
            scrollbar.pack_forget()
            canvas.configure(scrollregion=(0, 0, 0, 0))

    def create_product_cards():
        products = fetch_products()

        columns = 5
        padding = 10
        container_width = 900
        card_width = (container_width - ((columns + 1) * padding)) / columns if container_width > 0 else 200
        card_height = 170

        for widget in cards_frame.winfo_children():
            widget.destroy()

        for i, product in enumerate(products):
            row = i // columns
            column = i % columns

            product_card = CTkFrame(cards_frame, fg_color="#e0e0e0", corner_radius=10, width=card_width, height=card_height)
            product_card.grid(row=row, column=column, padx=padding, pady=padding, sticky="nsew")

            product_card.bind("<Enter>", lambda event, card=product_card: card.configure(fg_color="#d3d3d3"))
            product_card.bind("<Leave>", lambda event, card=product_card: card.configure(fg_color="#e0e0e0"))
            product_card.bind("<Button-1>", lambda event, product=product: add_to_table(product))

            CTkLabel(product_card, text=product["name"], font=("Public Sans", 14, "bold"), text_color="black").place(relx=0.05, rely=0.2, anchor="w")
            CTkLabel(product_card, text=f"Code: {product['code']}", font=("Public Sans", 12), text_color="gray").place(relx=0.05, rely=0.4, anchor="w")
            CTkLabel(product_card, text=f"Available: {product['available']}", font=("Public Sans", 12), text_color="gray").place(relx=0.05, rely=0.6, anchor="w")
            CTkLabel(product_card, text=f"₱ {product['price']:.2f}", font=("Public Sans", 12, "bold"), text_color="green").place(relx=0.05, rely=0.8, anchor="w")

                # Add "Add Product" card
        add_card = CTkFrame(cards_frame, fg_color="#e0e0e0", corner_radius=10, width=card_width, height=card_height)

        if not products:
            # If no products, add the card to the first position
            row, column = 0, 0
        else:
            # Add "Add Product" card after the last product
            row = len(products) // columns
            column = len(products) % columns

        add_card.grid(row=row, column=column, padx=padding, pady=padding, sticky="nsew")

        # Add "Add" button to the "Add Product" card
        CTkLabel(add_card, text="Add Product", font=("Public Sans", 12, "bold"), text_color="black").place(relx=0.5, rely=0.4, anchor="center")
        CTkButton(add_card, text="Add", text_color="white", fg_color="#141b35", hover_color="#1d2847", command=open_add_product_popup).place(relx=0.5, rely=0.7, anchor="center")

        update_scroll_region()
        canvas.update_idletasks()

        # Open Add Product popup
    def open_add_product_popup():
        popup = CTkToplevel()
        popup.title("Add Product")
        popup.geometry("400x300")

        popup.attributes("-topmost", True)

        # Input fields for product details
        CTkLabel(popup, text="Product Name:").pack(pady=(20, 5))
        name_entry = CTkEntry(popup, width=250)
        name_entry.pack()

        CTkLabel(popup, text="Quantity:").pack(pady=(10, 5))
        quantity_entry = CTkEntry(popup, width=250)
        quantity_entry.pack()

        CTkLabel(popup, text="Price (₱):").pack(pady=(10, 5))
        price_entry = CTkEntry(popup, width=250)
        price_entry.pack()

        # Save button to add product
        def save_product():
            name = name_entry.get()
            quantity = quantity_entry.get()
            price = price_entry.get()

            if not name or not quantity or not price:
                messagebox.showerror("Error", "All fields are required!")
                return

            try:
                quantity = int(quantity)
                price = float(price)
            except ValueError:
                messagebox.showerror("Error", "Quantity must be an integer and Price must be a number!")
                return

            # Add product to database
            code = f"P{len(fetch_products()) + 1:03}"  # Generate product code
            add_product_to_db(name, code, quantity, price)

            # Update product cards
            create_product_cards()

            popup.destroy()

        CTkButton(popup, text="Save", command=save_product).pack(pady=(20, 5))

    current_row_index = [1]  # Row index starts at 1

    def add_to_table(product):
        nonlocal current_row_index

        # Get the current_row_index by referencing the list
        current_row = current_row_index[0]

        # Check if the product is already in the cart
        for i in range(1, current_row):  # Start from 1 to skip header row
            existing_name = item_table.grid_slaves(row=i, column=0)
            if existing_name and existing_name[0].cget("text") == product["name"]:
                # Product already exists, update quantity and subtotal
                qty_label = item_table.grid_slaves(row=i, column=1)[0]  # Get the qty label
                subtotal_label = item_table.grid_slaves(row=i, column=3)[0]  # Get the subtotal label

                # Update quantity and subtotal
                new_qty = int(qty_label.cget("text")) + 1
                qty_label.configure(text=str(new_qty))
                subtotal_label.configure(text=f"₱ {product['price'] * new_qty:.2f}")

                update_totals()  # Recalculate totals after updating the table
                return

        # Fixed row count - prevent it from growing beyond a certain number of rows
        MAX_ROWS = 11  # Adjust this based on how many rows you want to display initially

        if current_row >= MAX_ROWS:
            messagebox.showinfo("Info", "The cart is full.")
            return

        # Add product to the table (if it's not in the cart already)
        item_name = CTkLabel(item_table, text=product["name"], font=("Public Sans", 16), text_color="black", anchor="center")
        item_name.grid(row=current_row, column=0, padx=10, pady=5, sticky="nsew")

        qty = CTkLabel(item_table, text="1", font=("Public Sans", 16), text_color="black", anchor="center")
        qty.grid(row=current_row, column=1, padx=10, pady=5, sticky="nsew")

        price = CTkLabel(item_table, text=f"₱ {product['price']:.2f}", font=("Public Sans", 16), text_color="black", anchor="center")
        price.grid(row=current_row, column=2, padx=10, pady=5, sticky="nsew")

        subtotal = CTkLabel(item_table, text=f"₱ {product['price']:.2f}", font=("Public Sans", 16), text_color="black", anchor="center")
        subtotal.grid(row=current_row, column=3, padx=10, pady=5, sticky="nsew")

        # Ensure the rows are spaced equally and added in a top-down order
        item_table.grid_rowconfigure(current_row, weight=0)  # Rows will not stretch, and the content will stack top to bottom

        # Increment the current row index stored in the list
        current_row_index[0] += 1

        # Update totals after adding a new product
        update_totals()

    # Item table headers
    item_table = CTkFrame(calculator_frame, fg_color="#f0f0f0", corner_radius=20)
    item_table.place(relx=0.5, rely=0.32, anchor="center", relwidth=0.95, relheight=0.6)

    CTkLabel(item_table, text="Item", font=("Public Sans", 18, "bold"), text_color="black", anchor="center").grid(row=0, column=0, padx=10, pady=7, sticky="nsew")
    CTkLabel(item_table, text="Qty", font=("Public Sans", 18, "bold"), text_color="black", anchor="center").grid(row=0, column=1, padx=10, pady=7, sticky="nsew")
    CTkLabel(item_table, text="Price", font=("Public Sans", 18, "bold"), text_color="black", anchor="center").grid(row=0, column=2, padx=10, pady=7, sticky="nsew")
    CTkLabel(item_table, text="Subtotal", font=("Public Sans", 18, "bold"), text_color="black", anchor="center").grid(row=0, column=3, padx=10, pady=7, sticky="nsew")

    # Configure table columns to be resizable
    item_table.grid_columnconfigure(0, weight=1)
    item_table.grid_columnconfigure(1, weight=1)
    item_table.grid_columnconfigure(2, weight=1)
    item_table.grid_columnconfigure(3, weight=1)

    # Make sure the rows start from top and stack downwards
    item_table.grid_rowconfigure(1, weight=0)  # The first row (headers) shouldn't stretch
    item_table.grid_propagate(False)  # Prevent the container from resizing automatically

    subtotal_frame = CTkFrame(calculator_frame, fg_color="#f0f0f0", corner_radius=20)
    subtotal_frame.place(relx=0.28, rely=0.81, anchor="center", relwidth=0.5, relheight=0.34)

    # Create labels inside subtotal_frame
    subtotal_text_label = CTkLabel(subtotal_frame, text="Subtotal:", font=("Public Sans", 18), text_color="black", anchor="w")
    subtotal_text_label.place(relx=0.05, rely=0.1, anchor="w")

    subtotal_value_label = CTkLabel(subtotal_frame, text="₱ 0.00", font=("Public Sans", 18), text_color="black", anchor="e")
    subtotal_value_label.place(relx=0.95, rely=0.1, anchor="e")

    discount_text_label = CTkLabel(subtotal_frame, text="Discount:", font=("Public Sans", 18), text_color="black", anchor="w")
    discount_text_label.place(relx=0.05, rely=0.3, anchor="w")

    discount_value_label = CTkLabel(subtotal_frame, text="₱ 0.00", font=("Public Sans", 18), text_color="black", anchor="e")
    discount_value_label.place(relx=0.95, rely=0.3, anchor="e")

    tax_text_label = CTkLabel(subtotal_frame, text="Tax (12%):", font=("Public Sans", 18), text_color="black", anchor="w")
    tax_text_label.place(relx=0.05, rely=0.5, anchor="w")

    tax_value_label = CTkLabel(subtotal_frame, text="₱ 0.00", font=("Public Sans", 18), text_color="black", anchor="e")
    tax_value_label.place(relx=0.95, rely=0.5, anchor="e")

    # Separator line
    separator_line = CTkFrame(subtotal_frame, height=2, fg_color="#b6b9bd", corner_radius=0)
    separator_line.place(relx=0.02, rely=0.8, relwidth=0.96, anchor="w")

    # Total label
    total_text_label = CTkLabel(subtotal_frame, text="Total:", font=("Public Sans", 18, "bold"), text_color="black", anchor="w")
    total_text_label.place(relx=0.05, rely=0.90, anchor="w")

    total_value_label = CTkLabel(subtotal_frame, text="₱ 0.00", font=("Public Sans", 18, "bold"), text_color="black", anchor="e")
    total_value_label.place(relx=0.95, rely=0.90, anchor="e")

    # Define function to calculate totals
    def update_totals():
        subtotal = 0

        # Iterate through rows in the table to sum up subtotals
        for i in range(1, current_row_index[0]):  # Skip header row
            # Get quantity and price widgets
            qty_widget = item_table.grid_slaves(row=i, column=1)  # Quantity column
            price_widget = item_table.grid_slaves(row=i, column=2)  # Price column

            if qty_widget and price_widget:  # If both widgets exist
                try:
                    qty = int(qty_widget[0].cget("text"))  # Get quantity value
                    price = float(price_widget[0].cget("text").replace("₱", "").strip())  # Get price value
                    subtotal += qty * price  # Calculate subtotal for this row
                except ValueError:
                    pass  # Skip rows with invalid data

        # Calculate other totals
        discount = 0  # You can implement discounts based on conditions if needed
        tax = subtotal * 0.12  # 12% tax
        total = subtotal + tax - discount

        # Update labels
        subtotal_value_label.configure(text=f"₱ {subtotal:.2f}")
        discount_value_label.configure(text=f"₱ {discount:.2f}")
        tax_value_label.configure(text=f"₱ {tax:.2f}")
        total_value_label.configure(text=f"₱ {total:.2f}")

    # Discount Button
    discount_button = CTkButton(calculator_frame, text="Discount", font=("Public Sans", 16), text_color="white", fg_color="#141b35", hover_color="#1d2847", corner_radius=10)
    discount_button.place(relx=0.97, rely=0.745, anchor="e", relwidth=0.4, relheight=0.05)

    # Tax Button
    tax_button = CTkButton(calculator_frame, text="Tax", font=("Public Sans", 16), text_color="white", fg_color="#141b35", hover_color="#1d2847", corner_radius=10)
    tax_button.place(relx=0.97, rely=0.82, anchor="e", relwidth=0.4, relheight=0.05)

    # Payment Button (Green)
    payment_button = CTkButton(calculator_frame, text="Payment", font=("Public Sans", 16), text_color="white", fg_color="#4CAF50", hover_color="#45a049", corner_radius=10)
    payment_button.place(relx=0.97, rely=0.95, anchor="e", relwidth=0.4, relheight=0.05)
    

    create_product_cards()
