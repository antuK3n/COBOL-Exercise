from customtkinter import *
import tkinter as tk
from tkinter import messagebox, Canvas, Scrollbar
from main_database import fetch_products, add_product_to_db, update_product_stock_in_db
import csv
import os


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

    def inventory_csv():
        data = fetch_products()

        with open('INVENTORY.csv', mode='w', newline='', encoding='utf-8') as file:
            writer = csv.writer(file)
            
            # Write product data
            for product in data:
                writer.writerow([
                    product['id'], 
                    product['name'], 
                    product['code'], 
                    product['available'], 
                    product['price']
                ])
        
        print(f"Inventory exported to INVENTORY.csv")

    def inventory_export_click():
        # Export the data to CSV before clearing the table
        inventory_csv()

        # Optionally, show a message confirming that the receipt has been saved
        messagebox.showinfo("Inventory", "Data has been saved to INVENTORY.csv")

    #Generate Products Inventory (Green)
    inventory_button = CTkButton(
        header_frame,  # Assuming calculator_frame is already defined
        text="Export Inventory",
        font=("Public Sans", 16),
        text_color="white",
        fg_color="#4CAF50",
        hover_color="#45a049",
        corner_radius=10,
        command=inventory_export_click  # Bind the click event to the function
    )
    inventory_button.place(relx=0.35, rely=0.53, anchor="e", relwidth=0.2, relheight=0.7)

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

        # Clear existing cards
        for widget in cards_frame.winfo_children():
            widget.destroy()

        for i, product in enumerate(products):
            row = i // columns
            column = i % columns

            # Create product card
            product_card = CTkFrame(cards_frame, fg_color="#e0e0e0", corner_radius=10, width=card_width, height=card_height)
            product_card.grid(row=row, column=column, padx=padding, pady=padding, sticky="nsew")

            # Stock label to dynamically update the stock display
            stock_label = CTkLabel(product_card, text=f"Available: {product['available']}", font=("Public Sans", 12), text_color="gray")
            stock_label.place(relx=0.05, rely=0.6, anchor="w")

            # Bind click event to handle product click
            product_card.bind(
                "<Button-1>",
                lambda event, p=product.copy(), label=stock_label: handle_product_click(p, label)
            )

            # Other product details on the card
            CTkLabel(product_card, text=product["name"], font=("Public Sans", 14, "bold"), text_color="black").place(relx=0.05, rely=0.2, anchor="w")
            CTkLabel(product_card, text=f"Code: {product['code']}", font=("Public Sans", 12), text_color="gray").place(relx=0.05, rely=0.4, anchor="w")
            CTkLabel(product_card, text=f"₱ {product['price']:.2f}", font=("Public Sans", 12, "bold"), text_color="green").place(relx=0.05, rely=0.8, anchor="w")

        # Add "Add Product" card
        add_card = CTkFrame(cards_frame, fg_color="#e0e0e0", corner_radius=10, width=card_width, height=card_height)

        if not products:
            row, column = 0, 0
        else:
            row = len(products) // columns
            column = len(products) % columns

        add_card.grid(row=row, column=column, padx=padding, pady=padding, sticky="nsew")

        CTkLabel(add_card, text="Add Product", font=("Public Sans", 12, "bold"), text_color="black").place(relx=0.5, rely=0.4, anchor="center")
        CTkButton(add_card, text="Add", text_color="white", fg_color="#141b35", hover_color="#1d2847", command=open_add_product).place(relx=0.5, rely=0.7, anchor="center")

        update_scroll_region()
        canvas.update_idletasks()

    def handle_product_click(product, stock_label):
        # First, deduct the stock
        if product["available"] > 0:
            # Deduct the quantity
            product["available"] -= 1
            
            # Update the stock label immediately to reflect the new stock
            stock_label.configure(text=f"Available: {product['available']}")
            
            # Update the product stock in the database
            update_product_stock_in_db(product)
            
            # Add the product to the table after stock deduction
            add_to_table(product)

            # Re-render the product cards to reflect the updated stock
            create_product_cards()

            # Check if the product is now out of stock
            if product["available"] == 0:
                display_out_of_stock_message()

        else:
            # If product is already out of stock, just show the message
            display_out_of_stock_message()

    def display_out_of_stock_message():
        # This function displays a message that the product is out of stock.
        messagebox.showerror("Out of Stock", "Sorry, this product is out of stock!")

    def run_cobol_and_process_csv(csv_path):
        """
        Processes a CSV file where each product is represented as:
        - Row 1: Product Name
        - Row 2: Quantity (QTY)
        - Row 3: Unit Price
        Followed by an empty line before the next set.

        Parameters:
            csv_path (str): Path to the CSV file generated by the COBOL program.
        """
        try:
            # Step 1: Check if CSV file exists
            if not os.path.exists(csv_path):
                print(f"CSV file not found: {csv_path}")
                return

            # Step 2: Open and read the CSV file
            with open(csv_path, mode='r', encoding='utf-8') as csv_file:
                csv_reader = csv.reader(csv_file)
                lines = [line for line in csv_reader if any(line)]  # Remove empty rows
                
                # Step 3: Process rows in sets of 3 (Product Name, QTY, Unit Price)
                i = 0
                while i < len(lines):
                    try:
                        product_name = lines[i][0].strip()  # Row 1: Product name
                        quantity = int(lines[i + 1][0].strip())  # Row 2: Quantity
                        price = float(lines[i + 2][0].strip())  # Row 3: Unit price

                        # Generate product code dynamically
                        code = f"P{len(fetch_products()) + 1:03}"

                        # Add product to the database
                        add_product_to_db(product_name, code, quantity, price)
                        create_product_cards()

                        print(f"Added product: {product_name}, QTY: {quantity}, Price: {price}")
                        i += 3  # Move to the next set of rows
                    except (IndexError, ValueError) as e:
                        print(f"Skipping invalid row set starting at line {i+1}: {e}")
                        i += 1  # Skip to the next row if an error occurs
            
            # Step 4: Clear the contents of the CSV file after processing
            with open(csv_path, 'w', newline='', encoding='utf-8') as file:
                pass  # Opening in 'w' mode clears the contents of the file

            print(f"CSV file cleared: {csv_path}")

        except Exception as e:
            print(f"An unexpected error occurred: {e}")


    def open_add_product():
        """
        Processes the CSV directly, without running the COBOL executable.
        """
        # Get the current script directory
        current_dir = os.path.dirname(os.path.abspath(__file__))

        # Construct the path to the CSV file in the 'cobol' folder
        csv_path = os.path.join(current_dir, 'PRODUCTS.CSV')

        # Process the CSV
        run_cobol_and_process_csv(csv_path)
        
        # Clear the contents of the CSV file
        with open(csv_path, 'w', newline='') as file:
            pass  # Opening in 'w' mode clears the contents of the file



    current_row_index = [1]  # Row index starts at 1

    def add_to_table(product):
        nonlocal current_row_index

        # Get the current row index
        current_row = current_row_index[0]

        # Check if the product is already in the cart
        for i in range(1, current_row):  # Start from 1 to skip the header row
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

        # If the product is not in the cart, add it to the table
        item_name = CTkLabel(item_table, text=product["name"], font=("Public Sans", 16), text_color="black", anchor="center")
        item_name.grid(row=current_row, column=0, padx=10, pady=5, sticky="nsew")

        qty = CTkLabel(item_table, text="1", font=("Public Sans", 16), text_color="black", anchor="center")
        qty.grid(row=current_row, column=1, padx=10, pady=5, sticky="nsew")

        price = CTkLabel(item_table, text=f"₱ {product['price']:.2f}", font=("Public Sans", 16), text_color="black", anchor="center")
        price.grid(row=current_row, column=2, padx=10, pady=5, sticky="nsew")

        subtotal = CTkLabel(item_table, text=f"₱ {product['price']:.2f}", font=("Public Sans", 16), text_color="black", anchor="center")
        subtotal.grid(row=current_row, column=3, padx=10, pady=5, sticky="nsew")

        # Ensure the rows are spaced equally and added in a top-down order
        item_table.grid_rowconfigure(current_row, weight=0)  # Rows will not stretch

        # Increment the current row index
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

    tax_text_label = CTkLabel(subtotal_frame, text="Tax:", font=("Public Sans", 18), text_color="black", anchor="w")
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

    # Global variables for discount and tax rates
    discount_rate = 0.0  # Default 0% discount
    tax_rate = 0.12      # Default 12% tax rate

    def update_totals():
        subtotal = 0.0

        # Iterate through rows in the table to sum up subtotals
        for i in range(1, current_row_index[0]):  # Skip header row
            # Get quantity and price widgets
            qty_widget = item_table.grid_slaves(row=i, column=1)  # Quantity column
            price_widget = item_table.grid_slaves(row=i, column=2)  # Price column

            if qty_widget and price_widget:  # If both widgets exist
                try:
                    # Safely extract quantity and price
                    qty = int(qty_widget[0].cget("text"))
                    # Remove currency symbol, strip whitespace, and convert to float
                    price_str = price_widget[0].cget("text")
                    price = float(price_str.replace("₱", "").replace(",", "").strip())
                    
                    row_subtotal = qty * price  # Calculate subtotal for this row
                    subtotal += row_subtotal  # Add to overall subtotal
                except (ValueError, AttributeError):
                    print(f"Error processing row {i}: Invalid quantity or price")
                    continue

        # Calculate other totals based on updated discount and tax rates
        discount = subtotal * discount_rate
        tax = subtotal * tax_rate
        total = subtotal + tax - discount

        # Update labels to reflect the calculated values
        # Use thousands separator for better readability
        subtotal_value_label.configure(text=f"₱ {subtotal:,.2f}")
        discount_value_label.configure(text=f"₱ {discount:,.2f}")
        tax_value_label.configure(text=f"₱ {tax:,.2f}")
        total_value_label.configure(text=f"₱ {total:,.2f}")

    def open_discount_popup():
        def set_discount():
            nonlocal discount_rate
            try:
                # Get the discount percentage from the entry widget
                entered_discount = discount_entry.get()
                if entered_discount:  # Only update if input is not empty
                    # Convert to decimal (divide by 100)
                    discount_rate = float(entered_discount) / 100
                else:
                    discount_rate = 0.0  # No discount if no input is provided
                
                discount_popup.destroy()  # Close the popup window
                update_totals()  # Recalculate and update totals
            except ValueError:
                # Create custom error popup
                error_popup = CTkToplevel(discount_popup)
                error_popup.title("Invalid Input")
                error_popup.geometry("300x200")
                
                # Error message label
                error_label = CTkLabel(
                    error_popup, 
                    text="Please enter a valid\nnumeric value for discount.", 
                    font=("Public Sans", 14)
                )
                error_label.pack(expand=True, pady=20)
                
                # OK button to close the error popup
                def close_error():
                    error_popup.destroy()
                
                ok_button = CTkButton(
                    error_popup, 
                    text="OK", 
                    font=("Public Sans", 14), 
                    command=close_error
                )
                ok_button.pack(pady=20)
                
                # Make error popup modal
                error_popup.grab_set()
                error_popup.focus()

        # Create the popup window
        discount_popup = CTkToplevel()
        discount_popup.title("Set Discount")
        discount_popup.geometry("300x200")  # Set a reasonable size
        
        # Label and entry for discount percentage
        discount_label = CTkLabel(discount_popup, text="Enter Discount Percentage:", font=("Public Sans", 14))
        discount_label.pack(padx=20, pady=10)

        discount_entry = CTkEntry(
            discount_popup, 
            font=("Public Sans", 14), 
            placeholder_text="Enter Discount (%)"
        )
        discount_entry.pack(padx=20, pady=10)
        discount_entry.focus()  # Set focus to the entry widget

        # Submit button to set the discount
        submit_button = CTkButton(
            discount_popup, 
            text="Set Discount", 
            font=("Public Sans", 14), 
            command=set_discount
        )
        submit_button.pack(padx=20, pady=20)

    def open_tax_popup():
        def set_tax():
            nonlocal tax_rate
            try:
                # Get the tax percentage from the entry widget
                entered_tax = tax_entry.get()
                if entered_tax:  # Only update if input is not empty
                    # Convert to decimal (divide by 100)
                    tax_rate = float(entered_tax) / 100
                else:
                    tax_rate = 0.12  # Default tax rate
                
                tax_popup.destroy()  # Close the popup window
                update_totals()  # Recalculate and update totals
            except ValueError:
                # Create custom error popup
                error_popup = CTkToplevel(tax_popup)
                error_popup.title("Invalid Input")
                error_popup.geometry("300x200")
                
                # Error message label
                error_label = CTkLabel(
                    error_popup, 
                    text="Please enter a valid\nnumeric value for tax.", 
                    font=("Public Sans", 14)
                )
                error_label.pack(expand=True, pady=20)
                
                # OK button to close the error popup
                def close_error():
                    error_popup.destroy()
                
                ok_button = CTkButton(
                    error_popup, 
                    text="OK", 
                    font=("Public Sans", 14), 
                    command=close_error
                )
                ok_button.pack(pady=20)
                
                # Make error popup modal
                error_popup.grab_set()
                error_popup.focus()

        # Create the popup window
        tax_popup = CTkToplevel()
        tax_popup.title("Set Tax")
        tax_popup.geometry("300x200")  # Set a reasonable size
        
        # Label and entry for tax percentage
        tax_label = CTkLabel(tax_popup, text="Enter Tax Percentage:", font=("Public Sans", 14))
        tax_label.pack(padx=20, pady=10)

        tax_entry = CTkEntry(
            tax_popup, 
            font=("Public Sans", 14), 
            placeholder_text="Enter Tax (%)"
        )
        tax_entry.pack(padx=20, pady=10)
        tax_entry.focus()  # Set focus to the entry widget

        # Submit button to set the tax
        submit_button = CTkButton(
            tax_popup, 
            text="Set Tax", 
            font=("Public Sans", 14), 
            command=set_tax
        )
        submit_button.pack(padx=20, pady=20)

    # Discount Button
    discount_button = CTkButton(
        calculator_frame,
        text="Discount",
        font=("Public Sans", 16),
        text_color="white",
        fg_color="#141b35",
        hover_color="#1d2847",
        corner_radius=10,
        command=open_discount_popup  # Open discount popup when button is clicked
    )
    discount_button.place(relx=0.97, rely=0.745, anchor="e", relwidth=0.4, relheight=0.05)

    # Tax Button
    tax_button = CTkButton(
        calculator_frame,
        text="Tax",
        font=("Public Sans", 16),
        text_color="white",
        fg_color="#141b35",
        hover_color="#1d2847",
        corner_radius=10,
        command=open_tax_popup  # Open tax popup when button is clicked
    )
    tax_button.place(relx=0.97, rely=0.82, anchor="e", relwidth=0.4, relheight=0.05)

    # Function to clear the table and reset labels
    def clear_table():
        # Clear the table rows
        for i in range(1, current_row_index[0]):  # Start from 1 to skip the header row
            for widget in item_table.grid_slaves(row=i):  # Get all widgets in the row
                widget.destroy()

        # Reset the subtotal, discount, tax, and total labels to their default values
        subtotal_value_label.configure(text="₱ 0.00")
        discount_value_label.configure(text="₱ 0.00")
        tax_value_label.configure(text="₱ 0.00")
        total_value_label.configure(text="₱ 0.00")

    def export_to_csv():
        # Open the CSV file for writing with UTF-8 encoding
        with open('RECEIPT.csv', mode='w', newline='', encoding='utf-8') as file:
            writer = csv.writer(file)

            # Loop through all the rows in the table (starting from row 1 to skip header)
            for i in range(1, current_row_index[0]):  # Skipping header row
                # Extract widgets from each column
                item_name_widget = item_table.grid_slaves(row=i, column=0)  # Product Name
                qty_widget = item_table.grid_slaves(row=i, column=1)        # Quantity
                price_widget = item_table.grid_slaves(row=i, column=2)      # Price
                subtotal_widget = item_table.grid_slaves(row=i, column=3)   # Subtotal

                # Check if widgets exist for that row
                if item_name_widget and qty_widget and price_widget and subtotal_widget:
                    product_name = item_name_widget[0].cget("text")  # Product name
                    quantity = qty_widget[0].cget("text")  # Quantity
                    price = price_widget[0].cget("text")  # Price
                    subtotal = subtotal_widget[0].cget("text")  # Subtotal

                    # Write the data to the CSV file in the desired format (single row for item)
                    writer.writerow([product_name, quantity, price, subtotal])

            # Extract totals from the labels
            subtotal_total = subtotal_value_label.cget("text")  # Get text of subtotal label
            discount_total = discount_value_label.cget("text")  # Get text of discount label
            tax_total = tax_value_label.cget("text")  # Get text of tax label
            total = total_value_label.cget("text")  # Get text of total label

            # Write totals to the CSV file
            writer.writerow(["Subtotal", "", "", subtotal_total])
            writer.writerow(["Discount", "", "", discount_total])
            writer.writerow(["Tax", "", "", tax_total])
            writer.writerow(["Grand Total", "", "", total])

        print("Receipt exported to RECEIPT.csv")

    def on_payment_click():
        # Export the data to CSV before clearing the table
        export_to_csv()

        # Clear the table 
        clear_table()

        # Optionally, show a message confirming that the receipt has been saved
        messagebox.showinfo("Payment", "Receipt has been saved to RECEIPT.csv")

    # Payment Button (Green)
    payment_button = CTkButton(
        calculator_frame,  # Assuming calculator_frame is already defined
        text="Payment",
        font=("Public Sans", 16),
        text_color="white",
        fg_color="#4CAF50",
        hover_color="#45a049",
        corner_radius=10,
        command=on_payment_click  # Bind the click event to the function
    )
    payment_button.place(relx=0.97, rely=0.95, anchor="e", relwidth=0.4, relheight=0.05)

    create_product_cards()
