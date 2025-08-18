import pandas as pd

# Assume you already read your Google Sheet into a DataFrame `df`
# Example:
df = pd.read_csv('data/pre_audit.csv')  # or use gspread for Google Sheets

# Convert column names to a single-column DataFrame
column_names_df = pd.DataFrame(df.columns, columns=["Column_Names"])

# Write to Excel
column_names_df.to_excel("column_names.xlsx", index=False)

print("Column names written to 'column_names.xlsx'")