# Creating and Extending a Pivot Table in Excel

## Part 1: Create a New Spreadsheet with One Pivot Table

### 1. Create the workbook and enter data
- Open Excel → Blank workbook.
- In row 1, enter your headers: `A1: Foo`, `B1: Bar`, `C1: Baz`, `D1: Quz`.
- Below the headers, enter a few rows of sample data (a pivot table needs actual rows to summarize — an empty table with just headers won't produce anything meaningful).

### 2. Convert the range to a Table (optional but recommended)
- Select the data range including headers.
- `Insert → Table` (or `Ctrl+T`), confirm "My table has headers."
- This makes the pivot table auto-expand if you add rows later.

### 3. Insert the pivot table
- Click any cell inside your data/table.
- `Insert → PivotTable`.
- In the dialog, confirm the source range (or table name) is correct.
- Choose destination: `New Worksheet` (default, recommended) or `Existing Worksheet` + a cell reference.
- Click `OK`.

### 4. Build the pivot table layout
- A blank pivot table appears with a **PivotTable Fields** pane on the right listing `Foo`, `Bar`, `Baz`, `Quz`.
- Drag fields into the four zones as needed:
  - **Rows** — fields to group by (e.g., `Foo`)
  - **Columns** — fields to break out across the top (e.g., `Bar`)
  - **Values** — the field to aggregate (e.g., `Baz` or `Quz`, defaults to Sum/Count depending on data type)
  - **Filters** — optional, for slicing the whole table

### 5. Adjust value aggregation if needed
- Click the dropdown on the field in the **Values** area → `Value Field Settings` → pick Sum, Count, Average, etc.

### 6. Save
- `Ctrl+S`, save as `.xlsx`.

That's the full loop: data → table → PivotTable → drag fields → set aggregation → save.

---

## Part 2: Add a New Pivot Column to an Existing Pivot Table

### Add a new field as a Column
1. Click anywhere inside the existing pivot table (this makes the **PivotTable Fields** pane appear on the right; if it's hidden, `PivotTable Analyze → Field List`).
2. In the field list, find the field you want to add (e.g., `Quz`).
3. Drag it into the **Columns** box in the layout area at the bottom of the pane.
   - Alternatively, right-click the field name in the list → `Add to Column Labels`.
4. Excel immediately recalculates and spreads that field's unique values across the top of the table, nested under any field already there.

### A few things worth knowing
- **Order matters.** If multiple fields sit in the Columns box, the one listed first is the outer grouping, the next is nested inside it. Drag fields up/down within the box to reorder.
- **Adding a numeric field to Columns** groups by its distinct values, which is rarely what you want — numeric fields usually belong in **Values** (for aggregation) or **Rows** (if you want to group by discrete numbers/categories).
- **If the field isn't in the source data**, you can't just add it — you'd need to add the column to your original table/range first, then `PivotTable Analyze → Refresh` so the new field shows up in the field list.
- **To add a calculated column** (a field derived from others, not present in the source), use `PivotTable Analyze → Fields, Items & Sets → Calculated Field`, define the formula, then drag it into Columns or Values as appropriate.
