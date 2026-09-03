# Sandwich Panel Installation Tracker

**AutoCAD plugin for real-time construction progress tracking, subcontractor management, and automated reporting.**

---

## Problem

On a 5,000-panel industrial building project, site supervisors spent **4+ hours weekly** manually marking up printed shop drawings with colored markers to track:

- Which subcontractor installed which panels
- Which panels were defective and needed replacement
- Daily installed square footage for progress reports
- Delivery phasing
- Consumables calculation (sealant, screws, flashing tape)

Reports were compiled in Excel by hand. Errors led to subcontractor disputes and delayed payments.

---

## Solution

An **AutoCAD plugin** that turns the shop drawing into a **live construction dashboard**.

The foreman selects panels directly in AutoCAD, assigns a subcontractor and status — and instantly gets:

- **Visual feedback** (color-coded hatches: grey = not installed, green = installed, red = defective, yellow = repairable)
- **Auto-generated AutoCAD tables** with square footage, status breakdowns, and material take-offs
- **One-click Excel export** with separate sheets for summary, subcontractors, and joints
- **Joint calculations** (vertical edges, horizontal seams between panels)
- **Cutout tracking** with area and perimeter summation

---

## Key Metrics

| Metric | Before | After |
|:---|:---|:---|
| Weekly reporting time | 4 hours | 10 seconds |
| 1,903 panels initial setup | N/A | 12 minutes |
| 4,943 panels initial setup | N/A | ~30 minutes |
| Defect tracking | Manual marker on paper | Automated, color-coded, exportable |
| Subcontractor disputes | Frequent | Eliminated (data-backed) |

---

## Architecture

The plugin consists of **6 modules** with clear separation of concerns:

| Module | File | Responsibility |
|:---|:---|:---|
| XData Core | `xdata.lsp` | Read/write extended entity data (persistent DWG database) |
| Assignment | `assign.lsp` | Subcontractor, status, queue, cutout, and hatch management |
| Reports | `reports.lsp` | Text-based summaries and statistics |
| Tables | `table.lsp` | AutoCAD native tables (summary, subcontractors, defects, marks) |
| Joints | `joints.lsp` | Vertical edge and horizontal seam calculations |
| Excel | `excel.lsp` | COM-based Excel export |

**15+ commands** total, registered on a custom AutoCAD ribbon tab.

---

## Technologies Used

- **AutoLISP / Visual LISP** — core automation
- **ActiveX COM** — Excel integration
- **Dynamic Block Properties API** — reading custom parameters from dynamic blocks
- **Extended Data (XData)** — persistent per-entity database inside DWG
- **Handle-based tracking** — permanent object references (survives DWG close/reopen)

---

## Quick Start

1. Copy all `.lsp` files to a single folder
2. In AutoCAD: `OPTIONS` → **Files** → **Support File Search Path** → add the folder
3. Load the plugin:
