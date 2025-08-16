# Shiny Ikiam Database Manager

A multi-user Shiny web application for managing the Ikiam Insectary database. It serves as the data entry and editing counterpart to the [Shiny Ikiam Wings Gallery](http://wings.gallery.info.gf).

## [Live application](https://wings.gallery.info.gf/db)

## Deployment

This application is deployed on the same Amazon EC2 instance as the *Shiny Ikiam Wings Gallery*, served under a specific path (`/db`). It leverages the same Docker, Nginx, and HTTPS setup.

For detailed deployment instructions, see [DEPLOYMENT.md](DEPLOYMENT.md).

## How it works

The app provides a secure, multi-user interface to interact with a central database hosted on Google Sheets. It connects to Google Sheets using a service account. Changes made by users are first saved to a local state on the server. An authorized user can then review, manage, and commit these changes in batches to the master Google Sheet, ensuring data integrity and providing an audit trail.

## Features

-   **Multi-user Authentication**: Login system to track edits by user and control permissions.
-   **Tab-based Workflow**: Dedicated modules for common data entry tasks:
    -   **Registrar Muertes**: Record specimen deaths, including date and cause.
    -   **Registrar Tubos**: Assign and manage tissue sample tube IDs, with predictive suggestions.
    -   **Registrar Emergidos**: Log new individuals emerging from clutches, with batch data entry support.
-   **Intelligent Data Entry**:
    -   **Autofill**: Automatically populates fields like `SPECIES`, `Stock_of_origin`, and `Wild_Reared` based on the selected `CLUTCH NUMBER`.
    -   **Predictive Suggestions**: Provides dropdown suggestions for sequential IDs (like `CAM_ID` and `Tube_ID`) based on the most recent entries, speeding up data input.
-   **Local Change Tracking**: Edits are not sent directly to Google Sheets. They are logged locally, allowing for review and correction before final submission.
-   **Commit Management (`Subir Cambios` Tab)**: A dedicated interface for authorized users to:
    -   View all pending local changes from all users.
    -   Individually undo/redo specific edits.
    -   Commit approved changes to the Google Sheet in a single operation.
-   **Commit History (`Historial de Cambios` Tab)**: View a log of past data commits, filterable by date, user, and commit ID.
