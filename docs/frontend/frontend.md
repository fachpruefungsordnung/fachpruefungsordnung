# Frontend Dev Documentation

Everything related to the frontend source code is located in the `/frontend/src/FPO` folder. 

There are 6 subfolders in there for the different parts of the application. Those are 
- [**Components**](#components) - Reusable/ important UI components
- [**Data**](#data) - Core data types, routing, and application state management
- [**Dto**](#dto) - Data Transfer Objects for API communication
- [**Page**](#page) - Individual page components and their logic
- [**Translations**](#translations) - Internationalization and language support
- [**UI**](#ui) - UI utilities, styling, and design system components

## Base Functionality
### Entrypoint 
The frontend application has one entrypoint, that is the `Main.purs` file. 

This file defines the root component that manages routing and page rendering based on URL changes. 

The ``component`` function serves as the root Halogen component that renders different pages (Home, Editor, Login, Admin panels, Profile, etc.) based on the current route, while also maintaining persistent global components like the navbar and toast notifications. 

The ``main`` function initializes the application by setting up the initial store with user preferences (language, translator), mounting the root component to the DOM, and establishing hash-based routing listeners to handle navigation between different pages of the application.

### AppMonad
The AppM monad in `AppM.purs` is a newtype wrapper that combines Halogen's component monad with a Store monad to manage application state and provides navigation functionality by setting URL hashes when routes change.

## Page

Individual page components and their logic. Every file in this folder correspond to one page in the application with a dedicated url behind it (e.g. Login, EditorPage, Profile, ...).

### Home 
### Page404

## Components

Here all components for

## Data

Core data types, routing, and application state management

## Dto

Data Transfer Objects for API communication

## Translations

Internationalization and language support

## UI

UI utilities, styling, and design system components
