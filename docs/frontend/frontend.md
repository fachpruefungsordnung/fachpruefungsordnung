# Frontend Dev Documentation

Everything related to the frontend source code is located in the `/frontend/src/FPO` folder. 

A simple introduction to the underlying web framework is found in the first 'chapter': - [**Halogen**](#halogen).

There are 6 subfolders in there for the different parts of our application. Those are 
- [**Data**](#data) - Core data types, routing, and application state management
- [**Page**](#page) - Individual page components and their logic
- [**Components**](#components) - Reusable/ important UI components
- [**Translations**](#translations) - Internationalization and language support
- [**Dto**](#dto) - Data Transfer Objects for API communication
- [**UI**](#ui) - UI utilities, styling, and design system components

## Halogen
Halogen is the name of the 'web framework' we chose for this project. In halogen you define components for everything that should be shown in the resulting web page. Halogen components have the data type 
```hs
component :: forall m. H.Component Query Input Output m
```
where `Query` is the data type for requesting information about the compoent from another component `Input` is the data that the components needs from other components and the `Output` type specifies what the component can output.

That is the base functionality. You don't have to use any of them and have a standalone component or use everything if the component is well connected to other ones.

Then there are a few key functions to handle the display of the component as well as the functionality. Nearly every creation of a component looks like this:
```hs
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval 
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , receive = Just <<< Receive 
        , finalize = ...
      }
    }```
where 
initialState :: Input -> State
render :: forall m. State -> H.ComponentHTML Action () m
handleAction :: forall m. Action -> H.HalogenM State Action Slots Output m Unit
handleQuery :: forall a m. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
```

Every component needs a `State`, that reflects the current information of a component. Like the method declarations indicate `initialState` is building a state from the input and render renders the UI of the component based on the current state. Those two functions are pure, that means we don't have access to the AppMonad and thus not on the application wide store, API requests and other things.

`receive` and `initialize` are there so that you can use monadic expressions when the component is created or the input changes. `finalize` is not used in our system, but can be used when a component is destroyed.

For the functionality and interaction of the components there are the other functions mentioned in the `eval` field. `handleAction` handles actions triggered by elements inside of the component itself, while `handleQuery` handles actions triggered by other components that requested this information. 

One not explored type until now is `Slots`. This type defines all the children components, that are embedded in this component. Like you will see later, out `Splitview` component contains many different children
```hs
type Slots =
  ( comment :: H.Slot Comment.Query Comment.Output Unit
  , commentOverview :: H.Slot CommentOverview.Query CommentOverview.Output Unit
  , editor :: H.Slot Editor.Query Editor.Output Int
  , preview :: H.Slot Preview.Query Preview.Output Unit
  , toc :: H.Slot TOC.Query TOC.Output Unit
  )
```

If there are open questions, look at the official documentation here: https://purescript-halogen.github.io/purescript-halogen/.

## Base Functionality
### Entrypoint 
The frontend application has one entrypoint, that is the `Main.purs` file. 

This file defines the root component that manages routing and page rendering based on URL changes. 

The ``component`` function serves as the root Halogen component that renders different pages (Home, Editor, Login, Admin panels, Profile, etc.) based on the current route, while also maintaining persistent global components like the navbar and toast notifications. 

The ``main`` function initializes the application by setting up the initial store with user preferences (language, translator), mounting the root component to the DOM, and establishing hash-based routing listeners to handle navigation between different pages of the application.

### AppMonad
The AppM monad in `AppM.purs` is a newtype wrapper that combines Halogen's component monad with a Store monad to manage application state and provides navigation functionality by setting URL hashes when routes change.

## Data

Here all core data types and other application wide data logic (error handling, data store, etc.) are placed in here.

### Store 
In `Store.purs` we defined a global state for the application is delivered by Halogen if you want to use it. At application startup the language is loaded and the resulting translator for the website is configured. While the application is running this store keeps track of upcoming errors while doing backend requests, updates the current route of the application and this updated the shown page simultaneously. You can see all those in the `type Store = { ... }` in the source file.

### Route 
The `Route.purs` file includes all the neccessary information and mapping urls to pages for routing in the application.

### Request
The `Request.purs` file contains our own set of functions for backend API calling. Those functions all use `Affjax` in the background and are connected to the store of our application to catch and update errors in those functions directly, so that the business logic doesn't have to keep track of all errors. There are general functions like `getJson` that returns a JSON object from a get request or specified functions like `getUser` that also encode the result into the specified user data type.

## Page

Individual page components and their logic. Every file in this folder correspond to one page in the application with a dedicated url behind it (e.g. Login, EditorPage, Profile, ...).

### Home 

The `Home` Page is a bit different from the others, because you get a totally different view whether you are logged in or not. This is determined via the line 
```hs
userWithError <- Store.preventErrorHandlingLocally getUser
```
and the following logic where the state of this component will get updated.

### Page404

This is like the sink of the application and every request resulting in a `404 - Not Found` from the backend will automatically redirected here. 

## Components

Here all components for possibly reusable contents on web pages are stored. Most of the time one or more components are embedded into a page because of the distinct features or applications although the component might not be used somewhere else.

### Splitview
The `Splitview.purs` component is our most complicated one. It consists of the three splitted views (as the name suggests) from the editor page perspective. In there lies at the left side the TOC or the comment section, in the middle the code editor itself and on the right side the preview or a comparison view. This, the size of the screens and all the transitions of data are handled through this component.

### Table Of Content (TOC)
The `TOC.purs` file contains all the information about the table of contents of the currently viewed project. It stores the currently viewed table of contents, all the meta information which configuration of paragraphs is allowed, synchronizes the TOC with the backend and handles all the document's and paragraph's history.

### Editor
Everything related to the code editor lies in the `FPO.Components.Editor` package. For everything to be working right here, we need to have two extra JavaScript files that allow us to configure and use the external editor library how we want to. External configurations include editor highlighting based on different font styles or comments and keybindings. 


## Dto
We use DTOs (or **D**ata **T**ransfer **O**bjects) mostly for handling data transmission with the backend or between components. That is, because a API reponse might not give back all the information about a user but just a subset of information that is then translated into the specific DTO in the frontend.

## Translations
To be able to have our website in two languages (and to easily add another one in the future if needed) we use the `Simple.I18n.Translation` package from PureScript. With that, instead of simply putting a hard-coded string everywhere on our pages, we have a translator in our `Store` and put lines like 
```
(translate (label :: _ "comment_delete") state.translator
```
in our html pages. Via the navbar component you can update the language of the page instantly through this implementation.

This package has some interesting design decisions, for example that the complete list of all labels (defined in `Labels.purs`) has to contain all application wide used labels lexicographically ordered in a single 'list'. We have a dedicated translation file for all the pages, so that adding or editing a is easier.

## UI
The `FPO.UI` package contains various utlity functions for consistent design or functions that the otherwise used `Web.HTML` library does not provide. Some of them are also directly translated from their JavaScript counterparts, therefore you see two js files also in there.
