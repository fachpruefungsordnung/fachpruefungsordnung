module FPO.UI.Css where

import Halogen.HTML.Core (ClassName(..))

-- State
active :: ClassName
active = ClassName "active"

disabled :: ClassName
disabled = ClassName "disabled"

show :: ClassName
show = ClassName "show"

fade :: ClassName
fade = ClassName "fade"

invisible :: ClassName
invisible = ClassName "invisible"

-- Display
dBlock :: ClassName
dBlock = ClassName "d-block"

dFlex :: ClassName
dFlex = ClassName "d-flex"

dGrid :: ClassName
dGrid = ClassName "d-grid"

dInlineBlock :: ClassName
dInlineBlock = ClassName "d-inline-block"

dInlineFlex :: ClassName
dInlineFlex = ClassName "d-inline-flex"

dNone :: ClassName
dNone = ClassName "d-none"

-- Flex
flexColumn :: ClassName
flexColumn = ClassName "flex-column"

flexGrow0 :: ClassName
flexGrow0 = ClassName "flex-grow-0"

flexGrow1 :: ClassName
flexGrow1 = ClassName "flex-grow-1"

flexNowrap :: ClassName
flexNowrap = ClassName "flex-nowrap"

flexShrink0 :: ClassName
flexShrink0 = ClassName "flex-shrink-0"

alignItemsCenter :: ClassName
alignItemsCenter = ClassName "align-items-center"

alignItemsEnd :: ClassName
alignItemsEnd = ClassName "align-items-end"

alignMiddle :: ClassName
alignMiddle = ClassName "align-middle"

justifyContentBetween :: ClassName
justifyContentBetween = ClassName "justify-content-between"

justifyContentCenter :: ClassName
justifyContentCenter = ClassName "justify-content-center"

justifyContentEnd :: ClassName
justifyContentEnd = ClassName "justify-content-end"

-- Spacing
gap1 :: ClassName
gap1 = ClassName "gap-1"

gap2 :: ClassName
gap2 = ClassName "gap-2"

gap3 :: ClassName
gap3 = ClassName "gap-3"

m0 :: ClassName
m0 = ClassName "m-0"

m1 :: ClassName
m1 = ClassName "m-1"

mAuto :: ClassName
mAuto = ClassName "m-auto"

mt0 :: ClassName
mt0 = ClassName "mt-0"

mt1 :: ClassName
mt1 = ClassName "mt-1"

mt2 :: ClassName
mt2 = ClassName "mt-2"

mt3 :: ClassName
mt3 = ClassName "mt-3"

mt4 :: ClassName
mt4 = ClassName "mt-4"

mt5 :: ClassName
mt5 = ClassName "mt-5"

mb0 :: ClassName
mb0 = ClassName "mb-0"

mb1 :: ClassName
mb1 = ClassName "mb-1"

mb2 :: ClassName
mb2 = ClassName "mb-2"

mb3 :: ClassName
mb3 = ClassName "mb-3"

mb4 :: ClassName
mb4 = ClassName "mb-4"

ms1 :: ClassName
ms1 = ClassName "ms-1"

ms2 :: ClassName
ms2 = ClassName "ms-2"

msAuto :: ClassName
msAuto = ClassName "ms-auto"

me1 :: ClassName
me1 = ClassName "me-1"

me2 :: ClassName
me2 = ClassName "me-2"

me3 :: ClassName
me3 = ClassName "me-3"

me5 :: ClassName
me5 = ClassName "me-5"

meAuto :: ClassName
meAuto = ClassName "me-auto"

mx1 :: ClassName
mx1 = ClassName "mx-1"

mx2 :: ClassName
mx2 = ClassName "mx-2"

mxAuto :: ClassName
mxAuto = ClassName "mx-auto"

my3 :: ClassName
my3 = ClassName "my-3"

my4 :: ClassName
my4 = ClassName "my-4"

my5 :: ClassName
my5 = ClassName "my-5"

p0 :: ClassName
p0 = ClassName "p-0"

p1 :: ClassName
p1 = ClassName "p-1"

p2 :: ClassName
p2 = ClassName "p-2"

p3 :: ClassName
p3 = ClassName "p-3"

p4 :: ClassName
p4 = ClassName "p-4"

px1 :: ClassName
px1 = ClassName "px-1"

px3 :: ClassName
px3 = ClassName "px-3"

py0 :: ClassName
py0 = ClassName "py-0"

py1 :: ClassName
py1 = ClassName "py-1"

py2 :: ClassName
py2 = ClassName "py-2"

py3 :: ClassName
py3 = ClassName "py-3"

py4 :: ClassName
py4 = ClassName "py-4"

py5 :: ClassName
py5 = ClassName "py-5"

pe3 :: ClassName
pe3 = ClassName "pe-3"

peNone :: ClassName
peNone = ClassName "pe-none"

ps2 :: ClassName
ps2 = ClassName "ps-2"

g4 :: ClassName
g4 = ClassName "g-4"

gx3 :: ClassName
gx3 = ClassName "gx-3"

w100 :: ClassName
w100 = ClassName "w-100"

h100 :: ClassName
h100 = ClassName "h-100"

vh100 :: ClassName
vh100 = ClassName "vh-100"

-- Text
textCenter :: ClassName
textCenter = ClassName "text-center"

textStart :: ClassName
textStart = ClassName "text-start"

textTruncate :: ClassName
textTruncate = ClassName "text-truncate"

textDecorationNone :: ClassName
textDecorationNone = ClassName "text-decoration-none"

fs1 :: ClassName
fs1 = ClassName "fs-1"

fs2 :: ClassName
fs2 = ClassName "fs-2"

fs4 :: ClassName
fs4 = ClassName "fs-4"

fs5 :: ClassName
fs5 = ClassName "fs-5"

fs6 :: ClassName
fs6 = ClassName "fs-6"

fwBold :: ClassName
fwBold = ClassName "fw-bold"

fwMedium :: ClassName
fwMedium = ClassName "fw-medium"

fwNormal :: ClassName
fwNormal = ClassName "fw-normal"

fwSemibold :: ClassName
fwSemibold = ClassName "fw-semibold"

display1 :: ClassName
display1 = ClassName "display-1"

display4 :: ClassName
display4 = ClassName "display-4"

lead :: ClassName
lead = ClassName "lead"

small :: ClassName
small = ClassName "small"

-- Colors
textBody :: ClassName
textBody = ClassName "text-body"

textDanger :: ClassName
textDanger = ClassName "text-danger"

textDark :: ClassName
textDark = ClassName "text-dark"

textInfo :: ClassName
textInfo = ClassName "text-info"

textMuted :: ClassName
textMuted = ClassName "text-muted"

textPrimary :: ClassName
textPrimary = ClassName "text-primary"

textSecondary :: ClassName
textSecondary = ClassName "text-secondary"

textSuccess :: ClassName
textSuccess = ClassName "text-success"

textWhite :: ClassName
textWhite = ClassName "text-white"

textBgDanger :: ClassName
textBgDanger = ClassName "text-bg-danger"

textBgInfo :: ClassName
textBgInfo = ClassName "text-bg-info"

textBgPrimary :: ClassName
textBgPrimary = ClassName "text-bg-primary"

textBgSecondary :: ClassName
textBgSecondary = ClassName "text-bg-secondary"

textBgSuccess :: ClassName
textBgSuccess = ClassName "text-bg-success"

textBgWarning :: ClassName
textBgWarning = ClassName "text-bg-warning"

bgBodySecondary :: ClassName
bgBodySecondary = ClassName "bg-body-secondary"

bgBodyTertiary :: ClassName
bgBodyTertiary = ClassName "bg-body-tertiary"

bgDanger :: ClassName
bgDanger = ClassName "bg-danger"

bgInfo :: ClassName
bgInfo = ClassName "bg-info"

bgPrimary :: ClassName
bgPrimary = ClassName "bg-primary"

bgSecondary :: ClassName
bgSecondary = ClassName "bg-secondary"

bgSecondarySubtle :: ClassName
bgSecondarySubtle = ClassName "bg-secondary-subtle"

bgSuccess :: ClassName
bgSuccess = ClassName "bg-success"

bgWarning :: ClassName
bgWarning = ClassName "bg-warning"

bgWhite :: ClassName
bgWhite = ClassName "bg-white"

-- Borders
border :: ClassName
border = ClassName "border"

border0 :: ClassName
border0 = ClassName "border-0"

borderBottom :: ClassName
borderBottom = ClassName "border-bottom"

rounded :: ClassName
rounded = ClassName "rounded"

roundedCircle :: ClassName
roundedCircle = ClassName "rounded-circle"

roundedPill :: ClassName
roundedPill = ClassName "rounded-pill"

-- Shadows
shadow :: ClassName
shadow = ClassName "shadow"

shadowSm :: ClassName
shadowSm = ClassName "shadow-sm"

-- Position
positionAbsolute :: ClassName
positionAbsolute = ClassName "position-absolute"

positionFixed :: ClassName
positionFixed = ClassName "position-fixed"

positionRelative :: ClassName
positionRelative = ClassName "position-relative"

top0 :: ClassName
top0 = ClassName "top-0"

bottom0 :: ClassName
bottom0 = ClassName "bottom-0"

start0 :: ClassName
start0 = ClassName "start-0"

end0 :: ClassName
end0 = ClassName "end-0"

-- Overflow
overflowAuto :: ClassName
overflowAuto = ClassName "overflow-auto"

overflowHidden :: ClassName
overflowHidden = ClassName "overflow-hidden"

overflowXHidden :: ClassName
overflowXHidden = ClassName "overflow-x-hidden"

overflowYAuto :: ClassName
overflowYAuto = ClassName "overflow-y-auto"

opacity25 :: ClassName
opacity25 = ClassName "opacity-25"

-- Grid
container :: ClassName
container = ClassName "container"

containerFluid :: ClassName
containerFluid = ClassName "container-fluid"

row :: ClassName
row = ClassName "row"

col :: ClassName
col = ClassName "col"

col6 :: ClassName
col6 = ClassName "col-6"

col12 :: ClassName
col12 = ClassName "col-12"

colSm8 :: ClassName
colSm8 = ClassName "col-sm-8"

colSm11 :: ClassName
colSm11 = ClassName "col-sm-11"

colMd3 :: ClassName
colMd3 = ClassName "col-md-3"

colMd4 :: ClassName
colMd4 = ClassName "col-md-4"

colMd6 :: ClassName
colMd6 = ClassName "col-md-6"

colMd8 :: ClassName
colMd8 = ClassName "col-md-8"

colMd9 :: ClassName
colMd9 = ClassName "col-md-9"

colLg3 :: ClassName
colLg3 = ClassName "col-lg-3"

colLg4 :: ClassName
colLg4 = ClassName "col-lg-4"

colLg6 :: ClassName
colLg6 = ClassName "col-lg-6"

colLg7 :: ClassName
colLg7 = ClassName "col-lg-7"

colLg8 :: ClassName
colLg8 = ClassName "col-lg-8"

colLg10 :: ClassName
colLg10 = ClassName "col-lg-10"

-- Buttons
btn :: ClassName
btn = ClassName "btn"

btnClose :: ClassName
btnClose = ClassName "btn-close"

btnDanger :: ClassName
btnDanger = ClassName "btn-danger"

btnGroup :: ClassName
btnGroup = ClassName "btn-group"

btnGroupSm :: ClassName
btnGroupSm = ClassName "btn-group-sm"

btnLg :: ClassName
btnLg = ClassName "btn-lg"

btnLight :: ClassName
btnLight = ClassName "btn-light"

btnLink :: ClassName
btnLink = ClassName "btn-link"

btnOutlineDanger :: ClassName
btnOutlineDanger = ClassName "btn-outline-danger"

btnOutlineInfo :: ClassName
btnOutlineInfo = ClassName "btn-outline-info"

btnOutlineLight :: ClassName
btnOutlineLight = ClassName "btn-outline-light"

btnOutlineSuccess :: ClassName
btnOutlineSuccess = ClassName "btn-outline-success"

btnOutlinePrimary :: ClassName
btnOutlinePrimary = ClassName "btn-outline-primary"

btnOutlineSecondary :: ClassName
btnOutlineSecondary = ClassName "btn-outline-secondary"

btnOutlineWarning :: ClassName
btnOutlineWarning = ClassName "btn-outline-warning"

btnPrimary :: ClassName
btnPrimary = ClassName "btn-primary"

btnSecondary :: ClassName
btnSecondary = ClassName "btn-secondary"

btnSm :: ClassName
btnSm = ClassName "btn-sm"

btnSuccess :: ClassName
btnSuccess = ClassName "btn-success"

-- Forms
formCheckInput :: ClassName
formCheckInput = ClassName "form-check-input"

formControl :: ClassName
formControl = ClassName "form-control"

formControlSm :: ClassName
formControlSm = ClassName "form-control-sm"

formLabel :: ClassName
formLabel = ClassName "form-label"

formText :: ClassName
formText = ClassName "form-text"

inputGroup :: ClassName
inputGroup = ClassName "input-group"

inputGroupSm :: ClassName
inputGroupSm = ClassName "input-group-sm"

inputGroupText :: ClassName
inputGroupText = ClassName "input-group-text"

-- Navbar
navbar :: ClassName
navbar = ClassName "navbar"

navbarBrand :: ClassName
navbarBrand = ClassName "navbar-brand"

navbarCollapse :: ClassName
navbarCollapse = ClassName "navbar-collapse"

navbarExpandSm :: ClassName
navbarExpandSm = ClassName "navbar-expand-sm"

navbarNav :: ClassName
navbarNav = ClassName "navbar-nav"

nav :: ClassName
nav = ClassName "nav"

navItem :: ClassName
navItem = ClassName "nav-item"

navLink :: ClassName
navLink = ClassName "nav-link"

navTabs :: ClassName
navTabs = ClassName "nav-tabs"

-- Dropdown
dropdown :: ClassName
dropdown = ClassName "dropdown"

dropdownItem :: ClassName
dropdownItem = ClassName "dropdown-item"

dropdownMenu :: ClassName
dropdownMenu = ClassName "dropdown-menu"

dropdownMenuEnd :: ClassName
dropdownMenuEnd = ClassName "dropdown-menu-end"

dropdownToggle :: ClassName
dropdownToggle = ClassName "dropdown-toggle"

-- Cards
card :: ClassName
card = ClassName "card"

cardBody :: ClassName
cardBody = ClassName "card-body"

cardFooter :: ClassName
cardFooter = ClassName "card-footer"

cardHeader :: ClassName
cardHeader = ClassName "card-header"

-- Modals
modal :: ClassName
modal = ClassName "modal"

modalBody :: ClassName
modalBody = ClassName "modal-body"

modalContent :: ClassName
modalContent = ClassName "modal-content"

modalDialog :: ClassName
modalDialog = ClassName "modal-dialog"

modalDialogCentered :: ClassName
modalDialogCentered = ClassName "modal-dialog-centered"

modalFooter :: ClassName
modalFooter = ClassName "modal-footer"

modalHeader :: ClassName
modalHeader = ClassName "modal-header"

modalTitle :: ClassName
modalTitle = ClassName "modal-title"

-- Tables
table :: ClassName
table = ClassName "table"

tableBordered :: ClassName
tableBordered = ClassName "table-bordered"

tableHover :: ClassName
tableHover = ClassName "table-hover"

tableSecondary :: ClassName
tableSecondary = ClassName "table-secondary"

-- Toasts
toast :: ClassName
toast = ClassName "toast"

toastBody :: ClassName
toastBody = ClassName "toast-body"

toastContainer :: ClassName
toastContainer = ClassName "toast-container"

-- List Groups
listGroup :: ClassName
listGroup = ClassName "list-group"

listGroupFlush :: ClassName
listGroupFlush = ClassName "list-group-flush"

listGroupItem :: ClassName
listGroupItem = ClassName "list-group-item"

listGroupItemAction :: ClassName
listGroupItemAction = ClassName "list-group-item-action"

listGroupItemPrimary :: ClassName
listGroupItemPrimary = ClassName "list-group-item-primary"

-- Pagination
pagination :: ClassName
pagination = ClassName "pagination"

pageItem :: ClassName
pageItem = ClassName "page-item"

pageLink :: ClassName
pageLink = ClassName "page-link"

-- Alerts
alert :: ClassName
alert = ClassName "alert"

alertDanger :: ClassName
alertDanger = ClassName "alert-danger"

-- Badges
badge :: ClassName
badge = ClassName "badge"

-- Progress
progress :: ClassName
progress = ClassName "progress"

progressBar :: ClassName
progressBar = ClassName "progress-bar"

-- Spinners
spinnerBorder :: ClassName
spinnerBorder = ClassName "spinner-border"

spinnerBorderSm :: ClassName
spinnerBorderSm = ClassName "spinner-border-sm"

-- Icons
bi :: ClassName
bi = ClassName "bi"

-- Misc
vr :: ClassName
vr = ClassName "vr"
