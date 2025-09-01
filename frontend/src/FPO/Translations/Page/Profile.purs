module FPO.Translations.Page.Profile where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type ProfileLabels =
  ( "prof_accountEmail"
      ::: "prof_accountEmailHelp"
      ::: "prof_chooseResetMethod"
      ::: "prof_close"
      ::: "prof_confirmPassword"
      ::: "prof_errorOccurred"
      ::: "prof_failedToSaveUsername"
      ::: "prof_featureNotImplemented"
      ::: "prof_groupsAndRoles"
      ::: "prof_loginSuccessful"
      ::: "prof_newPassword"
      ::: "prof_orSeparator"
      ::: "prof_passwordMismatch"
      ::: "prof_passwordResetLinkSent"
      ::: "prof_passwordSecurity"
      ::: "prof_passwordStrengthHelp"
      ::: "prof_passwordUpdated"
      ::: "prof_profile"
      ::: "prof_resetPassword"
      ::: "prof_role"
      ::: "prof_rolesHelp"
      ::: "prof_sendResetLink"
      ::: "prof_sentResetLinkDone"
      ::: "prof_unsaved"
      ::: "prof_updatePassword"
      ::: "prof_userData"
      ::: "prof_usernameHelp"
      ::: "prof_usernameSaved"
      ::: "prof_you"
      ::: SNil
  )

enProfile :: Translation ProfileLabels
enProfile = fromRecord
  { prof_accountEmail: "Account email"
  , prof_accountEmailHelp:
      "This is your unique identifier for the account. It is not changeable."
  , prof_chooseResetMethod: "Choose how you want to reset:"
  , prof_close: "Close"
  , prof_confirmPassword: "Confirm password"
  , prof_errorOccurred: "An error occurred."
  , prof_failedToSaveUsername: "Failed to save the username."
  , prof_featureNotImplemented: "Feature not yet implemented."
  , prof_groupsAndRoles: "Groups & Roles"
  , prof_loginSuccessful: "Login successful"
  , prof_newPassword: "New password"
  , prof_orSeparator: "— or —"
  , prof_passwordMismatch: "Passwords do not match."
  , prof_passwordResetLinkSent: "Password reset link sent."
  , prof_passwordSecurity: "For security, your password isn't shown."
  , prof_passwordStrengthHelp:
      "Use 12+ chars with a mix of letters, numbers, and symbols."
  , prof_passwordUpdated: "Password updated."
  , prof_profile: "Profile"
  , prof_resetPassword: "Reset password"
  , prof_role: "Role"
  , prof_rolesHelp: "Roles control what you can do in each group."
  , prof_sendResetLink: "Send reset link to email"
  , prof_sentResetLinkDone: "Sent password reset link to "
  , prof_unsaved: "Unsaved"
  , prof_updatePassword: "Update password"
  , prof_userData: "User data"
  , prof_usernameHelp:
      "Click into the field to edit. Changes are local until you hit Save."
  , prof_usernameSaved: "Username saved successfully."
  , prof_you: "You"
  }

deProfile :: Translation ProfileLabels
deProfile = fromRecord
  { prof_accountEmail: "Konto-E-Mail"
  , prof_accountEmailHelp:
      "Dies ist Ihre eindeutige Kennung für das Konto. Sie kann nicht geändert werden."
  , prof_chooseResetMethod: "Wählen Sie, wie Sie zurücksetzen möchten:"
  , prof_close: "Schließen"
  , prof_confirmPassword: "Passwort bestätigen"
  , prof_errorOccurred: "Ein Fehler ist aufgetreten."
  , prof_failedToSaveUsername: "Fehler beim Speichern des Benutzernamens."
  , prof_featureNotImplemented: "Feature noch nicht implementiert."
  , prof_groupsAndRoles: "Gruppen & Rollen"
  , prof_loginSuccessful: "Login erfolgreich"
  , prof_newPassword: "Neues Passwort"
  , prof_orSeparator: "— oder —"
  , prof_passwordMismatch: "Passwörter stimmen nicht überein."
  , prof_passwordResetLinkSent: "Passwort-Reset-Link gesendet."
  , prof_passwordSecurity: "Aus Sicherheitsgründen wird Ihr Passwort nicht angezeigt."
  , prof_passwordStrengthHelp:
      "Verwenden Sie 12+ Zeichen mit einer Mischung aus Buchstaben, Zahlen und Symbolen."
  , prof_passwordUpdated: "Passwort aktualisiert."
  , prof_profile: "Profil"
  , prof_resetPassword: "Passwort zurücksetzen"
  , prof_role: "Rolle"
  , prof_rolesHelp: "Rollen steuern, was Sie in jeder Gruppe tun können."
  , prof_sendResetLink: "Reset-Link an E-Mail senden"
  , prof_sentResetLinkDone: "Link zum Zurücksetzen gesendet an "
  , prof_unsaved: "Nicht gespeichert"
  , prof_updatePassword: "Passwort aktualisieren"
  , prof_userData: "Nutzerdaten"
  , prof_usernameHelp:
      "Klicken Sie in das Feld, um es zu bearbeiten. Änderungen sind lokal, bis Sie auf Speichern klicken."
  , prof_usernameSaved: "Benutzername erfolgreich gespeichert."
  , prof_you: "Du"
  }
