# User Management

The system contains at least one Superadmin, who can:

* Create users
* Delete users
* Edit users
* Grant Superadmin status to users
* Assign users to groups and assign roles (Admin or Member)

## User Creation

User creation is possible by a (Super) Admin.

When creating a user, an Admin enters a username, an email address and a temporary password.
Afterward, the user can log in and has to change their passsword.

The reasons that this is not done via a mail service is that at the time this was implemented,
we were still missing a mail service and had no time to revisit this after.


Admins create users within one of their groups, while a Superadmin can also create users without assigning them to a group.

### Users in Groups

An Admin has an overview of all users in their group and can:

* Create users within the group
* Grant and revoke Admin rights within the group
* Add existing users to the group
* Remove users from the group
* Create and delete new groups (Admins are authorized to do this, not just Superadmins)


## Role System

**Roles:** `Member`, `Admin`, `Superadmin`

An `Admin` is a `Member` with special privileges.
`Members` are assigned to `Groups`, while `Superadmins` operate globally.
`Admins` always hold their Admin rights in relation to a specific group.

### Example:

| Username | Group 1  | Group 2  |
| -------- | -------- | -------- |
| User123  | `Member` | `Admin`  |
| Admin4   | `Admin`  | `Member` |

## Role Permissions

Admins only have rights (for example, promoting users) within the groups where they are Admins.

|                        | Member | Admin | Superadmin |
| ---------------------- | :----: | :---: | :--------: |
| Create users           |    ❌   |   ✅   |      ✅     |
| Edit users             |    ❌   |   ✅   |      ✅     |
| Remove users           |    ❌   |   ✅   |      ✅     |
| Delete users           |    ❌   |   ❌   |      ✅     |
| View full user data    |    ❌   |   ❌   |      ✅     |
| Promote to Member      |    ❌   |   ✅   |      ✅     |
| Promote to Admin       |    ❌   |   ✅   |      ✅     |
| Promote to Superadmin  |    ❌   |   ❌   |      ✅     |
| View user list         |    ❌   |   ✅   |      ✅     |
| Create groups          |    ❌   |   ✅   |      ✅     |
| Delete groups          |    ❌   |   ✅   |      ✅     |
| Create documents       |    ❌   |   ✅   |      ✅     |
| Delete documents       |    ❌   |   ✅   |      ✅     |
| Change document rights |    ❌   |   ✅   |      ✅     |


## Password Reset System

To trigger a password reset, users can request a password reset over the website,
which will trigger a request to `/password-reset/request`. This will trigger a mail
containing a link with a one-time token to their mail, should that mail exist. The
link can be used to reset their password, as would be expected of such functionality.

Note that for this to work, a valid
[mail configuration](../configure.md#Mail-configuration-(optional)) has to be provided.
