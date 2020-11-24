# Windows 10 Factoids

## Hidden Administrator account

* Launch PowerShell or cmd.exe in Administrator mode
  * Right-click and select "Run as Administrator"
* Set a password on the Administrator account
  (You will need to type the `*` to keep PW out of shell history)

  ```
     > net user Administrator *
     Type a password for the user:
     Retype the password to confirm:
     The command completed successfully.
  ```

* Enable the hidden administrator account

  ```
     > net user Administrator /active:yes
     The command completed successfully.
  ```

* Switch user and login as Administrator
* Switch back to your user administrator account
* Disable the hidden administrator account

  ```
     > net user Administrator /active:no
  ```

## Reapplying all Group Policy Settings

```
   > gpupdate /force
```
