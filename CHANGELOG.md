# Changelog for Easy NPC (1.18.2)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [Git Hub History][history] instead.

### 2023.03.05

- Added Easy NPC wand to select and configure NPCs without directly targeting them.
- Added position screen for easier positioning of NPCs.

### 2023.03.04

- **Breaking Change: Added custom data serialization to support more complex data types. Backup first before install!**
- Improved performance of server and client processing by using custom data serialization.
- Added basic pose support for Easy NPC entities.

### 2023.02.24

- Added basic CarryOn support to block unwanted pickup of NPCs.
- Improved documentation and added more examples.

### 2023.02.22

- Added scaling configuration.
- Replaced some free text options with enums.

### 2023.02.20

- Added equipment configuration and corresponding render support.

### 2023.02.19

- Added Interaction action which could be triggered without any dialog.
- Added Skeleton model and remote skins support.
- Unified configuration UIs and added translation keys.
- Improved yes/no dialog buttons to allow longer texts.
- Improved value validation and status updates.
- Improved texture manager for custom models.
- Remove test NPC spawn eggs.

### 2023.02.16

- Added Action configuration with permission level support.
- Added Action Debug possibility for permission level 1 and above.
- Improved npc rendering and general UI layout.

### 2023.02.12

- Refactored dialogs and UI based code for better performance.
- Refactored network protocols for using specific types instead of text.
- Added Skin Preview for default, player based and remote skins.
- Added Skin Manager to better handling of different skins and styles.
- Added possibility to adding Skin over username.
- Added possibility to adding Skin over remote url.
- Added possibility to remove NPC.
- Improved separation of client and server side handling.

### 2023.01.28

- Released first beta version for more detailed live testing.

[history]: https://github.com/MarkusBordihn/BOs-Easy-NPC/commits/
