# Changelog for Easy NPC (1.18.2)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [Git Hub History][history] instead.

### 2023.09.03

- Added new `/easy_npc preset import_new <ResourceLocation> <x> <y> <z>` and `/easy_npc preset import_new <ResourceLocation> <x> <y> <z> <UUID>` command to allow the import a single preset multiple times.
- Improved action handling by passing block position and other additional information to the action.
- Improved preset import to allow import of NPCs on specific position e.g. `/easy_npc preset import <ResourceLocation> <x> <y> <z>`.

### 2023.06.09

- Added new distance based actions for 16, 8, 4 and 1 block distance from the NPC.
- Fixed issue to allow empty action to reset actions.

### 2023.05.29

- Added custom pose editor which supports rotation, position and visibility of the model parts.
- Added better model animation support for custom poses.
- Added better NBT format for easier editing and more complex data types.
- Added crouch pose for models which not support this by default.
- Improved import and export functionality for NPCs.
- Improved documentation and added more examples.
- Cleanup death code for better performance.

### 2023.05.20

- Added custom export and import functionality for NPCs over local files to allow easier sharing of NPCs.
- Added world export and import functionality for NPCs over world files to allow easier packing of NPCs.
- Added default presets for some common NPCs.
- Added basic knight skin.
- Improved UI and dialogs for better usability.
- Improved logging and error handling.
- Improved documentation and added more examples.

### 2023.04.10

- Fixed issue with custom texture skins.
- Improved custom texture screen for better usability.

### 2023.03.24

- Added config for basic permission system to control the use of specific options.
- Added support for normal players to be able to use Easy NPCs with limited options.
- Improved menu handling and general performance.

### 2023.03.19

- Added new Zombie and Zombie Villager models.
- Added custom texture support for individual texture skins for map makers.
- Added possibility to execute commands as player.
- Fixed locked rotation for custom models by resetting rotations.
- Fixed crash when using invalid numbers for position.
- Fixed issue with invisible NPCs.
- Improved documentation and added more examples.

### 2023.03.18

- Added support for custom model rotations and to lock rotations.
- Added support to disable dialog.
- Improved network protocol to support more complex data types.
- Improved NPC wand to easier select rotated and scaled NPCs.
- Refactored entity data for easier maintenance.
- Moved documentation to separate Wiki.

### 2023.03.11

- Added support for custom poses which allows to rotate the head, body, arms and legs.

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
