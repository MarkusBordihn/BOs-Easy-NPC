# Changelog for Easy NPC (1.19)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [GitHub History][history] instead.

Note: Please always back up your world / NPCs before updating to a new version!

### 4.1.0

- Normalized all cache, skin, template and preset names to lowercase with additional checks for
  unsafe characters.
- Finalized orc model and texture.
- Fixed #123: By adding additional checks for the export and import of NPCs.
- Replaced custom TAGs with entity TAGs for better compatibility with other mods.
- Optimized custom and player texture manager.

### 4.0.1

- Improved stroll objectives for flying NPCs.
- Improved general navigation support for all NPCs.
- Fixed fairy fly navigation.
- Fixed fairy hitbox.
- Fixed orc hitbox.

### 4.0.0

Note: Please backup your world / npcs before updating from 3.x or earlier!
NPCs are now stored in a new format and the old format is not 100% compatible with the new version.

- Larger code refactoring for better code base.
- Added experimental support for Fabric.
- Added ability `Can be Leashed` to allow NPCs to be leashed.
- Added ability `Push entities` to save resources (if deactivated) and avoid pushing entities.
- Added `/easy_npc owner set <npc-uuid> <player>` command to change the owner of an NPC.
- Added `/easy_npc owner get <npc-uuid>` command to get the current owner of an NPC.
- Added experimental orc model, which is still work in progress.
- Fixed #33 and #86: Added additional NPC models.
- Fixed ##95: Added color support for NPC names.
- Fixed #101: Separated client and server entity data for better compatibility.
- Fixed #79 and #104: Added objective `Random Stroll around Home` to allow NPCs to randomly stroll
  around their home position.
- Fixed standard sword attack swing animation.
- Fixed standard golem attack swing animation.
- Updated presets to match the new format.

### 3.11.0

- Added `/easy_npc trading open <npc-uuid> <player>` command to open trading screens for specific
  players.
- Added `/easy_npc navigation set position <npc-uuid> <x> <y> <z>` command to set the position for
  the NPC.
- Fixed #97: Added automatic home point and `/easy_npc navigation set home <npc-uuid> <x> <y> <z>`
  command.
- Fixed #101: Added better support for [Project MMO][project_mmo] mod.
- Fixed #102: Added better support for [Better Combat][better_combat] weapons.
- Improved command block support.

### 3.10.0

- Refactored code for better performance and easier maintenance.
- Added item tooltips for equipment and trading configuration screens.
- Fixed #93: Added render support for equipped modded items and armors.

### 3.9.0

- Added `/easy_npc preset import` support for command blocks and functions with resource location.
- Added `/easy_npc dialog open <npc-uuid> <player> <dialog>` command to open the dialog screen for a
  specific NPC and player for #75.
- Fixed #87: Ignore damage even from creative players and ops.
- Fixed #85: Added de-spawn range option for the NPC Spawner.
- Fixed #84: Text format codes applying on more text that they should.
- Fixed #77: Added options to disable armor/equipment rendering for NPCs.
- Fixed #76 and #79: Added home position and `MoveBackToHome` for NPCs to allow them to return to a
  specific position.

### 3.8.0

- Added NPC Presets to allow easier creation and usage of NPC presets in-game.
- Added NPC Spawner to allow spawning of NPCs with specific settings and NPC presets.
- Added Debug Manager to minimize the logging and to allow better debugging.
- Added LivingEntityManager which will replace the old EntityNPCManager.
- Added Debugging.md to the wiki.
- Added text color and text style formatting for dialog texts. #80
- Added possibility to adjust light level for NPCs. #45 and #67
- Increased text length for simple dialogs and yes/no dialogs to 512 characters.
- Move ActionEventData and DialogData to common package for better performance.

### 3.7.3

Note: Please backup your world / npcs before updating from 6.x or earlier!
This update includes a lot of changes and new features and even if I try to support all former data
it could be that some of them are not compatible with older versions.

- Added transparent "None" skin option for better [Armourer's Workshop][armourers_workshop]
  compatibility and other mod support.
- Fixed issue with advanced and custom pose model rendering.
- Fixed issue with auto-selecting the correct dialog type within the configuration screen.
- Optimized model data for better performance and smaller data size.
- Completed EasyNPC wiki documentation for all features.

### 3.7.2

- Added Easy NPC position tool to allow easier positioning of NPCs.
- Added additional Easy NPC Spawn Eggs and entity types for mod integrations.
- Fixed bounding boxes for NPCs with custom models, poses and scaling.
- Fixed issue with cut-off dialog button text.

### 3.7.1

- Fixed dialog editor UI issue with the new dialog text editor.

### 3.7.0

- Restricted the edit behavior for NPC to allow more use-cases to the following:
    - User is sneaking and clicking on the NPC (creative mode only)
    - User is holding the NPC wand and clicking on the NPC (all modes)
- Added possibility to adjust sliders with mouse wheel and arrow keys for better usability.
- Added dialog text editor to allow multiple dialog texts variants for the same dialog.
- Added "can_use_nether_portal" attribute to allow NPCs to use nether portals or not.
- Added support for `@initator` with `onHurt` and `onDeath` events.
- Added support for [Armourer's Workshop][armourers_workshop] mod. Thanks to @SAGESSE-CN.
- Added possibility to disable model parts to the advanced pose editor.
- Separated the Player Skin and remote URL Skin screen for better usability.
- Fixed head and body rotation are not show correctly in some instances.
- Code optimizations and cleanup.

### 3.6.1

- Smaller bug fixes and improvements.

### 2023.11.22

- Added attack animation for bow, crossbow and sword.
- Added additional animations like celebrate, dance, spell casting for testing.
- Added crossbow attack goal.
- Added bow attack goal.
- Added missing translations.
- Improved base attribute screen for better usability.
- Improved melee attack goal.
- Fixed smaller issues.

### 2023.11.20

- Added base attribute screen for health, attack, armor, knockback resistance, movement speed, ...
- Added missing translations for some screens.
- Fixed smaller issues with the UI and dialogs.
- Improved skin selection for better usability.

### 2023.11.18

- Added attack and follow objectives.
- Added respawn button in the case the NPC is stucked or buggy.
- Improved objective validation and handling.
- Optimized nbt data to skip default values for better performance and smaller data size.

### 2023.11.05 (NPCs comes to life update)

Note: Please backup your world before updating to this version!
This update includes a lot of changes and new features and even if I try to support all former data
it could be that some of them are not compatible with older versions.

- Added attribute configuration like can open doors, can close doors, can pass doors, can be hurt,
  can be pushed, ...
- Added objectives configuration for like follow owner, follow player, follow entity, walk
  around ...
- Added custom dialog configuration for more complex dialogs.
- Added custom action events `on_hurt`, `on_death` and `on_button_click`.
- Added Iron Golem model.
- Refactored Dialog system for supporting more complex dialogs.
- Refactored Action system for supporting more complex actions.
- Refactored Action Event system for supporting upcoming events.
- Refactored NPC network data for sending only relevant data to clients.
- Fixed issue with importing filenames with upper-case letters.
- Fixed animation issue with custom poses.
- Fixed compatibility issue with resource packs like Stoneborn, Mythic, Creator Pack, ...
- Improved mod compatibility with other mods.

### 2023.10.24

- Added advanced trading configuration for NPCs with multiple items and prices.
- Added auto-reset for advanced and basic trading configuration.
- Added commands to open the trading screen and to reset the trading items.
- Added Skeleton Trader preset example for a dialog with trading options.
- Added better basic trading configuration.
- Fixed issue with importing legacy presets.
- Fixed issue with advanced and custom pose configuration.

### 2023.10.10

- Refactored UI and configuration system for easier support for upcoming features.
- Added basic (over UI) and custom (over NBT) trading configuration for NPCs.
- Fixed issue with custom model rendering.

### 2023.10.06

- Refactored model and render system for better performance and easier support for custom models.
- Refactored network protocol for better performance and 1.20.2 changes.
- Refactored data serialization (NBT) for upcoming features.
- Added cat and chicken model.
- Added position "freefall" option to allow NPCs to fall down and not to float in the air.
- Fixed remote skin support with to avoid failed skin downloads.

### 2023.09.03

- Added new `/easy_npc preset import_new <ResourceLocation> <x> <y> <z>`
  and `/easy_npc preset import_new <ResourceLocation> <x> <y> <z> <UUID>` command to allow the
  import a single preset multiple times.
- Improved action handling by passing block position and other additional information to the action.
- Improved preset import to allow import of NPCs on specific position
  e.g. `/easy_npc preset import <ResourceLocation> <x> <y> <z>`.

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

- Added custom export and import functionality for NPCs over local files to allow easier sharing of
  NPCs.
- Added world export and import functionality for NPCs over world files to allow easier packing of
  NPCs.
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
- Refactored Code for 1.19-41.1.0.

### 2023.03.05

- Added Easy NPC wand to select and configure NPCs without directly targeting them.
- Added position screen for easier positioning of NPCs.

### 2023.03.04

- **Breaking Change: Added custom data serialization to support more complex data types. Backup
  first before install!**
- Improved performance of server and client processing by using custom data serialization.
- Added basic pose support for Easy NPC entities.

### 2023.02.24

- Added basic CarryOn support to block unwanted pickup of NPCs.
- Fixed Allay scale rendering.
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

### 2023.01.29

- Added Allay NPC version and Lava variant.
- Refactored code for 1.19.2-43.2.4.

### 2023.01.28

- Released first beta version for more detailed live testing.

[history]: https://github.com/MarkusBordihn/BOs-Easy-NPC/commits/

[armourers_workshop]: https://www.curseforge.com/minecraft/mc-mods/armourers-workshop

[project_mmo]: https://www.curseforge.com/minecraft/mc-mods/project-mmo

[better_combat]: https://www.curseforge.com/minecraft/mc-mods/better-combat-by-daedelus
