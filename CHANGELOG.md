# Changelog for Easy NPC: Core (1.21.11)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [GitHub History][history] instead.

Note: Please always back up your world / NPCs before updating to a new version!
Check the [upgrade guide][upgrade_guide] for more information.

### 7.0.0

- Fixed #788 by extending the AI safeguards to Epic Fight and raw NPC variants (Piglin, Villager,
  Enderman, Creeper, Raider and Witch).
- Fixed #787, #531, #237 and #184 by adding a faction system with `/easy_npc faction` commands,
  per-NPC faction assignment and automatic scoreboard team sync.
- Fixed #787, #771 and #572 by adding a new `target` objectives tab to attack hostile factions or
  specific targets by player name, team, entity tag or UUID.
- Fixed #786 by adding gender and shiny model variants for Cobblemon NPCs, searchable via `female`
  or `shiny` in the model selection.
- Fixed faction combat between invulnerable NPCs by adding the new `Attackable by Factions` combat
  attribute.
- Fixed Cobblemon model selection previews for oversized species by scaling with Cobblemon's own
  profile scale instead of hitbox height.
- Improved the `Attackable by Players/Monsters/Factions` combat attributes to work independently of
  `Invulnerable`, each opening a targeted hole for its own attacker group.
- Added Easy Model Entities Support.
- Added faction manager and faction editor screens to create, color and delete factions and manage
  hostile relations, protected by the new `FACTION_MANAGEMENT` security feature.
- Added new `Misc` attributes tab as home for the NPC faction assignment.
- Added reusable warning and help icon components for inline hints with tooltips.
- Added additional unit and game tests for factions, target objectives and Cobblemon model
  variants.

### 6.25.0

- Fixed #782, #636, #622 by preserving data components during validations.
- Fixed #780 by caching remote texture downloads and reducing duplicate registrations.
- Fixed #767 by clamping attribute sliders against extreme values from other mods.
- Fixed #754 by adding custom data item matching for NPCs with actions and conditions.
- Fixed texture manager running in the wrong thread may cause crashes and deadlocks in >= 1.21.x.
- Fixed empty UUID handling for action conditions.
- Fixed execution limit reset for actions.
- Added texture registration queue to avoid multiple simultaneous downloads in the same tick.
- Added NPC coin, key and red token as consumable items for NPCs with actions and conditions.
- Added `Welcome Coin Trader` as example NPC preset with custom coin, key and red token item data.
- Added attribute safeguards against extreme values from other mods.
- Increased fallback max health value from 1024 to 4096 for high-health NPCs.
- Improved texture manager by reducing duplicate texture checks and registrations.

### 6.24.0

- Fixed #459, #775 by adding an option to block interactions with invisible NPCs.
- Fixed #774 startup crash related to optional Epic Fight and Cobblemon entity type maps.
- Fixed #773 by checking timed trade resets during base ticks and before opening trade screens.
- Fixed trade reset timing by increasing internal tickers correctly.
- Added time of day and weather conditions for #459.
- Added additional unit and game tests for trade resets, ticker handling and entity attributes.
- Added `Skywatch Scout` as example NPC preset with time of day and weather conditions.

### 6.23.0

- Fixed #774 by migrating isSunBurnTick error to 26.x.
- Fixed #772 by adjusting texture retries with 403 and 404 errors and lowering logging.
- Fixed #770 item rendering for Epic Fight NPCs (Evoker, Pillager, Vindicator and humanoid skins).
- Fixed #769 door loops with NPCs not reliably opening or closing doors.
- Fixed #760 by adding a dialog option and config setting (buttonConditionMode: LOCK or HIDE) to
  either lock or hide unavailable conditional dialog buttons.
- Fixed #709 by adding color picker popup for names with 32 colors and/or a manual RGB/hex value.
- Fixed NPC name color being reset when renaming without a specified color by preserving the
  existing name color.
- Improved the door movement attributes so "Pass Door" is automatically enabled while "Open Door"
  or "Close Door" is active.

### 6.22.0

- Fixed #766 by passing the full server player context to command execution for better compatibility
  with 3rd party mods and plugins that rely on specific player data.
- Fixed #764 by adding entity health, npc health conditions and NPC preset
  `Grukk, the Sparring Post` as example for using these conditions.
- Fixed #763 by making sure dialog button execution limits are now recorded.
- Fixed #758 by providing a example NPC preset named `One Wish Companion`.
- Fixed crashes related to 3rd party villager AI reading non-registered brain memories on Easy NPC
  villagers (e.g. PoiCompetitorScan / job_site) #756.
- Fixed Dialog buttons are not locked for server-side data as preparation for #760 and #644.
- Fixed #628 by adding conditional dialog opening support / command for better control over when
  dialogs are shown to players.
- Added brain AI for Piglin and Piglin Brute NPCs and registered the full vanilla brain memory set.
- Added additional guns to the gun tag for #757.
- Added a new "Open Dialog (Conditional)" action that opens a named dialog only if its conditions
  are met, while the existing "Open Dialog" action keeps opening the dialog unconditionally.
- Added new entity health and npc health conditions for actions, dialogs and dialogs buttons.
- Added Piglin, Piglin Brute and Zombified Piglin NPCs invulnerable, for consistency with other
  NPCs.
- Added a new "Knockback Immunity" combat attribute that prevents the NPC from being knocked back by
  melee or projectile attacks.
- Added a new "Explosion Immunity" combat attribute that makes the NPC immune to explosion damage
  and displacement (e.g. TNT, creepers, end crystals).
- Moved the "Projectile Collision" attribute to the Combat attribute screen, where it fits better
  thematically (previously on the Abilities screen).

### 6.21.0

- Fixed item attachment for Illagers in crossed-arms pose.
- Fixed broken illager crossbow attack goal.
- Fixed crashes related to 3rd party items which are not renderable as hand items #756.
- Fixed crashes related to 3rd party mixins trying to access non-existing model parts #756.
- Fixed log.info spam message by reducing log verbosity and adding additional checks for custom
  model parts.
- Improved EasyNPCModelManager performance.
- Refactored NPC renderers to remove default hand item layer.

### 6.20.0

- Fixed crashes related to 3rd mods trying to access non-existing NPCs goals.

### 6.19.0

Note: This update adjusted the dialog background images by 10x6 pixels, which may require users to
adjust their custom dialog backgrounds if they are using custom ones.

- Fixed #752, #130 by adding dialog button condition support to allow to disable buttons.
- Fixed #683, #215 by adding typewriter effect support for dialog text.
- Fixed #690, #309 by allowing conditions for dialog, dialog buttons and interactions.
- Fixed smaller UI issues with dialog screen layout and background.
- Refactored dialog background images by 10x6 pixels on each side.
- Refactored dialog screen layout to better fit the new background size and improve appearance.
- Added typewriter effect and client configuration for dialog text.
- Added condition symbols to indicate the condition status for dialog, buttons and interactions.
- Added amount field for item conditions to allow checking for specific amounts of items.
- Added `Threshold Warden` as example NPC preset with various conditions and actions.
- Added smaller bug fixes and optimizations related to dialog conditions and button states.

### 6.18.0

Note: This update changes the internal data format for conditions and actions,
which may reset / remove legacy existing execution-limit entries without stored IDs.

- Fixed #745 by adding conditions for actions to allow more complex and dynamic behavior.
- Fixed #652 by adding dialog button tooltip for longer texts.
- Fixed #687 by adding command value tooltip to show the full value.
- Added Experience Level conditions.
- Added Game-mode conditions.
- Added has item in inventory condition.
- Added has item in hand condition with (main hand and off-hand) support.
- Added player health condition.
- Added player tag condition.
- Added player team condition.
- Added Goldmere example NPC preset with various conditions and actions.
- Added negative condition support to allow negating conditions for more complex logic.
- Removed parameter pseudo IDs for `ActionDataEntry` and replaced them with random UUIDs.
- Refactored condition system to allow more condition types.

### 6.17.0

- Fixed #738 by adding additional tooltips for "Execute as player"
- Fixed to detailed logs by reducing log.info messages during startup and in-game.
- Added additional unit and game-tests.

### 6.16.0

- Fixed #732 by providing option to disable ESC key and close button for dialogs.
- Fixed #723 by avoid auto-close in specific cases like trading screens.
- Added general dialog configuration options to allow users to customize dialog behavior.
- Added avatar scaling and position offset options for dialogs.
- Added missing translation keys.

### 6.15.1

- Fixed #735 by extending the correct base class.
- Fixed several 26.1.2 specific issues from manual spawn testing.

### 6.15.0 (Trades, Player Heads, Epic Fight and QOL updates)

- Fixed #731 by resyncing species data, if species list is empty on the client.
- Fixed #730 by improving Epic Fight integration and enabled experimental features.
- Fixed #729 by making sure trade uses are stored correctly.
- Fixed #728 by adding custom head support for player heads.
- Fixed #689 by showing remaining trade uses and allow resetting manually and automatically.
- Fixed #218 and #132 with new trade configuration screen and better support for custom trades.
- Fixed trade specific edge by switching between basic and normal/advanced trading mode.
- Fixed dialog button text is truncated with tooltip showing the full text.
- Fixed Cobblemon animation issues by syncing animation state with Cobblemon Animation System.
- Added per trade specific actions to allow users triggering actions after buys.
- Added trade reset button to reset all trades easily.
- Added trade usage information to trade configuration screen.
- Added experimental feature state to allow users to manually enable experimental features.
- Added Griselda Grindstone as example trader with actions.
- Cleanup test files.

### 6.14.0

- Fixed #727 by making sure changes are synced to the client when changing values.
- Fixed #726 by adding additional allow lists for specific commands.
- Fixed #686, #389, #304, #211 by adding basic Cobblemon support with Cobblemon NPC.
- Fixed dialog misalignment and navigation issues by adjusting dialog sprites and screens.
- Fixed x and z root rotation by correcting rotation logic and pivot calculations.
- Added basic Cobblemon support with custom NPC type and model for testing.

### 6.13.0 (Security and Permissions Updates)

Notes: This update includes better security checks and permissions for multiplayer environments.
It allows to execute server commands which can be potentially dangerous if used with malicious
intent.

**The update splits the model root data and logic from the rest of the model part data and logic,
which mean it may reset the root rotation and scaling of existing NPCs to the default values.**

- Fixed #725 by separating model root data and logic from the rest of the model part data and logic.
- Fixed #722 by supporting .sbnt files as local presets.
- Fixed #675 by adding dedicated assets for dialog and config screen backgrounds.
- Fixed #605, #532, #529 with new security checks and permissions system.
- Fixed #469, #311 by adding experimental api documentation and examples for custom model support.
- Fixed double rendering of config screen background by removing redundant background rendering.
- Fixed Ghast and Slime hitbox and name tag position issues by adjusting bounding box.
- ⚠️ Added security checks to prevent potential exploits and allow safe usage in multiplayer
  environments.
- Replaced dynamic dialog screen background with a static one for better performance, customization
  and compatibility.
- Replaced dynamic config screen background with a static one for better performance.
- Refactored config-ui specific components and removed them from the core mod.
- Removed duplicated sprite sheets.
- Prepare permission system for possible Luck Perms integration in the future like #712.
- General code cleanup and optimizations related to config screen rendering and assets.

### 6.12.0

- Fixed #714 by adding additional checks for custom model part list based on vanilla models.
- Fixed #713 by reducing log verbosity with downgraded per-NPC INFO messages to DEBUG and
  consolidated redundant warning messages.
- Fixed #711 by reduced oversized startup/config log output.
- Fixed NBT validation guard to prevent unnecessary buffer allocations in production.
- Added model part name constants to EasyNPCModelManager for consistent usage across mixins.
- Improved RenderEntityTypeSupportConfig by enhancing filter logic and optimizing data structures
  for better performance and maintainability.
- Removed verbose debug logs that dump large data structures.

### 6.11.1

- Fixed #710 by improving backward-compatible parsing for recoverable trade data.

### 6.11.0

- Fixed #703 rendering issue with new custom layer system and hand renderings.
- Fixed #701 by adding additional safeguards and logging for invalid trading offers.
- Fixed smart animation not working properly with modified head position.
- Fixed head tracking by moving player detection back to server side-only.
- Fixed CustomLookAtPlayerGoal to better handle edge cases and reduce jitter.
- Fixed issue with Doppler NPCs keeping their hand inventory.
- Improved PoseManager logging message.
- Updated logo with a new design.

### 6.10.0

- Fixed #698 by adding try and catch for 3rd party entity creation.
- Fixed #695 by clearing model part rotations/positions when switching to a `DEFAULT` vanilla pose.
- Fixed #695 by applying lock rotation and pose animation only when the NPC is idle.
- Fixed #695 by correcting root rotation pivot to use actual bounding box height instead of 0.5f.
- Fixed multiple objective registration issues including inverted `isTargetedPlayer` check, wrong
  UUID in leave-handler, missing player-targeted refresh on join/leave, and retry for offline
  targets.
- Fixed goal reference being retained after objective is unregistered.
- Removed auto-lock of ROOT rotation when loading poses; root lock is now user-controlled.
- Added lock rotation checkbox to the advanced and custom pose configuration screens.
- Added model-specific pose key filtering in `ConfigurationMenuHandler` for the default pose screen.
- Added unit tests for `CustomPosition`, `CustomRotation`, `CustomScale`, `ModelPose`,
  `ModelAnimationBehavior` and `ModelAnimationData`.
- Added `NPCDataIsolationTestHelper` and game tests for Fabric and Forge.
- Increased max head yaw range from 60° to 65°.

### 6.9.0

- Fixed #692, #666 by adding caching and safeguards to prevent redundant retriggering of NPC entity
  data updates.
- Fixed #651, #617 by adding basic custom poses for all supported NPC types.
- Fixed #597 by adding additional Flee goals for fleeing from players, villagers, monsters, ...
- Fixed pose loading to read directly from mod resources instead of copying files to disk.
- Fixed duplicate `saveNPC` calls on entity join by checking existing registry entry first.
- Added `despawn` and `spawn` commands to remove and re-spawn NPCs by UUID with configurable
  removal reasons.
- Added `ModelPoseAPI` and `EasyNPCEntityHandler` public API classes for controlling NPC poses and
  managing NPCs programmatically from external mods.
- Added pose data files for all supported NPC types.
- Added lock rotation checkbox to the basic pose configuration screen.
- Added `MoveToPositionGoal` to move an NPC to a position before executing a callback action.
- Added `EasyNPCLookControl` and `EasyNPCBodyRotationControl` to respect locked root rotation.
- Added synced owner change and dimension change to the NPC entity data index.
- Added EasyNPCItemAttachmentLayer into various entity renderers.
- Added dedicated `Flee Objective` configuration tab with objectives for fleeing creepers,
  monsters, mobs, players, villagers and the sun.
- Added `FOLLOW_ITEM` objective to the follow objectives tab to make NPCs follow item entities
  by resource location (e.g. `minecraft:apple`).
- Added `LookAtEntityByUUIDGoal` and enabled the look-at-entity-by-UUID and look-at-owner
  objectives in the look objectives screen.
- Added `persistent` flag to `SynchedDataIndex` so that transient indices (e.g. crossbow charge,
  model animation) no longer trigger dirty-save marking.
- Added configurable `customParticlesEnabled` flag to `SlimeBase` for API consumers.
- Added `SoundType.PET` and mapped `CAT_PURR` to the cat NPC for tamed ambient sound variety.
- Reduced log verbosity for periodic NPC save operations from INFO to DEBUG.

### 6.8.3

- Fixed #680 by enforcing version miss-match by upgrading network protocol version.
- Fixed #679 by resetting position, rotation and size for name tag.
- Fixed #665 by adding multi state slide for root rotations.
- Fixed #664 by adding option to disable following mouse cursor for easier posing.
- Fixed #663 by improve support for X and Z root rotations.
- Fixed #661 by adding quick rotation with NPC wand and sneaking to the NPC to rotate the root to
  face the player.
- Fixed move tool is wrongly triggering interactions by adding check for move tool in interaction
  handler.
- Fixed Preset Item is wrongly triggering interactions by adding check for preset item in
  interaction handler.
- Fixed Slime and Ghast hitbox and name tag position issues by adjusting bounding box and eye height
  calculations.
- Fixed Slime and Ghast GUI position by adjusting offset values.

### 6.8.2

- Fixed #677 by extending `SafeMerchantData` with notifyTrade to avoid crashes.
- Added progression data support for leveling up NPCs based on player interactions and actions.
- Added global NPC tracking system for better management and debugging of NPCs across the world.
- Added `OriginalModelConfig.withVariantTexture()` option to allow using variant textures with the
  original model.

### 6.8.1

- Fixed #676 by using existing preset helper method instead of custom one.

### 6.8.0 (Adding API support)

- Fixed #667 by adding warning messages and additional checks for invalid network packets.
- Fixed Horse spawn eggs model.
- Fixed pose support for allay, chicken, creeper, fox, ghast, horse, illager, iron golem, slime and
  vex.
- Added NPC base classes for better API support.
- Added NPC raw classes for advanced API support.
- Added Slim and Ghast NPC types.
- Added Custom Model API for easier integration with other mods and custom models.
- Refactored internal data handling and registration for better maintainability and future
  improvements.

### 6.7.1

- Fixed #657 and #656 by adding additional client side checks for vanilla bug.

### 6.7.0

- Fixed #650 by implementing Wolf Leg Animations in setupAnim method.
- Fixed #648 by fixing canBeHitByProjectiles translation.
- Fixed #645 by adding `allowBypassInvulnerability` config option to allow/deny bypassing
  invulnerability for NPCs.
- Fixed delete button showing label even with small width.
- Fixed up and down buttons not positioned correctly in some cases.
- Fixed name tag showing up for dialog and configuration UI.
- Fixed distance action issue were multiple actions are not properly triggered.
- Fixed issues with NPC presets.
- Fixed Cat NPC and Wolf NPC owner data not syncing properly.
- Added preset browser for easier selection of common NPC presets.
- Added preset .snbt (text) export and import functionality for sharing NPC presets.
- Refactored existing NPC presets to use new .snbt format.
- Refactored spawner system to use new preset format for better maintainability and future
  improvements.

### 6.6.2

- Fixed #643 by rework scaling system.
- Fixed scaling not properly updating hitbox and nametag position after restart / reload.
- Added MID and MOUSE_OVER name tag visibility modes for finer control over NPC name display.
- Added team-based name tag visibility support respecting vanilla Team.

### 6.6.1

- Fixed #638 by refactored internal data handling for display attributes.

### 6.6.0

- Fixed #634 by implementing asynchronous texture loading with dedicated thread pool.
- Fixed texture loading blocking render thread causing game freezes.
- Fixed race conditions in texture reload protection using atomic operations.
- Fixed resource leaks in HTTP connections during remote texture downloads.
- Fixed URL validation spam allowing multiple simultaneous downloads of the same texture.
- Fixed exception handling for remote image validation preventing crashes on invalid URLs.
- Fixed `defineId called for:` warning messages during NPC loading.
- Refactored texture loading architecture with multi-level defense and rate limiting.
- Refactored entity data registration logic for better maintainability.
- Converted data classes to modern Java records for better immutability and thread-safety.
- Added thread-safe session server spam protection with ConcurrentHashMap.
- Added comprehensive error recovery with automatic cooldown reset on failures.
- Improved texture loading with 2-thread pool and 500ms rate limiting.
- Improved exception handling with specific catch blocks for IIOException and FileNotFoundException.

### 6.5.2

- Fixed Villager profession and job skin issue.
- Improved GitHub workflows by adding cache for gradle dependencies.
- Improved Gradle build time and cleanup tasks.

### 6.5.1

- Fixed #632 by implementing ON_KILL action type and event.
- Fixed #629 by improving texture handling performance, caching, and memory usage.
- Fixed kill command is not working on NPCs.
- Fixed texture reload protection preventing cache reload after eviction.
- Fixed WebP validation bug in remote image validator (missing return statement).
- Refactored texture handling logic for better maintainability and future improvements.
- Added time-based reload protection (60 seconds) to replace permanent blocking mechanism.
- Added missing variant textures for chicken, pig and wolf.
- Added additional unit tests for texture handling.
- Improved gradle build tasks.

### 6.5.0

- ⚠️ Removed jar-in-jar bundle approach for better mod compatibility and api capabilities.
- Fixed #627 scissor implementation for better compatibility with other mods.
- Fixed #625 lively animation issues when using rotated or moved model parts.
- Fixed default animation are canceled when using scaled model parts.
- Added better pose animation control with smart, default und none options.

### 6.4.1

- Fixed #626 screen switching logic for different NPC UUIDs, thanks to `Spawnblade` for the detailed
  investigation and fix suggestion.

### 6.4.0

- Fixed #626 by improving dialog data validation and error handling.
- Fixed #623 by refactoring render data and render handling.
- Fixed #622 by making sure custom data are properly saved and loaded.
- Fixed open dialog action type to allow opening dialogs from other NPCs.
- Fixed hashing issues by adding missing equals and hashCode methods for ConditionDataSet and
  ActionDataSet.
- Added custom data test item for #622 and easier testing of custom data.
- Improved records and fixed potential issues with missing data.

### 6.3.0

- Refactored config ui specific components and removed them from the core mod.
- Fixed dialog data by filtering dialog data before sending to the client.
- Fixed dialog editor layout issues and improved usability.
- Fixed missing default values for some dialog data fields.
- Added dialog priority support to control the order of dialog execution.
- Added condition support for dialog and scoreboard actions.
- Added frequency support for dialog to limit how often an dialog can be shown.
- Added new NPC preset to for scoreboard and condition support.
- Improved dialog button data format by removing redundant fields.

### 6.2.0

- Fixed legacy custom name parsing.
- Fixed string injection within dialogs.
- Fixed missing translation for action types.
- Added scoreboard action type to increase, decrease, set scoreboard values.
- Added `@score(...)` NPC macro to display scoreboard values in the dialog.
- Added additional unit tests.
- Improved spin button with indicators and better usability.

### 6.1.2

- Fixed container and menu sync issues with missing close container packets.
- Fixed close button not working in some cases.
- Fixed jumping mouse cursor between screen transitions.
- Fixed translation files and removed duplicate and deprecated entries.
- Moved Easy NPC wand to existing core item tab instead of creating a new one.
- Added cat pose support for different model parts (except tail).
- Improved EasyNPCWand glowing effect performance by disabling it when not needed.
- Improved custom entity detection by excluding non-living entities like displays, makers, throwns
  and spawners.
- Improved cat variant handling.

### 6.1.1

- Fixed #615 by checking if item is equipped in one of the hands.
- Fixed #612 by re-validating entity types.
- Fixed #610 by force sync of hat and head layers for specific models.
- Fixed orc textures.
- Fixed cat variant handling for 1.21.5.
- Fixed player render by adjusting render state `skin` field.
- Fixed cat renderer by adjusting render state `texture` field.
- Fixed position, scaling and rotation preview.
- Fixed Fairy model.
- Improved custom model support and performance, by moving related logic into to client side only.
- Improved caching of player to UUID mappings for player skins.
- Limited change model commands and logic to Doppler NPCs only.
- Added renderEntityRaw method to render NPC as-it-is.

### 6.1.0 ✨

This is a major release. Please back up your worlds and NPC data before updating.
This version contains many improvements, optimizations, and internal changes that
are **not fully compatible** with earlier releases.

⚠️ Breaking changes

- Existing NPCs and their configuration data from versions before 6.1.0 may not
  load correctly or may require manual adjustments.
- Internal data formats and some behaviors have been refactored to support new use-cases.

🧩 New modular structure

Easy NPC is now split into three separate mods:

- **Easy NPC** – Bundle that includes both Core and Configuration UI for a
  plug‑and‑play experience.
- **Easy NPC: Core** – Lightweight runtime and logic for NPCs, with minimal
  dependencies. Intended for servers, modpacks, and developers.
- **Easy NPC: Configuration UI** – Standalone configuration interface for
  creating and editing NPCs. Can be installed on clients that need the UI.

🎯 Why this change?

- Allows servers and modpacks to ship only the **Core** mod on the server side
  for a leaner setup.
- Reduces memory usage and load times when the UI is not required everywhere.
- Makes development and testing faster by separating UI and core logic.

[history]: https://github.com/MarkusBordihn/BOs-Easy-NPC/commits/

[upgrade_guide]: https://github.com/MarkusBordihn/BOs-Easy-NPC/wiki/Upgrading
