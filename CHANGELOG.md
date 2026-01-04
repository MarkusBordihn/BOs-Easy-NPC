# Changelog for Easy NPC: Core (1.21.1)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [GitHub History][history] instead.

Note: Please always back up your world / NPCs before updating to a new version!
Check the [upgrade guide][upgrade_guide] for more information.

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

- Fixed #612 by re-validating entity types.
- Fixed #610 by force sync of hat and head layers for specific models.
- Fixed orc textures.
- Improved custom model support and performance, by moving related logic into to client side only.
- Improved caching of player to UUID mappings for player skins.
- Limited change model commands and logic to Doppler NPCs only.

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
