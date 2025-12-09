# Changelog for Easy NPC: Config UI (1.21.5)

## Note

This change log includes the summarized changes.
For the full changelog, please go to the [GitHub History][history] instead.

Note: Please always back up your world / NPCs before updating to a new version!
Check the [upgrade guide][upgrade_guide] for more information.

### 6.1.1

- Added Entity Type Validator to avoid issues with displaying custom models.

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
