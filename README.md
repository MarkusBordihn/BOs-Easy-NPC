# 🗣 Easy NPC (1.21.1)

![Easy NPC Versions](http://cf.way2muchnoise.eu/versions/Minecraft_559312_all.svg)

[![Download on CurseForge](http://cf.way2muchnoise.eu/title/559312.svg)](https://www.curseforge.com/minecraft/mc-mods/easy-npc)
[![CurseForge Downloads](http://cf.way2muchnoise.eu/full_559312_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/easy-npc)

[![Download on Modrinth](https://img.shields.io/badge/dynamic/json?labelColor=black&color=grey&label=&query=title&url=https://api.modrinth.com/v2/project/CgGEe1h3&style=flat&logo=modrinth)](https://modrinth.com/mod/easy-npc)
[![Modrinth Downloads](https://img.shields.io/badge/dynamic/json?labelColor=black&color=grey&label=&suffix=%20downloads&query=downloads&url=https://api.modrinth.com/v2/project/CgGEe1h3&style=flat&logo=modrinth)](https://modrinth.com/mod/easy-npc)

[![Report an Issue](https://img.shields.io/badge/dynamic/json?label=Report%20an%20Issue%20%2F%20Bug%20%2F%20Crash%20%2F%20Feature%20Request&labelColor=black&color=grey&query=title&url=https://api.modrinth.com/v2/project/CgGEe1h3&style=flat&logo=github)][issues]
[![Open Issues](https://img.shields.io/github/issues/MarkusBordihn/BOs-Easy-NPC?style=flat&logo=Github&color=red)](https://github.com/MarkusBordihn/BOs-Easy-NPC/issues?q=is%3Aopen+%21label%3Aenhancement)
[![Closed Issues](https://img.shields.io/github/issues-closed/MarkusBordihn/BOs-Easy-NPC?style=flat&logo=Github)](https://github.com/MarkusBordihn/BOs-Easy-NPC/issues?q=is%3Aclosed)

[![Java CI with Gradle](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/gradle.yml/badge.svg?branch=1.21.1)](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/gradle.yml)
[![Run game tests on Fabric](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/game-test-fabric.yml/badge.svg?branch=1.21.1)](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/game-test-fabric.yml)
[![Run game tests on Forge](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/game-test-forge.yml/badge.svg?branch=1.21.1)](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/game-test-forge.yml)
[![Run game tests on NeoForge](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/game-test-neoforge.yml/badge.svg?branch=1.21.1)](https://github.com/MarkusBordihn/BOs-Easy-NPC/actions/workflows/game-test-neoforge.yml)

[![Wiki](https://img.shields.io/badge/dynamic/json?label=Wiki&labelColor=black&color=grey&query=title&url=https://api.modrinth.com/v2/project/CgGEe1h3&style=flat&logo=github)][wiki]

Easy NPC provides a simplified system for creating custom NPCs with dialogs, trading,
and interactions for map makers, RPGs, adventure servers, modpacks, and other mods.

This project is the **Easy NPC Bundle**, a launcher convenience package that installs the
required Easy NPC modules automatically.

This bundle does not contain the Easy NPC modules themselves.

## 📦 Installation Overview

Easy NPC is split into multiple modules.
The bundle exists to simplify installation via launchers that support automatic dependency
resolution.

### Option A: Launcher install (recommended)

- Install **Easy NPC Bundle**
- Your launcher will automatically install the required modules:
    - **Easy NPC Core**
    - **Easy NPC Config UI** (configuration tools and required networking)

### Option B: Manual install or copied modpacks

If you install mods manually or copy a modpack between instances, make sure the required modules are
present:

- **Easy NPC Core**
- **Easy NPC Config UI**

Tip: When sharing or copying a modpack, copy the full `mods/` folder to ensure all required jars are
included.

## ✨ Features

- Easy NPCs with dialogs, trading, and interactions
- Simple setup for map makers, servers, and RPG environments
- Configurable actions with debug options
- Trading support with configurable offers and conditions
- Skin configuration with player name and URL support
- Designed for both server and client environments
- API support and integration for other mods

## 📦 Mod Integration

### [Armourer's Workshop][armourers_workshop]

Basic integration for Armourer's Workshop to use skins from the library.

### [Epic Fight Mod][epic_fight_mod]

Built in support for Epic Fight is included since Easy NPC **6.4.x**.
No additional integration mod is required.

### [Immersive Melodies][immersive-melodies]

Basic integration for Immersive Melodies to use music instruments.

## Usage

Spawn an NPC using the corresponding spawn egg or via the summon command.

Use the predefined `/easy_npc` commands or the NPC config wand to open a graphical user interface.
The GUI allows you to configure dialogs, trading options, skins, actions, and other NPC related
settings directly in game.

Most interactions and adjustments can be done directly in game without editing files or restarting
the server.

## ⬆️ Upgrading

Please make sure to check the [upgrade guide][upgrade_guide] before upgrading/updating to a new
version.

## ℹ️ Why there is no jar-in-jar bundle anymore

Older versions used a jar-in-jar setup to bundle multiple Easy NPC modules into a single file.

This approach caused several technical issues:

- Duplicate mod IDs when modules were installed separately
- Class loading conflicts and ambiguous mod sources
- Incompatibilities when only the Easy NPC Core was required as a dependency
- Increased maintenance and support complexity

Modern launchers already provide dependency resolution.
Using this mechanism results in a cleaner, more predictable setup and allows Easy NPC modules to be
updated independently.

## ℹ️ More Information

Please check the [wiki][wiki] for additional information.

## 🐛 Report Issues, Bugs, Crashes or Feature Requests

Please report issues and feature requests over the [issues link][issues]. I'm happy to help you.

## 🧠 AI Assistance

AI-assisted tools are used to improve documentation, translations, and repetitive code sections.
This allows more time to be spent on feature development, maintenance, and long-term support.
All technical concepts, gameplay logic, and final assets are created manually.
See [AI ASSISTANCE](AI_ASSISTANCE.md) for full details.

## ⚖️ License

**This project is open source under the MIT License.**

⚠️ **Important:** The license applies **only to the source code** in this repository.

**Assets are excluded from the license:**

* 3D models (`.bbmodel` files)
* Textures and images
* Sounds and music
* Animations
* Other creative/artistic content

**These assets may not be redistributed, modified, or used in other projects without permission.**

For the full license text, see [LICENSE.md](LICENSE.md).

[wiki]: https://github.com/MarkusBordihn/BOs-Easy-NPC/wiki

[upgrade_guide]: https://github.com/MarkusBordihn/BOs-Easy-NPC/wiki/Upgrading

[armourers_workshop]: https://www.curseforge.com/minecraft/mc-mods/armourers-workshop

[epic_fight_mod]: https://www.curseforge.com/minecraft/mc-mods/epic-fight-mod

[easy_npc_epic_fight_mod]: https://www.curseforge.com/minecraft/mc-mods/easy-npc-epic-fight

[immersive-melodies]: https://www.curseforge.com/minecraft/mc-mods/immersive-melodies

[issues]: https://github.com/MarkusBordihn/BOs-Easy-NPC/issues
