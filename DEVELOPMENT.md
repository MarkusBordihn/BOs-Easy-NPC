# Development Guide 🛠️🚀

This repository is intentionally split into three independent Gradle projects to cover all
build-system edge cases across different mod loaders and tooling:

- core - the main mod (game logic, data, and loader integrations)
- config-ui - the optional configuration UI mod, built against core
- bundle - a convenience meta package that declares core + config-ui as dependencies

Why split? A pure Gradle multi-project setup does not support all combinations of loader plugins (
Fabric Loom, ForgeGradle/NeoForge) and publication tasks at once.
Keeping the modules separate lets us:

- use loader-specific Gradle plugins without cross-plugin conflicts
- publish/consume artifacts cleanly via Maven Local
- iterate and release core and config-ui independently
- offer a meta package "bundle" for users who prefer a single dependency

## Project layout at a glance

Each of the three top-level folders is its own Gradle build with subprojects per loader:

- core/
    - Common/ - shared sources and assets
    - Fabric/ - Fabric-specific sources and Loom configuration
    - Forge/ - Forge-specific sources and tasks
    - NeoForge/ - available starting with 1.21.x
- config-ui/
    - Common/ - shared UI sources and assets
    - Fabric/
    - Forge/
    - NeoForge/ - available starting with 1.21.x
- bundle/
    - Fabric/ - meta package declaring core + config-ui dependencies
    - Forge/ - meta package declaring core + config-ui dependencies
    - NeoForge/ - meta package declaring core + config-ui dependencies

Note: The exact set of loader subprojects in your clone may vary by branch/version; check the folder
tree.
NeoForge targets start with Minecraft 1.21.x.

### Project overview

| Project   | Loader subprojects              | Group                             | Artifact prefix                         | Notes                        |
|-----------|---------------------------------|-----------------------------------|-----------------------------------------|------------------------------|
| core      | Common, Fabric, Forge, NeoForge | de.markusbordihn.easynpc          | easy_npc-<loader>-<mcVersion>           | NeoForge ≥ 1.21.x            |
| config-ui | Common, Fabric, Forge, NeoForge | de.markusbordihn.easynpc.configui | easy_npc_config_ui-<loader>-<mcVersion> | NeoForge ≥ 1.21.x            |
| bundle    | Fabric, Forge, NeoForge         | de.markusbordihn.easynpc.bundle   | easy_npc_bundle-<loader>-<mcVersion>    | Meta package (no jar-in-jar) |

## Artifact flow (Maven Local) 🔁

Artifacts are exchanged via your local Maven repository (~/.m2/repository) using Gradle's
mavenLocal() repository:

- Building core publishes core artifacts to Maven Local automatically.
- Building config-ui resolves core from Maven Local and then publishes config-ui to Maven Local.
- Building bundle resolves both core and config-ui from Maven Local and creates a meta package
  per loader (dependencies only, no jar-in-jar).

Coordinates (examples):

- Core group: de.markusbordihn.easynpc
    - Artifact pattern: easy_npc-<loader>-<mcVersion>
    - Example (Fabric): de.markusbordihn.easynpc:easy_npc-fabric-1.20.1:<version>
    - Example (Common, compileOnly): de.markusbordihn.easynpc:easy_npc-common-1.20.1:<version>
- Config UI group: de.markusbordihn.easynpc.configui
    - Artifact pattern: easy_npc_config_ui-<loader>-<mcVersion>
    - Example (Fabric): de.markusbordihn.easynpc.configui:easy_npc_config_ui-fabric-1.20.1:<version>
- Bundle group: de.markusbordihn.easynpc.bundle
    - Artifact pattern: easy_npc_bundle-<loader>-<mcVersion>
    - Example (Fabric): de.markusbordihn.easynpc.bundle:easy_npc_bundle-fabric-1.20.1:<version>

The exact version and groupId come from each project's gradle.properties.

## Required build order

Always build in this order so dependencies resolve from Maven Local:

1) core
2) config-ui
3) bundle (optional, only if you need the meta package)

Both core and config-ui are configured to run publishToMavenLocal after build, so simply building
them is enough to make artifacts available.

## Root-level helper tasks 🧰

The repository root contains helper tasks that orchestrate the three projects in order (core >
config-ui > bundle):

- **build** - builds all three projects sequentially
- **clean** - cleans all three projects sequentially
- **cleanMavenLocal** - deletes only this project's artifacts from Maven Local (~/.m2/repository)
  for the current version
- **cleanBuild** - cleanMavenLocal + build for all projects sequentially (recommended for most
  development)

**Performance tip:** Build caches (Gradle cache, Loom cache, Forge mappings cache) are kept intact
for maximum performance. Only Maven Local artifacts are cleaned when needed.
The build system uses intelligent dependency change detection (`changing = true` + 5-minute cache)
to automatically detect when core changes and config-ui/bundle need rebuilding.

**Note:** Publishing tasks (modrinth, curseforge) are available in individual project build files
but are intended for maintainers only, not general developers.

These tasks use Gradle's Tooling API to call each project's build independently, avoiding
multi-project plugin conflicts.

### Root tasks overview

| Task            | What it does                                                      | When to use                                 |
|-----------------|-------------------------------------------------------------------|---------------------------------------------|
| build           | Builds core > config-ui > bundle in sequence                      | Normal local builds                         |
| clean           | Cleans all three projects (calls each project's clean task)       | Before a full rebuild (keeps build caches!) |
| cleanMavenLocal | Deletes only project artifacts from Maven Local (current version) | When you suspect stale mavenLocal artifacts |
| cleanBuild      | cleanMavenLocal + build all projects sequentially                 | **Recommended for most development** (fast) |

**Note:** Build caches (Loom, Forge mappings, Gradle cache) are intentionally kept to maximize build
speed. Use individual project's `clean` task if you need to delete build directories.
| deepClean | Deletes all .gradle and build directories | Deep clean when caches are broken (
slow)    |
| publish | Runs per-project publishing (curseforge/modrinth) after a cleanBuild | Release
pipeline (requires tokens)          |

## Quick start (cross‑platform) 🚀

> [!IMPORTANT]
> First-time setup for reliable root tasks: Build each artifact at least once individually so that
> Maven Local contains fresh artifacts and loader toolchains are initialized.
>
> ```sh
> ./gradlew -p core clean build
> ./gradlew -p config-ui clean build
> ./gradlew -p bundle clean build
> ```
>
> After this initial setup, the root helper tasks work smoothly across the repository.

From the repository root:

- Build everything in the correct order

```sh
./gradlew cleanBuild
```

- Or build each project explicitly (without changing directories)

```sh
./gradlew -p core clean build
./gradlew -p config-ui clean build
./gradlew -p bundle clean build
```

- Rebuild with refreshed dependencies across all projects

```sh
./gradlew cleanBuildRefresh
```

- Delete all Gradle caches inside the repo (use only when necessary)

```sh
./gradlew deepClean
```

- Attempt publication (requires credentials/tokens in your environment)

```sh
./gradlew publish
```

Tip: If you only change core, rebuild core to refresh Maven Local, then rebuild the dependent
project (config-ui or bundle) that consumes it.

### Refreshing dependencies after Gradle or loader version changes

When you upgrade Gradle, Fabric Loader/API, Forge/NeoForge, mappings, or any loader plugin versions,
force a dependency refresh to avoid stale caches:

- Refresh all projects via the root helper task

```sh
./gradlew cleanBuildRefresh
```

- Or refresh only specific projects

```sh
./gradlew -p core clean build --refresh-dependencies
./gradlew -p config-ui clean build --refresh-dependencies
./gradlew -p bundle clean build --refresh-dependencies
```

**Note on internal dependencies:** The build system automatically detects changes in internal
dependencies (core → config-ui → bundle) within 5 minutes thanks to `changing = true` configuration.
You don't need `--refresh-dependencies` for internal module changes, just rebuild the modules in
order.

If you encounter persistent cache issues, manually delete the `.gradle` folders and rebuild.

## IntelliJ IDEA setup

Because each folder is an independent Gradle project, you can link them individually:

- File > New > From Existing Sources… (or the Gradle tool window > +)
- Select core/build.gradle, then repeat for config-ui/build.gradle and bundle/build.gradle
- IDEA will generate separate run configurations (for example, Fabric Client/Server) per project

This keeps loader-specific plugins isolated and avoids multi-project configuration clashes.

## Architecture and rationale

- core
    - Contains the gameplay/NPC logic and data
    - Split into Common (shared sources) + loader-specific subprojects (Fabric, Forge, NeoForge
      where applicable; NeoForge available ≥ 1.21.x)
- config-ui
    - Optional UI layer separated from core
    - Depends on core artifacts via Maven Local
    - Benefits: smaller core, optional UI for servers, independent versioning and releases (NeoForge
      targets available ≥ 1.21.x)
- bundle
    - Convenience meta package
    - Declares core + config-ui as dependencies (no jar-in-jar embedding)
    - Exists because many users prefer one file instead of managing dependencies via
      CurseForge/Modrinth launchers (NeoForge bundles available ≥ 1.21.x)

Benefits of the split:

- Clear separation of concerns: runtime logic vs. UI
- Independent release cadence for core and config-ui
- Smaller runtime for users who don't want the UI
- Loader-specific tooling without Gradle plugin conflicts
- Reproducible inter-project integration via Maven Local

## Running, testing, and game test tasks

Loader-specific run configs are generated by Loom/Forge tooling. Typical tasks include:

- core and config-ui projects expose run configurations like "Fabric Client/Server" in the Gradle
  tasks (and IDEA run configs)
- Game tests: runAllGameTests in core or config-ui delegates to the loader-specific game test tasks

Examples:

```sh
./gradlew -p core runAllGameTests
./gradlew -p config-ui runAllGameTests
```

## Troubleshooting 🔧

### I changed core but config-ui still uses an old version

1. Rebuild core: `./gradlew -p core build`
2. Wait 5 minutes (changing dependency cache window) OR run `./gradlew cleanMavenLocal`
3. Rebuild config-ui: `./gradlew -p config-ui build`

Or use the root helper: `./gradlew cleanBuild`

### After upgrading Gradle, loader, mappings, or plugins

Run with `--refresh-dependencies` to clear external dependency cache:

```sh
./gradlew -p core clean build --refresh-dependencies
./gradlew -p config-ui clean build --refresh-dependencies
./gradlew -p bundle clean build --refresh-dependencies
```

### IntelliJ runClient doesn't work

1. Stop all Gradle daemons: `./gradlew --stop` in each project folder
2. Reload Gradle projects in IntelliJ (↻ icon)
3. Make sure you're running from the individual project (e.g., config-ui), not the root
4. Ensure core artifacts are in Maven Local: `./gradlew cleanBuild` from root

### Gradle caches are corrupted

If Gradle caches are corrupted:

1. Stop all Gradle daemons by running `./gradlew --stop` in each project folder
2. Manually delete the `.gradle` folders in core, config-ui, and bundle directories using your file
   manager
3. Optionally also delete the `build` folders in each subproject (Common, Fabric, Forge)
4. Rebuild: `./gradlew cleanBuild`

## Support policy 📣

This repository is maintained as time permits.
Developer support cannot be provided directly.
For loader/tooling specifics, please refer to the official channels:

- Fabric: Fabric Loom, Fabric API, and Fabric Loader docs/support
- Forge: ForgeGradle/Forge documentation and community support
- NeoForge: NeoForge documentation and community support (targets ≥ 1.21.x)

Launchers like CurseForge or Modrinth will resolve the separate artifacts automatically.
