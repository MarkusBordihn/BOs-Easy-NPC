# Development Guide 🛠️🚀

This repository is intentionally split into three independent Gradle projects to cover all
build-system edge cases across different mod loaders and tooling:

- core - the main mod (game logic, data, and loader integrations)
- config-ui - the optional configuration UI mod, built against core
- bundle - a convenience packaging that ships core + config-ui as a single loader-specific JAR for
  users who asked for "one file"

Why split? A pure Gradle multi-project setup does not support all combinations of loader plugins (
Fabric Loom, ForgeGradle/NeoForge) and publication tasks at once.
Keeping the modules separate lets us:

- use loader-specific Gradle plugins without cross-plugin conflicts
- publish/consume artifacts cleanly via Maven Local
- iterate and release core and config-ui independently
- offer a single-JAR "bundle" for users while keeping a clean, modular dev setup

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
    - Fabric/ - builds a fat JAR with core + config-ui for Fabric
    - Forge/ - builds a fat JAR with core + config-ui for Forge
    - NeoForge/ - available starting with 1.21.x

Note: The exact set of loader subprojects in your clone may vary by branch/version; check the folder
tree.
NeoForge targets start with Minecraft 1.21.x.

### Project overview

| Project   | Loader subprojects              | Group                             | Artifact prefix                         | Notes                          |
|-----------|---------------------------------|-----------------------------------|-----------------------------------------|--------------------------------|
| core      | Common, Fabric, Forge, NeoForge | de.markusbordihn.easynpc          | easy_npc-<loader>-<mcVersion>           | NeoForge ≥ 1.21.x              |
| config-ui | Common, Fabric, Forge, NeoForge | de.markusbordihn.easynpc.configui | easy_npc_config_ui-<loader>-<mcVersion> | NeoForge ≥ 1.21.x              |
| bundle    | Fabric, Forge, NeoForge         | n/a (consumes from mavenLocal)    | n/a (produces fat JAR per loader)       | NeoForge ≥ 1.21.x; convenience |

## Artifact flow (Maven Local) 🔁

Artifacts are exchanged via your local Maven repository (~/.m2/repository) using Gradle’s
mavenLocal() repository:

- Building core publishes core artifacts to Maven Local automatically.
- Building config-ui resolves core from Maven Local and then publishes config-ui to Maven Local.
- Building bundle resolves both core and config-ui from Maven Local and creates an "all-in-one" JAR
  per loader.

Coordinates (examples):

- Core group: de.markusbordihn.easynpc
    - Artifact pattern: easy_npc-<loader>-<mcVersion>
    - Example (Fabric): de.markusbordihn.easynpc:easy_npc-fabric-1.20.1:<version>
    - Example (Common, compileOnly): de.markusbordihn.easynpc:easy_npc-common-1.20.1:<version>
- Config UI group: de.markusbordihn.easynpc.configui
    - Artifact pattern: easy_npc_config_ui-<loader>-<mcVersion>
    - Example (Fabric): de.markusbordihn.easynpc.configui:easy_npc_config_ui-fabric-1.20.1:<version>

The exact version and groupId come from each project’s gradle.properties.

## Required build order

Always build in this order so dependencies resolve from Maven Local:

1) core
2) config-ui
3) bundle (optional, only if you need the single JAR)

Both core and config-ui are configured to run publishToMavenLocal after build, so simply building
them is enough to make artifacts available.

## Root-level helper tasks 🧰

The repository root contains helper tasks that orchestrate the three projects in order (core >
config-ui > bundle):

- build - builds all three projects sequentially
- clean - cleans all three projects sequentially
- cleanBuild - clean + build for all projects (ensures order)
- cleanCache - cleans and rebuilds all projects with `--refresh-dependencies`
- cleanGradleDirs - deletes all .gradle directories in the repo
- publish - after a cleanBuild, attempts publishing tasks for each project (modrinth, curseforge) if
  properly configured

These tasks use Gradle’s Tooling API to call each project’s build independently, avoiding
multi-project plugin conflicts.

### Root tasks overview

| Task            | What it does                                                         | When to use                             |
|-----------------|----------------------------------------------------------------------|-----------------------------------------|
| build           | Builds core > config-ui > bundle in sequence                         | Normal CI/local builds                  |
| clean           | Cleans all three projects                                            | Before a full rebuild                   |
| cleanBuild      | clean + build for all projects in correct order                      | Ensures order and a fresh build         |
| cleanCache      | clean + build with `--refresh-dependencies` for each project         | After upgrading Gradle/loaders/mappings |
| cleanGradleDirs | Deletes all .gradle directories in the repository                    | Deep clean when caches are broken       |
| publish         | Runs per-project publishing (curseforge/modrinth) after a cleanBuild | Release pipeline (requires tokens)      |

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
./gradlew cleanCache
```

- Delete all Gradle caches inside the repo

```sh
./gradlew cleanGradleDirs
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
./gradlew cleanCache
```

- Or refresh only specific projects

```sh
./gradlew -p core clean build --refresh-dependencies
./gradlew -p config-ui clean build --refresh-dependencies
./gradlew -p bundle clean build --refresh-dependencies
```

If issues persist, also clear Gradle’s local state inside the repo and rebuild:

```sh
./gradlew cleanGradleDirs
./gradlew cleanBuild
```

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
    - Convenience packaging only
    - Produces a single "all-in-one" JAR that embeds core + config-ui for a given loader
    - Exists because many users prefer one file instead of managing dependencies via
      CurseForge/Modrinth launchers (NeoForge bundles available ≥ 1.21.x)

Benefits of the split:

- Clear separation of concerns: runtime logic vs. UI
- Independent release cadence for core and config-ui
- Smaller runtime for users who don’t want the UI
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

## Troubleshooting

- I changed core but config-ui still uses an old version
    - Rebuild core to publish to Maven Local, then rebuild config-ui
    - If still stale: run the root task cleanCache to force `--refresh-dependencies` across all
      projects
- After upgrading Gradle, loader, mappings, or plugins
    - Run ./gradlew cleanCache or add `--refresh-dependencies` to the affected subproject builds
    - If problems remain: ./gradlew cleanGradleDirs and then ./gradlew cleanBuild
- Gradle caches are inconsistent
    - Run cleanGradleDirs to remove all .gradle folders in the repo
- Publication fails
    - Ensure credentials/tokens are provided (see gradle.properties placeholders) and you have the
      required permissions

## Support policy 📣

This repository is maintained as time permits.
Developer support cannot be provided directly.
For loader/tooling specifics, please refer to the official channels:

- Fabric: Fabric Loom, Fabric API, and Fabric Loader docs/support
- Forge: ForgeGradle/Forge documentation and community support
- NeoForge: NeoForge documentation and community support (targets ≥ 1.21.x)

For general usage, end users can use the bundle if they prefer a single JAR. Otherwise, launchers
like CurseForge or Modrinth will resolve the separate artifacts automatically.
