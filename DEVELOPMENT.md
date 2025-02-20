# 🗣 Easy NPC - Development Guide

## 🛠 Setup Development Environment

Easy NPCs uses [Gradle](https://gradle.org/) as build system.
To make sure that the multi-module project is working correctly, please make sure to use a advanced
IDE like [IntelliJ IDEA](https://www.jetbrains.com/idea/).
Other IDEs like Eclipse or Visual Studio Code are not officially supported and may not work
correctly, but you can try to import the project as Gradle project.

## 📦 Project Structure

The project is split into multiple modules:

- `core`: Core mod with the main features.
    - `Common`: The common code shared between the different mod loaders.
    - `Forge`: The Forge specific code.
    - `Fabric`: The Fabric specific code.
- `config-ui`: The configuration UI for the mod.
    - `Common`: The common code shared between the different mod loaders.
    - `Forge`: The Forge specific code.
    - `Fabric`: The Fabric specific code.

In the end the 'core' and 'config-ui' modules are combined into a single mod for each mod loader.

- `bundle`: The combined mod for core and config-ui.
    - `Common`: The common code shared between the different mod loaders.
    - `Forge`: The Forge specific code.
    - `Fabric`: The Fabric specific code.

They are split into different modules to make it easier to maintain the code and to make it easier
to add new features or fix bugs.

Each of the modules are self-contained and can be built separately.

## 📚 Dependencies

The `core` module has no direct dependencies to other mods or modules.
The `config-ui` module directly depends on the `core` module for the shared code.
The `bundle` module directly depends on the `core` and `config-ui` modules for the shared code.

Most of these dependencies are managed by the build system over local Maven repositories.
This means you need to build the `core` module first before you can build the `config-ui` and
`bundle` modules.

Normally this is done automatically by the build system, but if you run into issues, you can build
the modules manually.

## 🛠 Building

To build the mod and their artifacts, you can use the following command in each of the modules:

```shell
./gradlew build
```

Note: You need to run this command in each of the modules like `core`, `config-ui` and `bundle`.
Most IDEs like IntelliJ IDEA allows you to link Gradle projects, so you can easily access them.

This will take some time, because it will download all dependencies and build the mod for all
supported mod loaders.
After the build is finished, the IDE should recognize the correct structure and you can start
developing.

If you receive an error like `Could not find de.markusbordihn.easynpc:easy_npc-forge-1.18.2:6.x.y.`,
make sure to build the `core` module first.

## Cleaning

If you want to clean the build artifacts, you can use the following command:

```shell
./gradlew clean build --refresh-dependencies
```

This will remove all build artifacts and dependencies and start a fresh build.
This can be useful if you run into issues with the build system or the dependencies.

## 🧪 Testing

To run the tests for the mod, you can use the following commands:

Core Game Test:

```shell
./gradlew -p core runAllGameTests
```

Config UI Game Test:

```shell
./gradlew -p config-ui runAllGameTests
```

Bundle Game Test:

```shell
./gradlew -p bundle runAllGameTests
```

Note: These commands will start the game with the test environment and run the tests.
They are not covering all tests, but they are a good starting point.
