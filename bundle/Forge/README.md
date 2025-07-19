# Easy NPC Bundle (Forge)

This is a bundle version of Easy NPC that automatically includes both the core mod and config UI mod
using Jar-in-Jar technology.

## What's Included

This bundle contains:

- **Easy NPC Core**: Provides all NPC functionality
- **Easy NPC Config UI**: Provides configuration interface for NPCs

## Installation

1. Download the bundle JAR file
2. Place it in your `mods` folder
3. Start Minecraft - both mods will be automatically loaded

## Building

To build the bundle version:

```bash
./gradlew :bundle:Forge:build
```

This will create a JAR file that contains both mods embedded using Jar-in-Jar technology.

## Technical Details

- Uses Forge's Jar-in-Jar system to embed dependencies
- Creates a single JAR file containing both mods
- Automatically handles mod loading and dependency resolution
- Compatible with Minecraft 1.20.1 and Forge 47.3.11+

## Benefits

- **Simplified Installation**: Only one file to install instead of two
- **Automatic Compatibility**: Ensures both mods are compatible versions
- **Reduced Conflicts**: Eliminates potential version mismatch issues
- **Easy Distribution**: Single file for mod pack creators

## Development

The bundle mod itself is minimal - it just serves as a container for the two actual mods. The real
functionality comes from the embedded Easy NPC Core and Config UI mods.
