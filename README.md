# 🗣 Easy NPC (1.19.2)

[![Easy NPC Downloads](http://cf.way2muchnoise.eu/full_559312_downloads.svg)](https://www.curseforge.com/minecraft/mc-mods/easy-npc)
[![Easy NPC Versions](http://cf.way2muchnoise.eu/versions/Minecraft_559312_all.svg)](https://www.curseforge.com/minecraft/mc-mods/easy-npc)

Easy NPCs allows a simplified setup for custom NPCs with dialogs for map maker and other mods.

**NOTE: The beta version is still undergoing changes and further testing before its official release. For this reason it will not be compatible with the final release version.**

## ✨ Features

- Providing easy NPCs with basic dialogs and a easy setup.
- Easy Action configuration with debug option.
- Skin configuration with player name and URL support.
- Server and client friendly.
- API support / integration for other mods (WIP).

## Usage

Just spawn a NPC with the corresponding spawn egg and click with your empty hand on the spawned NPC.
If you need to adjust the existing dialog, just click with your empty hand and crouching on the NPC.

## Dialogs

### Placeholders

The dialog text supports additional placeholders to allow more personalization for the text.

| Placeholder | Description                         |
| ----------- | ----------------------------------- |
| @npc        | Name of the NPC                     |
| @initiator  | Name of the initiator of the dialog |

### Basic Dialog

The basic dialog allows to display a simple text with max. 255 characters.

![Basic Dialog Setup screen](/examples/dialogs/basic_dialog_setup.png)

![Basic Dialog](/examples/dialogs/basic_dialog.png)

### Yes/No Dialog

The yes / no dialog allows to display a simple text with max. 255 characters, but additional with two buttons to provide an additional text depending on the answer.

![Yes/No Dialog Setup screen](/examples/dialogs/yes_no_dialog_setup.png)

![Yes/No Dialog](/examples/dialogs/yes_no_dialog.png)

### Advanced Dialog

WIP

## Actions

Actions allows you create more interactive parts with your NPC even outside of this mod.
Some basic examples:

- `/say Hello, ...`
- `/give @p stone`
- `/scoreboard players add @p close 5`
- `/tp @p -28 63 399`

The action are locked with the user permission level.
Which mean that a standard user will not be able to use /give /tp and other related commands for higher permission levels.

By enabling the debug action all executed commands will produce a corresponding feedback in the log like script errors and others.

### Basic Actions

The basic action screen allows to define actions for the basic dialog screen and the yes/no dialog screen like:

- Player opens dialog screen
- Player close the dialog screen
- Player select "Yes" from the "yes/no" dialog screen
- Player select "No" from the "yes/no" dialog screen

![Basic Action Setup screen](/examples/actions/basic_actions.png)

## NPC Types

### Humanoid NPC

Humanoid NPCs are the most common one and used for characters like Steve.

![Humanoid NPC](/examples/npcs/humanoid_npc.png)

### Humanoid Slim NPC

Humanoid slim NPCs are the most common one and used for slim characters like Alex.

![Humanoid Slim NPC](/examples/npcs/humanoid_slim_npc.png)

### Allay NPC

Allay NPCs using the Allay model with additional variants.

![Allay NPC](/examples/npcs/allay_npc.png)

### Fairy NPC

Fairy NPCs using a custom model and support three kinds of fairy's to far.

![Fairy NPC](/examples/npcs/fairy_npc.png)

### Villager NPC

Villager NPCs are the most common one and you can select from about 118 different villagers combinations.

![Villager NPC](/examples/npcs/villager_npc.png)

## License

The MIT [LICENSE.md](LICENSE.md) applies only to the code in this repository. Images, models and other assets are explicitly excluded.
