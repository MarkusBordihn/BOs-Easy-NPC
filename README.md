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
- No additional dependencies.
- API support / integration for other mods (WIP).

## Usage

Just spawn a NPC with the corresponding spawn egg and click with your empty hand on the spawned NPC.
If you need to adjust the existing dialog, just click with your empty hand and crouching on the NPC.

## Configuration Items

### Easy NPC Wand

With this tool, players can adjust the settings for EasyNPCs and select the desired NPCs without having to directly target them.

Normally, players have to aim directly at an NPC to select and configure it. With this tool, this process is simplified as it has a larger range and players can simply point at the ground or in the air to select an NPC.

## Dialogs

### Dialog Placeholders

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

- `/say Hello, @initiator ...`
- `/give @initiator stone`
- `/scoreboard players add @initiator close 5`
- `/tp @initiator -28 63 399`

The action are locked with the user permission level.
Which mean that a standard user will not be able to use /give /tp and other related commands for higher permission levels.

By enabling the debug action all executed commands will produce a corresponding feedback in the log like script errors and others.

### Action Placeholders

The action text supports additional placeholders to allow more personalization for the action.

| Placeholder | Description                         |
| ----------- | ----------------------------------- |
| @npc        | Name of the NPC                     |
| @initiator  | Name of the initiator of the dialog |

### Basic Actions

The basic action screen allows to define actions for the basic dialog screen and the yes/no dialog screen like:

- Player interacts with the NPC (without/with dialog)
- Player opens dialog screen
- Player close the dialog screen
- Player select "Yes" from the "yes/no" dialog screen
- Player select "No" from the "yes/no" dialog screen

![Basic Action Setup screen](/examples/actions/basic_actions.png)

## Delete an NPC

To remove an NPC open the corresponding config screen and click on the "Remove" button.

![Delete NPC](/examples/misc/delete_npc.png)

You need to confirm the removal with the "Yes" button to remove the NPC and all related data.

![Delete NPC Confirmation](/examples/misc/delete_npc_confirm.png)

## NPC Skins

The NPC skins can be configured with the corresponding config screen.

### Default Skins

The default skins provides a list of default (built-in) skins for the NPCs.
Select a skin from the list to use it for the NPC.

![Default Skin](/examples/skins/default_skin.png)

### Player Skins

The player skins allows to use the skin of a player for the NPC.
This option is only available for humanoids NPCs.

Just enter the name of the player and click on the "Add" button to add the corresponding skin.

![Player Skin](/examples/skins/player_skin.png)

### Remote URL Skins

The remote URL skins allows to use a remote URL for the skin of the NPC.
This option is available for all NPC types.

First you need to get the image URL of the skin image which could require to click on a additional button like "Image Link" or "Direct Link" to get the corresponding URL.

![Remote URL Image Link](/examples/skins/remote_url_image_link.png)

The URL should have a format similar like this: `https://www.example.org/uploads/skins/2019/06/09/kaworru-13064517.png?v538`

Copy and paste this URL it into the corresponding field in the config screen and click on the "Add" button to add the corresponding skin.

![Remote URL Skin](/examples/skins/remote_url_skin.png)

After the skin is processed you should see the corresponding skin in the config screen.
Easy NPC use a cache to store the downloaded skins to reduce the load on the remote server.

## Equipment

The equipment screens allows to add items to the NPC equipment slots which are rendered in the NPC hand or on the body.
Depending on the NPC type the available equipments slots are limited.

![Inventory](/examples/misc/equipment.png)

## Scaling

The scaling screen allows to scale the NPC model in the X, Y and Z direction.

![Scaling](/examples/misc/scaling.png)

## Pose

The pose screen allows to change the pose of the NPC, it's supports the basic poses, but not all models support all poses.

![Pose](/examples/pose/default_pose.png)

## Position

The position screen allows to change the position of the NPC, this is useful if you want to move the NPC to a different position.

![Position](/examples/misc/position.png)

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

### Skeleton NPC

Skeleton NPCs using a custom model and support the basic three kinds of skeletons.

### Villager NPC

Villager NPCs are the most common one and you can select from about 118 different villagers combinations.

![Villager NPC](/examples/npcs/villager_npc.png)

## Mod integration

### CarryOn

I added a basic block list for the CarryOn mod to exclude the NPC from being picked up.
If this is not working, please use the CarryOn config `carryon-server.toml` to add the NPCs to the block list instead.

```toml
[blacklist]
  ...
  #Entities that cannot be picked up
  forbiddenEntities = [..., "easy_npc:*"]
  ...
```

### Simple Quests

You can use the [Simple Quests](https://www.curseforge.com/minecraft/mc-mods/simple-quests) mod to create quests for the Easy NPCs.
The quests could be triggered over the action configuration screen of the Easy NPC.

## License

The MIT [LICENSE.md](LICENSE.md) applies only to the code in this repository. Images, models and other assets are explicitly excluded.
