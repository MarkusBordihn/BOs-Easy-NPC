# Changelog for Easy NPC: Core (26.2)

## Note

This change log includes the summarized changes. For the full changelog, please go to
the [GitHub History][history] instead.

Note: Please always back up your world / NPCs before updating to a new version!
Check the [upgrade guide][upgrade_guide] for more information.

### 7.7.7

- Fixed #821 by keeping the transparency of 64x64 skins, which Fresh Moves needs for its eyes. Skins
  downloaded before need a reload over the button in the player skin screen.
- Fixed #818 by running trade actions for Villager and Wandering Trader NPCs as well.
- Fixed a dialog with several text variants switching its speech bubble size every second.
- Fixed the skin cache storing the processed image instead of the original download.
- Fixed the untranslated interval names, like `PER_DAY`, of the "Execution Limit" condition.
- Fixed the `sit` animation of Easy Model NPCs, which never used the built-in sitting pose.
- Fixed the speech bubble being cut off by the head of an NPC when a player stood close to it.
- Changed speech bubbles to keep their screen size and to move down or aside when a player is close.
- Changed the speech bubble text to render 20% larger than a name tag.
- Changed a time based action set with a "Wait" action to run in order instead of picking one entry.
- Changed dialog texts to wrap later, so an existing dialog can switch to the wider screen layout.
- Changed fallback actions to skip screen actions without a player, like the regular actions do.
- Changed the minimum Easy Model Entities version to 2.1.0, older versions disable the integration.
- Added a "Wait" action, which pauses the following actions of an event for a chosen duration.
- Added the "Countdown Timekeeper" preset, a speech bubble count to ten ending in sound and sparks.
- Added scaling and rotation for Doppler NPCs behind the "Experimental Features" checkbox. Both only
  affect the visible model of entities that accept it, and never the hitbox.
- Added a "Sound" action, which plays a sound at the NPC with sound category, volume and pitch.
- Added "Every 5 Minutes" and "Every 15 Minutes" to the "Execution Limit" condition.
- Added a "Player Idle" condition, which checks how long a player has not moved.
- Added an "Immovable" attribute, which keeps an NPC in place against pushing and knockback.
- Added `/easy_npc pause` to pause a single NPC or all NPCs; the global pause ends with the server.
- Added API access to the sound action, the pause state and the idle time of a player.
- Added a texture override per model slot for Easy Model NPCs over `/easy_npc render texture`.
- Added a "Repeat" playback mode with a count and a time limit to the "Play Animation" action.
- Added API access to animation variants, texture slots and textures of Easy Model NPCs.

### 7.6.0

- Fixed unclear mod conflict errors for duplicate entity data tracker IDs.
- Fixed third-party health bars rendering over NPC previews in configuration and dialog screens.
- Fixed Doppler and Easy Model Entities NPCs using default hitboxes instead of their model size.
- Fixed the `/easy_npc` commands suggesting no NPC UUIDs at all on a server.
- Fixed the pose list of the "Set Pose" action drawing over its input field.
- Fixed the untranslated "Loop", "After current" and "Action Data Type" labels of the action editor.
- Fixed the missing hint text of the "Chance" condition.
- Fixed long condition hints running out of the editor screen instead of wrapping.
- Fixed the misaligned help icons and text buttons of the "Message" action.
- Fixed changes to the "Set Pose", "Play Animation" and "Stop Animation" actions not being saveable.
- Fixed the speech bubble text sitting closer to its upper border than to its lower one.
- Changed the NPC suggestions to list the crosshair NPC first and only own NPCs without permissions.
- Changed a preset export to only store what differs from a new NPC, without position or owner.
- Changed a preset without objectives or actions to fall back to the defaults of its NPC type.
- Changed events without a player, like "On Spawn" or "On Kill", to check their conditions. An
  action there with a player based condition, for example a scoreboard or item, no longer runs.
- Changed a time based action to pick the owner as `@initiator` whenever the owner is nearby.
- Changed speech bubbles to stay visible from 5 up to 18 seconds, depending on the text length.
- Changed the "Set Pose" action to only show its input field for the "custom" pose entry.
- Added time based actions on five fixed steps, from every ~1 second up to every ~15 minutes.
- Added a "Message" action for chat, system message and speech bubble text without a command.
- Added text variants, a sender name, a recipient and translation keys to the "Message" action.
- Added the speech bubble graphic as `easy_npc:textures/gui/speech_bubble.png` for resource packs.
- Added a "Chance" condition in percent and a "Relationship" condition for owner and faction.
- Added spawn, owner login, state change, day / night, weather and the missing "On Trade" events.
- Added a base preset per NPC type under `easy_npc:api/preset/base/`, usable as a parent preset.
- Added the "Wandering Companion" preset, a talkative NPC built from time based messages.
- Added an action registry, an action handler, a preset validator and richer event data for mods.
- Added a tooltip with name, type and distance to every NPC suggestion of the `/easy_npc` commands.
- Improved the NPC suggestions, which appear without typing and tolerate a near miss of one block.

### 7.5.0

- Fixed #814 by giving every Cobblemon NPC its own animation state instead of one per species.
- Fixed #813 by letting "Move back to Home" work at any distance instead of only near its home.
- Fixed #812 by checking once per session whether a player changed their skin.
- Fixed #810 by accepting Cobblemon aspects of resource packs instead of only "shiny" and "female".
- Fixed the home position of an NPC always reading as unset, which hid "Home" in the configuration
  screen, skipped it on save and stopped "Move back to Home" and "Stroll around Home" from working.
- Fixed NPCs spawned from a preset, the preset browser or a respawn never getting a home position.
- Fixed an NPC spawned from the preset browser keeping the home position of the world the preset was
  exported from instead of using its spawn position.
- Fixed NPCs being drawn as a white silhouette with a shadow in the configuration, skin and preset
  screens while they were highlighted by the Easy NPC Wand.
- Fixed the server crashing when an NPC with a "Tempt" objective ticked its goals.
- Fixed a single or boss spawner spawning without any limit when its preset carried no stored NPC.
- Fixed the "Legacy Easy NPC Data" warning appearing for newly created NPCs.
- Fixed missing transparency corrections for old 64x32 skins, which showed layer artifacts.
- Fixed the player skin screen freezing for a moment while a player name was looked up.
- Fixed the "Reset look at ..." objective label, which read "Reset loot at ..." in some languages.
- Fixed the "No Gravity" attribute being ignored after a preset import, which let NPCs fall.
- Fixed the Fabric game tests, which crashed on start because of the development helper mods.
- Fixed NPCs losing their owner when they were spawned again after a despawn.
- Fixed objectives being deleted from an NPC when the mod providing them was missing at start.
- Fixed importing a preset without a stored id, which spawned an NPC without owner and home.
- Fixed "Follow owner" and "Look at owner" keeping their old target after the owner was changed.
- Fixed attack targets staying assigned to an NPC after the targeted entity was gone.
- Fixed the "Look at Item" objective, which could be stored but never did anything.
- Fixed walking animation for Doppler NPC and 3rd party models.
- Fixed Forge never running its server start-up step, which left NPC tracking, factions and the
  custom identifier index empty for the whole session.
- Fixed Forge development runs starting without any mixins, which disabled the vanilla entity
  adjustments and crashed when a Fox NPC was spawned.
- Changed the movement objectives to a fixed order: flee, follow, return home, then stroll. NPCs
  with several movement objectives may therefore behave differently than before.
- Changed inserting a preset into a vanilla monster spawner to keep its delay, count and range.
- Changed "Follow owner" and "Look at owner" without a chosen target to follow the owner of the NPC.
- Changed datapack presets to `easy_npc/preset/`, the old folder still works, but only for easy_npc.
- Changed the preset browser to always list its entries in the same order.
- Removed the experimental user-defined NPCs; the extra config file is no longer read or created.
- Removed the unused `SERVER_SKIN` skin type.
- Added a new game test structure to support game tests in 1.21.11 and higher.
- Added a reload button to each entry of the player skin screen, to fetch a single skin again.
- Added the reason to the log whenever an NPC is removed, so an unexpected despawn can be traced.
- Added suggestions to `/easy_npc render set species`.
- Added support for presets from other mods, which no longer have to use the `easy_npc` namespace.
- Added a preset visibility that hides a preset from the preset browser and the NPC screens.
- Added a teleport distance and a resting spot offset to the follow objectives.
- Added item tags, a "can be scared" option and a "only without owner" option to "Follow item".
- Added an item field to the look objectives, so an NPC can watch a chosen dropped item.
- Added an identifier per NPC, so a mod can find its own NPCs again.
- Added an option to spawn an NPC again when its owner logs in.
- Added game tests for NeoForge, which had none, and restored the spawn egg, dialog screen, data
  isolation and smoke tests that were dropped during the update to 1.21.11.
- Added game tests for every configuration and editor screen, covering all three mod loaders.
- Added game tests for the home position, covering the spawn default, the stored value and
  reloading.
- Improved the model name parsing, which mistook a species ending in "_shiny" for a variant.
- Improved following of flying NPCs, which now steer directly when there is no path through the air.
- Improved third party support with an API for presets, own objectives and objective configuration.

### 7.4.0

- Fixed #807 by no longer pulling every same-type NPC within 32 blocks into a "Defend Self" fight.
- Fixed #805 by evaluating all visibility settings together instead of stopping at the first one.
- Fixed #804 by keeping the custom name on Doppler NPCs for name based textures and the name tag.
- Fixed #803 by adjusting assets to match typical Minecraft style instead of generic UI style.
- Fixed the day and night detection, which counted sunrise as night and sunset as day.
- Fixed the visibility in the Nether and the End, which used the Overworld time.
- Fixed the time and game mode visibility checkboxes appearing in a random order.
- Fixed game tests only being able to fail on their first assertion, which hid later failures.
- Fixed the "Saddled" horse skin variants, which never showed a saddle.
- Changed "Visible to Owner" and "Visible to Team" to also follow day, night and game mode.
- Changed Evoker, Illusioner and Vindicator variants without "Crossed Arms" to keep their arms down.
- Added a "Defend Faction" objective that attacks whoever hurt a nearby faction member or player.
- Added a system message when faction members fight each other and the faction stays out of it.
- Added a head equipment slot for Illagers so their captain banner can be removed or swapped.
- Added a day and night preview to the display configuration screen.
- Added unit and game tests for the visibility attributes and the faction defense.
- Improved the visibility check performance by reading the display attributes only once per check.
- Improved display attributes from older versions by completing them with their defaults.
- Improved the attack objective screen by disabling "Protect Owner" and "Defend Faction" when unset.

### 7.3.0

- Improved skin variants by carrying their data as typed fields instead of parsing variant names.
- Centralized the Cobblemon mod id and default model on shared constants.
- Reduced duplicated enum code with a shared `EnumUtils` helper for `get(String)` lookups.
- Optimized villager profession and type lookups to use direct registry keys instead of scans.
- Optimized enum name lookups by caching tag names, ids and keys once at construction.
- Added unit and game tests for skin variants, villager resolution and the enum helper.

### 7.1.2

- Fixed #801 by using combination for NPC, Dialog and Action UUIDs to avoid collisions for action
  tracking and execution limits.

### 7.1.1

- Fixed #800 by lazy loading entity types.
- Fixed #799 by fixing edge cases and adding additional documentation and better log messages.
- Fixed #798 by considering console, commands and command blocks as trusted command sources.
- Fixed a critical single-player / LAN issue where client and server NPCs shared the same registry.
- Fixed a memory leak in the configuration menu handling and added timeout and disconnect cleanup.
- Fixed duplicate downloads when several NPCs request the same skin at the same time.
- Fixed skin lookups failing after a error so a slow server no longer freezes the game.
- Fixed the URL skin screen freezing the game while checking a remote image; the check now runs in
  the background.
- Added a new World Spawner block that limits an NPC preset across the whole world (all dimensions),
  the existing Group Spawner now counts per dimension so far-away NPCs no longer block it.
- Hardened remote skin downloads with a strict size limit and safe redirect handling to prevent
  crashes from oversized or malicious skin URLs; added an optional `blockPrivateAddresses` setting.
- Improved server performance by only sending entity and player load events to NPCs that actually
  use them, grouped by dimension.

### 7.0.0

- Fixed #794 by using the expected type for NPC rendering.
- Fixed scaling screen preview by pinning the entity feet to the 0 line of the scale ruler, so NPCs
  scale from the bottom up again instead of from the center and no longer shift down after bounding
  box updates.
- Fixed #791 by adding Wandering Trader NPC type.
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
- Fixed faction management edge cases by validating faction names, sorting faction lists and
  refreshing loaded NPC scoreboard assignments after faction color or delete changes.
- Fixed custom skin reloads by loading and clearing texture caches per skin model instead of always
  rebuilding the full custom texture cache.
- Improved the `Attackable by Players/Monsters/Factions` combat attributes to work independently of
  `Invulnerable`, each opening a targeted hole for its own attacker group.
- Improved preset command and network handling by splitting the large handlers into smaller focused
  import, export and feature-specific components.
- Added Easy Model Entities Support.
- Added faction manager and faction editor screens to create, color and delete factions and manage
  hostile relations, protected by the new `FACTION_MANAGEMENT` security feature.
- Added new `Misc` attributes tab as home for the NPC faction assignment.
- Added reusable warning and help icon components for inline hints with tooltips.
- Added additional unit and game tests for factions, target objectives and Cobblemon model variants.

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

Note: This update changes the internal data format for conditions and actions, which may reset /
remove legacy existing execution-limit entries without stored IDs.

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

Notes: This update includes better security checks and permissions for multiplayer environments. It
allows to execute server commands which can be potentially dangerous if used with malicious intent.

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
- Added `despawn` and `spawn` commands to remove and re-spawn NPCs by UUID with configurable removal
  reasons.
- Added `ModelPoseAPI` and `EasyNPCEntityHandler` public API classes for controlling NPC poses and
  managing NPCs programmatically from external mods.
- Added pose data files for all supported NPC types.
- Added lock rotation checkbox to the basic pose configuration screen.
- Added `MoveToPositionGoal` to move an NPC to a position before executing a callback action.
- Added `EasyNPCLookControl` and `EasyNPCBodyRotationControl` to respect locked root rotation.
- Added synced owner change and dimension change to the NPC entity data index.
- Added EasyNPCItemAttachmentLayer into various entity renderers.
- Added dedicated `Flee Objective` configuration tab with objectives for fleeing creepers, monsters,
  mobs, players, villagers and the sun.
- Added `FOLLOW_ITEM` objective to the follow objectives tab to make NPCs follow item entities by
  resource location (e.g. `minecraft:apple`).
- Added `LookAtEntityByUUIDGoal` and enabled the look-at-entity-by-UUID and look-at-owner objectives
  in the look objectives screen.
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

This is a major release. Please back up your worlds and NPC data before updating. This version
contains many improvements, optimizations, and internal changes that are **not fully compatible**
with earlier releases.

⚠️ Breaking changes

- Existing NPCs and their configuration data from versions before 6.1.0 may not load correctly or
  may require manual adjustments.
- Internal data formats and some behaviors have been refactored to support new use-cases.

🧩 New modular structure

Easy NPC is now split into three separate mods:

- **Easy NPC** – Bundle that includes both Core and Configuration UI for a plug‑and‑play experience.
- **Easy NPC: Core** – Lightweight runtime and logic for NPCs, with minimal dependencies. Intended
  for servers, modpacks, and developers.
- **Easy NPC: Configuration UI** – Standalone configuration interface for creating and editing NPCs.
  Can be installed on clients that need the UI.

🎯 Why this change?

- Allows servers and modpacks to ship only the **Core** mod on the server side for a leaner setup.
- Reduces memory usage and load times when the UI is not required everywhere.
- Makes development and testing faster by separating UI and core logic.

[history]: https://github.com/MarkusBordihn/BOs-Easy-NPC/commits/

[upgrade_guide]: https://github.com/MarkusBordihn/BOs-Easy-NPC/wiki/Upgrading
