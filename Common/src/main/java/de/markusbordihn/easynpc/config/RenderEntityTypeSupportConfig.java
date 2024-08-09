/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.config;

import de.markusbordihn.easynpc.Constants;
import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;

public class RenderEntityTypeSupportConfig extends Config {

  public static final String CONFIG_FILE_NAME = "render_entity_type_support.cfg";
  public static final String CONFIG_FILE_HEADER =
      """
Render Entity Type Support Configuration

 Please note that this configuration file only includes confirmed entity types.
 If an entity type is not listed here, it doesn't mean it's automatically supported or unsupported!
""";
  private static final Set<String> knownUnsupportedEntityTypes =
      new HashSet<>(
          List.of(
              Constants.MINECRAFT_RESOURCE_PREFIX + "area_effect_cloud",
              Constants.MINECRAFT_RESOURCE_PREFIX + "armor_stand",
              Constants.MINECRAFT_RESOURCE_PREFIX + "arrow",
              Constants.MINECRAFT_RESOURCE_PREFIX + "bat",
              Constants.MINECRAFT_RESOURCE_PREFIX + "boat",
              Constants.MINECRAFT_RESOURCE_PREFIX + "chest_minecart",
              Constants.MINECRAFT_RESOURCE_PREFIX + "command_block_minecart",
              Constants.MINECRAFT_RESOURCE_PREFIX + "dragon_fireball",
              Constants.MINECRAFT_RESOURCE_PREFIX + "egg",
              Constants.MINECRAFT_RESOURCE_PREFIX + "end_crystal",
              Constants.MINECRAFT_RESOURCE_PREFIX + "ender_pearl",
              Constants.MINECRAFT_RESOURCE_PREFIX + "ender_dragon",
              Constants.MINECRAFT_RESOURCE_PREFIX + "evoker_fangs",
              Constants.MINECRAFT_RESOURCE_PREFIX + "experience_bottle",
              Constants.MINECRAFT_RESOURCE_PREFIX + "experience_orb",
              Constants.MINECRAFT_RESOURCE_PREFIX + "eye_of_ender",
              Constants.MINECRAFT_RESOURCE_PREFIX + "falling_block",
              Constants.MINECRAFT_RESOURCE_PREFIX + "fireball",
              Constants.MINECRAFT_RESOURCE_PREFIX + "firework_rocket",
              Constants.MINECRAFT_RESOURCE_PREFIX + "fishing_bobber",
              Constants.MINECRAFT_RESOURCE_PREFIX + "furnace_minecart",
              Constants.MINECRAFT_RESOURCE_PREFIX + "ghast",
              Constants.MINECRAFT_RESOURCE_PREFIX + "glow_item_frame",
              Constants.MINECRAFT_RESOURCE_PREFIX + "hopper_minecart",
              Constants.MINECRAFT_RESOURCE_PREFIX + "item",
              Constants.MINECRAFT_RESOURCE_PREFIX + "item_frame",
              Constants.MINECRAFT_RESOURCE_PREFIX + "leash_knot",
              Constants.MINECRAFT_RESOURCE_PREFIX + "lightning_bolt",
              Constants.MINECRAFT_RESOURCE_PREFIX + "llama_spit",
              Constants.MINECRAFT_RESOURCE_PREFIX + "marker",
              Constants.MINECRAFT_RESOURCE_PREFIX + "magma_cube",
              Constants.MINECRAFT_RESOURCE_PREFIX + "minecart",
              Constants.MINECRAFT_RESOURCE_PREFIX + "phantom",
              Constants.MINECRAFT_RESOURCE_PREFIX + "player",
              Constants.MINECRAFT_RESOURCE_PREFIX + "painting",
              Constants.MINECRAFT_RESOURCE_PREFIX + "potion",
              Constants.MINECRAFT_RESOURCE_PREFIX + "shulker_bullet",
              Constants.MINECRAFT_RESOURCE_PREFIX + "small_fireball",
              Constants.MINECRAFT_RESOURCE_PREFIX + "snowball",
              Constants.MINECRAFT_RESOURCE_PREFIX + "spawner_minecart",
              Constants.MINECRAFT_RESOURCE_PREFIX + "spectral_arrow",
              Constants.MINECRAFT_RESOURCE_PREFIX + "slime",
              Constants.MINECRAFT_RESOURCE_PREFIX + "text_display",
              Constants.MINECRAFT_RESOURCE_PREFIX + "tnt",
              Constants.MINECRAFT_RESOURCE_PREFIX + "tnt_minecart",
              Constants.MINECRAFT_RESOURCE_PREFIX + "trident",
              Constants.MINECRAFT_RESOURCE_PREFIX + "wither_skull",
              "ad_astra:ice_spit",
              "ad_astra:lander",
              "ad_astra:space_painting",
              "ad_astra:tier_1_rocket",
              "ad_astra:tier_1_rover",
              "ad_astra:tier_2_rocket",
              "ad_astra:tier_3_rocket",
              "ad_astra:tier_4_rocket",
              "ae2:tiny_tnt_primed",
              "aquamirae:pillagers_patrol",
              "armourers_workshop:mannequin",
              "botania:babylon_weapon",
              "botania:corporea_spark",
              "botania:doppleganger",
              "botania:ender_air",
              "botania:ender_air_bottle",
              "botania:falling_star",
              "botania:flame_ring",
              "botania:magic_landmine",
              "botania:magic_missile",
              "botania:mana_burst",
              "botania:mana_storm",
              "botania:pixie",
              "botania:player_mover",
              "botania:pool_minecart",
              "botania:spark",
              "botania:thorn_chakram",
              "botania:thrown_item",
              "create:carriage_contraption",
              "create:contraption",
              "create:crafting_blueprint",
              "create:gantry_contraption",
              "create:seat",
              "create:stationary_contraption",
              "create:super_glue",
              "dannys_expansion:mundane_slime",
              "dummmmmmy:target_dummy",
              "farmersdelight:rotten_tomato",
              "friendsandfoes:ice_chunk",
              "friendsandfoes:player_illusion",
              "frostiful:freezing_wind",
              "frostiful:frost_spell",
              "frostiful:packed_snowball",
              "frostiful:thrown_icicle",
              "graveyard:ghouling",
              "graveyard:skull",
              "handcrafted:fancy_painting",
              "handcrafted:seat",
              "lootr:lootr_minecart",
              "majruszsdifficulty:cursed_armor",
              "moretotems:summoned_bee",
              "moretotems:summoned_zombie",
              "orcz:decaystrikecustom",
              "orcz:wither_strikeringcustom",
              "simple_mobs.ground_spike",
              "simple_mobs:dragon_smoke",
              "simple_mobs:elemental_chain",
              "simple_mobs:lightning_spear",
              "simple_mobs:projecttest",
              "simple_mobs:rumble",
              "simple_mobs:staff_interact",
              "simple_mobs:staff_usage",
              "smallships:brigg",
              "smallships:cog",
              "smallships:galley",
              "swampier_swamps:swamp_gas",
              "techreborn:nuke",
              "terraform:boat",
              "the_bumblezone:bee_stinger",
              "the_bumblezone:cosmic_crystal_entity",
              "the_bumblezone:dirt_pellet",
              "the_bumblezone:electric_ring_entity",
              "the_bumblezone:honey_crystal_shard",
              "the_bumblezone:pollen_puff",
              "the_bumblezone:purple_spike_entity",
              "the_bumblezone:sentry_watcher",
              "the_bumblezone:thrown_stinger_spear",
              "twigs:pebble"));

  private static final Set<String> knownSupportedEntityTypes =
      new HashSet<>(
          List.of(
              Constants.MINECRAFT_RESOURCE_PREFIX + "axolotl",
              Constants.MINECRAFT_RESOURCE_PREFIX + "bee",
              Constants.MINECRAFT_RESOURCE_PREFIX + "blaze",
              Constants.MINECRAFT_RESOURCE_PREFIX + "cat",
              Constants.MINECRAFT_RESOURCE_PREFIX + "cave_spider",
              Constants.MINECRAFT_RESOURCE_PREFIX + "chicken",
              Constants.MINECRAFT_RESOURCE_PREFIX + "cod",
              Constants.MINECRAFT_RESOURCE_PREFIX + "cow",
              Constants.MINECRAFT_RESOURCE_PREFIX + "creeper",
              Constants.MINECRAFT_RESOURCE_PREFIX + "dolphin",
              Constants.MINECRAFT_RESOURCE_PREFIX + "donkey",
              Constants.MINECRAFT_RESOURCE_PREFIX + "drowned",
              Constants.MINECRAFT_RESOURCE_PREFIX + "elder_guardian",
              Constants.MINECRAFT_RESOURCE_PREFIX + "enderman",
              Constants.MINECRAFT_RESOURCE_PREFIX + "endermite",
              Constants.MINECRAFT_RESOURCE_PREFIX + "evoker",
              Constants.MINECRAFT_RESOURCE_PREFIX + "fox",
              Constants.MINECRAFT_RESOURCE_PREFIX + "giant",
              Constants.MINECRAFT_RESOURCE_PREFIX + "glow_squid",
              Constants.MINECRAFT_RESOURCE_PREFIX + "goat",
              Constants.MINECRAFT_RESOURCE_PREFIX + "guardian",
              Constants.MINECRAFT_RESOURCE_PREFIX + "hoglin",
              Constants.MINECRAFT_RESOURCE_PREFIX + "horse",
              Constants.MINECRAFT_RESOURCE_PREFIX + "husk",
              Constants.MINECRAFT_RESOURCE_PREFIX + "illusioner",
              Constants.MINECRAFT_RESOURCE_PREFIX + "iron_golem",
              Constants.MINECRAFT_RESOURCE_PREFIX + "llama",
              Constants.MINECRAFT_RESOURCE_PREFIX + "mooshroom",
              Constants.MINECRAFT_RESOURCE_PREFIX + "mule",
              Constants.MINECRAFT_RESOURCE_PREFIX + "ocelot",
              Constants.MINECRAFT_RESOURCE_PREFIX + "panda",
              Constants.MINECRAFT_RESOURCE_PREFIX + "parrot",
              Constants.MINECRAFT_RESOURCE_PREFIX + "pig",
              Constants.MINECRAFT_RESOURCE_PREFIX + "piglin",
              Constants.MINECRAFT_RESOURCE_PREFIX + "piglin_brute",
              Constants.MINECRAFT_RESOURCE_PREFIX + "pillager",
              Constants.MINECRAFT_RESOURCE_PREFIX + "polar_bear",
              Constants.MINECRAFT_RESOURCE_PREFIX + "pufferfish",
              Constants.MINECRAFT_RESOURCE_PREFIX + "rabbit",
              Constants.MINECRAFT_RESOURCE_PREFIX + "ravager",
              Constants.MINECRAFT_RESOURCE_PREFIX + "salmon",
              Constants.MINECRAFT_RESOURCE_PREFIX + "sheep",
              Constants.MINECRAFT_RESOURCE_PREFIX + "shulker",
              Constants.MINECRAFT_RESOURCE_PREFIX + "silverfish",
              Constants.MINECRAFT_RESOURCE_PREFIX + "skeleton",
              Constants.MINECRAFT_RESOURCE_PREFIX + "skeleton_horse",
              Constants.MINECRAFT_RESOURCE_PREFIX + "snow_golem",
              Constants.MINECRAFT_RESOURCE_PREFIX + "spider",
              Constants.MINECRAFT_RESOURCE_PREFIX + "squid",
              Constants.MINECRAFT_RESOURCE_PREFIX + "stray",
              Constants.MINECRAFT_RESOURCE_PREFIX + "strider",
              Constants.MINECRAFT_RESOURCE_PREFIX + "trader_llama",
              Constants.MINECRAFT_RESOURCE_PREFIX + "tropical_fish",
              Constants.MINECRAFT_RESOURCE_PREFIX + "turtle",
              Constants.MINECRAFT_RESOURCE_PREFIX + "vex",
              Constants.MINECRAFT_RESOURCE_PREFIX + "villager",
              Constants.MINECRAFT_RESOURCE_PREFIX + "vindicator",
              Constants.MINECRAFT_RESOURCE_PREFIX + "wandering_trader",
              Constants.MINECRAFT_RESOURCE_PREFIX + "witch",
              Constants.MINECRAFT_RESOURCE_PREFIX + "wither",
              Constants.MINECRAFT_RESOURCE_PREFIX + "wither_skeleton",
              Constants.MINECRAFT_RESOURCE_PREFIX + "wolf",
              Constants.MINECRAFT_RESOURCE_PREFIX + "zoglin",
              Constants.MINECRAFT_RESOURCE_PREFIX + "zombie",
              Constants.MINECRAFT_RESOURCE_PREFIX + "zombie_horse",
              Constants.MINECRAFT_RESOURCE_PREFIX + "zombie_villager",
              Constants.MINECRAFT_RESOURCE_PREFIX + "zombified_piglin"));

  private static final Set<String> supportedEntityTypes = new HashSet<>();
  private static final Set<String> unsupportedEntityTypes = new HashSet<>();

  public static void registerConfig() {
    registerConfigFile(CONFIG_FILE_NAME, CONFIG_FILE_HEADER);
    parseConfigFile();
  }

  public static void parseConfigFile() {
    File configFile = getConfigFile(CONFIG_FILE_NAME);
    Properties properties = readConfigFile(configFile);
    Properties unmodifiedProperties = (Properties) properties.clone();

    // Parse known unsupported entity types, and add them to the supported or unsupported list.
    for (String entityType : knownUnsupportedEntityTypes) {
      if (parseConfigValue(properties, entityType, false)) {
        addSupportedEntityType(entityType);
      } else {
        addUnsupportedEntityType(entityType);
      }
    }

    // Parse known supported entity types, and add them to the supported list.
    for (String entityType : knownSupportedEntityTypes) {
      if (parseConfigValue(properties, entityType, true)) {
        addSupportedEntityType(entityType);
      } else {
        addUnsupportedEntityType(entityType);
      }
    }

    // Parse the rest of the configuration file.
    for (String entityType : properties.stringPropertyNames()) {
      if (knownSupportedEntityTypes.contains(entityType)
          || knownUnsupportedEntityTypes.contains(entityType)) {
        continue;
      }
      if (entityType == null || !entityType.contains(":")) {
        log.error("Remove invalid entity type {} from {}.", entityType, CONFIG_FILE_NAME);
        properties.remove(entityType);
        continue;
      }
      if (parseConfigValue(properties, entityType, false)) {
        addSupportedEntityType(entityType);
      } else {
        addUnsupportedEntityType(entityType);
      }
    }

    // Update config file if needed
    updateConfigFileIfChanged(configFile, CONFIG_FILE_HEADER, properties, unmodifiedProperties);
  }

  public static void addUnsupportedEntityType(String entityType) {
    unsupportedEntityTypes.add(entityType);
    if (supportedEntityTypes.contains(entityType)) {
      supportedEntityTypes.remove(entityType);
    }
  }

  public static void addSupportedEntityType(String entityType) {
    supportedEntityTypes.add(entityType);
    if (unsupportedEntityTypes.contains(entityType)) {
      unsupportedEntityTypes.remove(entityType);
    }
  }

  public static boolean isSupportedEntityType(String entityType) {
    return supportedEntityTypes.contains(entityType);
  }

  public static boolean isUnsupportedEntityType(String entityType) {
    return unsupportedEntityTypes.contains(entityType);
  }

  public Set<String> getSupportedEntityTypes() {
    return supportedEntityTypes;
  }

  public Set<String> getUnsupportedEntityTypes() {
    return unsupportedEntityTypes;
  }
}
