/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.client.renderer.manager;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

@DisplayName("EntityTypeManager Tests")
class EntityTypeManagerTest {

  @Nested
  @DisplayName("shouldFilterEntityTypeByName - Suffix Patterns")
  class SuffixPatternTests {

    @ParameterizedTest
    @DisplayName("Should filter entities ending with projectile suffixes")
    @ValueSource(
        strings = {
          "minecraft:spectral_arrow",
          "cataclysm:lightning_spear",
          "cataclysm:wither_missile",
          "aether:zephyr_snowball",
          "aether:enchanted_dart",
          "aether:poison_dart",
          "aether:golden_dart",
          "twilightforest:lich_bolt",
          "cataclysm:laser_beam",
          "cataclysm:portal_abyss_blast",
          "cataclysm:abyss_orb",
          "galosphere:pink_salt_shard",
          "twilightforest:moonworm_shot",
          "aether:poison_needle",
          "cataclysm:urchin_spike",
          "cataclysm:tidal_tentacle",
          "cataclysm:void_vortex",
          "twilightforest:hydra_mortar",
          "cataclysm:coral_spear",
          "aether:golden_parachute",
          "aether:cold_parachute",
          "cataclysm:flare_bomb",
          "twilightforest:lich_bomb",
          "galosphere:silver_bomb",
          "galosphere:spectre_flare",
          "galosphere:glow_flare",
          "netherexp:shotgun_pellet",
          "deep_aether:venomite_bubble",
          "twilightforest:slime_blob",
          "mowziesmobs:fissure_piece",
          "netherexp:grave_cloud"
        })
    void shouldFilterProjectileSuffixes(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Should filter entities ending with vehicle/item suffixes")
    @ValueSource(
        strings = {
          "minecraft:oak_boat",
          "deeperdarker:boat",
          "deep_aether:boat",
          "biomesoplenty:boat",
          "twilightforest:boat",
          "deep_aether:quail_egg",
          "minecraft:ender_pearl",
          "aether:fire_crystal",
          "aether:ice_crystal",
          "aether:thunder_crystal",
          "minecraft:end_crystal",
          "aether_redux:veridium_dart",
          "aether_redux:infused_veridium_dart",
          "aether_redux:thrown_spear",
          "aether_redux:volatile_fire_crystal"
        })
    void shouldFilterVehicleAndItemSuffixes(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Should filter entities ending with effect/display suffixes")
    @ValueSource(
        strings = {
          "minecraft:area_effect_cloud",
          "someMod:some_display",
          "someMod:spawn_marker",
          "cataclysm:flare_bomb_ball",
          "someMod:ghast_fireball",
          "someMod:wither_charge",
          "someMod:fire_bullet"
        })
    void shouldFilterEffectAndDisplaySuffixes(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Entities with 'thrown' prefix are NOT caught by _thrown suffix pattern")
    @ValueSource(
        strings = {
          "twilightforest:thrown_wep",
          "twilightforest:thrown_ice",
          "twilightforest:thrown_block"
        })
    void thrownPrefixEntitiesAreNotCaughtBySuffixPattern(String entityTypeLocation) {
      assertFalse(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Entity with 'thrown_' prefix should NOT be caught by '_thrown' suffix: "
              + entityTypeLocation);
    }
  }

  @Nested
  @DisplayName("shouldFilterEntityTypeByName - Contains Patterns")
  class ContainsPatternTests {

    @ParameterizedTest
    @DisplayName("Should filter entities containing known non-mob patterns")
    @ValueSource(
        strings = {
          "minecraft:minecart",
          "minecraft:chest_minecart",
          "minecraft:tnt_minecart",
          "lootr:lootr_minecart",
          "amendments:falling_lantern",
          "blueprint:falling_block",
          "twilightforest:falling_ice",
          "lost_aether_content:falling_rock",
          "somemod:multi_part_entity",
          "somemod:effect_cloud",
          "somemod:flash_bang"
        })
    void shouldFilterContainsPatterns(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Should filter entities with :projectile or :spell_ prefix")
    @ValueSource(strings = {"somemod:projectile_entity", "somemod:spell_fire", "somemod:spell_ice"})
    void shouldFilterPrefixPatterns(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Should filter entities with _attack in name")
    @ValueSource(strings = {"somemod:fire_attack_entity", "somemod:melee_attack"})
    void shouldFilterAttackPattern(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }
  }

  @Nested
  @DisplayName("shouldFilterEntityTypeByName - Mod Prefix Patterns")
  class ModPrefixTests {

    @ParameterizedTest
    @DisplayName("Should filter own mod entities")
    @ValueSource(strings = {"easy_npc:humanoid", "easy_npc:fairy", "easy_npc:orc"})
    void shouldFilterOwnModEntities(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Should filter mythicmounts entities")
    @ValueSource(strings = {"mythicmounts:some_mount", "mythicmounts:dragon"})
    void shouldFilterMythicMountsEntities(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter: " + entityTypeLocation);
    }
  }

  @Nested
  @DisplayName("shouldFilterEntityTypeByName - Valid Mob Entities (should NOT be filtered)")
  class ValidMobTests {

    @ParameterizedTest
    @DisplayName("Should NOT filter vanilla mob entities")
    @ValueSource(
        strings = {
          "minecraft:zombie",
          "minecraft:skeleton",
          "minecraft:creeper",
          "minecraft:villager",
          "minecraft:iron_golem",
          "minecraft:witch",
          "minecraft:enderman",
          "minecraft:blaze",
          "minecraft:spider",
          "minecraft:cave_spider",
          "minecraft:pig",
          "minecraft:cow",
          "minecraft:sheep",
          "minecraft:chicken",
          "minecraft:wolf",
          "minecraft:cat",
          "minecraft:horse",
          "minecraft:wither_skeleton",
          "minecraft:evoker",
          "minecraft:vindicator",
          "minecraft:pillager",
          "minecraft:wandering_trader"
        })
    void shouldNotFilterVanillaMobs(String entityTypeLocation) {
      assertFalse(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should NOT filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Should NOT filter third-party mob entities")
    @ValueSource(
        strings = {
          "alexsmobs:gorilla",
          "alexsmobs:elephant",
          "alexsmobs:tiger",
          "alexsmobs:crocodile",
          "cataclysm:ender_golem",
          "cataclysm:netherite_monstrosity",
          "twilightforest:naga",
          "mowziesmobs:ferrous_wroughtnaut",
          "mowziesmobs:frostmaw",
          "quark:crab",
          "quark:foxhound",
          "guardvillagers:guard",
          "irons_spellbooks:necromancer",
          "supplementaries:red_merchant"
        })
    void shouldNotFilterThirdPartyMobs(String entityTypeLocation) {
      assertFalse(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should NOT filter: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Should NOT filter special non-matching entities")
    @ValueSource(
        strings = {
          "aether:aerwhale",
          "aether:zephyr",
          "aether:sentry",
          "twilightforest:ur_ghast",
          "twilightforest:hydra",
          "twilightforest:wraith",
          "twilightforest:knight_phantom",
          "twilightforest:maze_slime",
          "deeperdarker:sludge",
          "betterend:end_slime",
          "galosphere:spectator_vision",
          "galosphere:pink_salt_pillar"
        })
    void shouldNotFilterSpecialEntities(String entityTypeLocation) {
      assertFalse(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should NOT filter (handled by MobCategory.MISC or config): " + entityTypeLocation);
    }
  }

  @Nested
  @DisplayName("shouldFilterEntityTypeByName - Edge Cases")
  class EdgeCaseTests {

    @ParameterizedTest
    @NullAndEmptySource
    @DisplayName("Should filter null and empty entity type locations")
    void shouldFilterNullAndEmpty(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should filter null/empty values");
    }

    @Test
    @DisplayName("Should not filter entity that partially matches but does not meet criteria")
    void shouldNotFilterPartialMatches() {
      // "arrow_maker" contains "arrow" but does not END with "_arrow"
      assertFalse(EntityTypeManager.shouldFilterEntityTypeByName("somemod:arrow_maker"));
      // "boat_captain" starts with "boat" but does not match ":boat" at end or "_boat"
      assertFalse(EntityTypeManager.shouldFilterEntityTypeByName("somemod:boat_captain"));
      // "crystal_golem" contains "crystal" but does not END with "_crystal"
      assertFalse(EntityTypeManager.shouldFilterEntityTypeByName("somemod:crystal_golem"));
      // "bomb_thrower" contains "bomb" but does not END with "_bomb"
      assertFalse(EntityTypeManager.shouldFilterEntityTypeByName("somemod:bomb_thrower"));
    }

    @Test
    @DisplayName("Should filter entities with 'effect' at the end")
    void shouldFilterEffectSuffix() {
      assertTrue(EntityTypeManager.shouldFilterEntityTypeByName("somemod:poison_effect"));
      assertTrue(EntityTypeManager.shouldFilterEntityTypeByName("somemod:area_effect"));
    }

    @Test
    @DisplayName("Should handle entity names with only namespace")
    void shouldHandleNamespaceOnly() {
      assertFalse(EntityTypeManager.shouldFilterEntityTypeByName("somemod:zombie"));
      assertTrue(EntityTypeManager.shouldFilterEntityTypeByName("easy_npc:something"));
    }
  }

  @Nested
  @DisplayName("shouldFilterEntityTypeByName - Real Error Log Entities")
  class RealLogEntityTests {

    @ParameterizedTest
    @DisplayName("Entities from error log that SHOULD be caught by name patterns")
    @ValueSource(
        strings = {
          "cataclysm:laser_beam",
          "cataclysm:death_laser_beam",
          "cataclysm:lightning_spear",
          "cataclysm:coral_spear",
          "cataclysm:water_spear",
          "cataclysm:wither_missile",
          "cataclysm:wither_homing_missile",
          "cataclysm:abyss_blast",
          "cataclysm:mini_abyss_blast",
          "cataclysm:portal_abyss_blast",
          "cataclysm:abyss_orb",
          "cataclysm:void_vortex",
          "cataclysm:lionfish_spike",
          "cataclysm:urchin_spike",
          "cataclysm:tidal_tentacle",
          "cataclysm:flare_bomb",
          "cataclysm:poison_dart",
          "aether:enchanted_dart",
          "aether:poison_dart",
          "aether:golden_dart",
          "aether:zephyr_snowball",
          "aether:poison_needle",
          "aether:fire_crystal",
          "aether:ice_crystal",
          "aether:thunder_crystal",
          "aether:cloud_crystal",
          "aether:cold_parachute",
          "aether:golden_parachute",
          "aether_redux:veridium_dart",
          "aether_redux:infused_veridium_dart",
          "aether_redux:volatile_fire_crystal",
          "aether_redux:thrown_spear",
          "deep_aether:wind_crystal",
          "deep_aether:quail_egg",
          "deep_aether:boat",
          "deep_aether:venomite_bubble",
          "twilightforest:lich_bolt",
          "twilightforest:wand_bolt",
          "twilightforest:nature_bolt",
          "twilightforest:tome_bolt",
          "twilightforest:lich_bomb",
          "twilightforest:moonworm_shot",
          "twilightforest:slime_blob",
          "twilightforest:ice_snowball",
          "twilightforest:hydra_mortar",
          "twilightforest:boat",
          "twilightforest:falling_ice",
          "illagerinvasion:skull_bolt",
          "lost_aether_content:falling_rock",
          "lost_aether_content:cloud_shot",
          "deeperdarker:boat",
          "biomesoplenty:boat",
          "galosphere:pink_salt_shard",
          "galosphere:spectre_flare",
          "galosphere:glow_flare",
          "galosphere:silver_bomb",
          "netherexp:shotgun_pellet",
          "netherexp:grave_cloud",
          "mowziesmobs:earth_spike",
          "mowziesmobs:fissure_piece",
          "blueprint:falling_block",
          "amendments:falling_lantern"
        })
    void shouldFilterByNamePattern(String entityTypeLocation) {
      assertTrue(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Should be caught by name pattern: " + entityTypeLocation);
    }

    @ParameterizedTest
    @DisplayName("Entities from error log NOT caught by name patterns (need config or MISC filter)")
    @ValueSource(
        strings = {
          "aether:aerwhale",
          "aether:zephyr",
          "aether:blue_swet",
          "aether:golden_swet",
          "aether:sentry",
          "aether:cloud_minion",
          "aether:whirlwind",
          "aether:evil_whirlwind",
          "aether:tnt_present",
          "aether:floating_block",
          "aether:lightning_knife",
          "aether_redux:ember",
          "aether_redux:vanilla_swet",
          "twilightforest:ur_ghast",
          "twilightforest:carminite_ghastguard",
          "twilightforest:carminite_ghastling",
          "twilightforest:wraith",
          "twilightforest:hydra",
          "twilightforest:maze_slime",
          "twilightforest:knight_phantom",
          "twilightforest:slider",
          "twilightforest:protection_box",
          "twilightforest:chain_block",
          "twilightforest:cube_of_annihilation",
          "twilightforest:magic_painting",
          "twilightforest:thrown_wep",
          "twilightforest:thrown_ice",
          "twilightforest:thrown_block",
          "cataclysm:cursed_sandstorm",
          "cataclysm:storm_serpent",
          "cataclysm:void_howitzer",
          "cataclysm:spark",
          "cataclysm:flame_jet",
          "cataclysm:ancient_desert_stele",
          "cataclysm:player_ceraunus",
          "cataclysm:scylla_ceraunus",
          "cataclysm:abyss_mark",
          "cataclysm:abyss_blast_portal",
          "cataclysm:abyss_portal",
          "cataclysm:phantom_halberd",
          "cataclysm:the_leviathan_tongue",
          "cataclysm:eye_of_dungeon",
          "cataclysm:wither_howitzer",
          "cataclysm:sandstorm",
          "cataclysm:tidal_hook",
          "cataclysm:lightning_storm",
          "cataclysm:wave",
          "cataclysm:abyss_mine",
          "cataclysm:dimensional_rift",
          "cataclysm:octo_ink",
          "cataclysm:coral_bardiche",
          "cataclysm:accretion",
          "cataclysm:earthquake",
          "cataclysm:axe_blade",
          "cataclysm:bolt_strike",
          "deep_aether:baby_zephyr",
          "deep_aether:eots_segment",
          "deep_aether:eots_controller",
          "deep_aether:gentle_wind",
          "deeperdarker:sludge",
          "betterend:end_slime",
          "galosphere:spectator_vision",
          "galosphere:pink_salt_pillar",
          "netherexp:ecto_slab",
          "netherexp:will_o_wisp",
          "netherexp:black_icicle",
          "netherexp:antidote",
          "alexsmobs:fart",
          "alexsmobs:tendon_segment",
          "illagerinvasion:flying_magma",
          "illagerinvasion:hatchet",
          "illagerinvasion:invoker_fangs",
          "mowziesmobs:boulder_platform_crumbling",
          "mowziesmobs:block_swapper_tunneling",
          "mowziesmobs:pillar_sculptor",
          "mowziesmobs:fissure",
          "quark:phoenix_flamerang",
          "quark:valkyrie_pickarang",
          "quark:flamerang",
          "quark:dyed_item_frame",
          "dragonloot:dragon_trident",
          "decorative_blocks:dummy_entity",
          "bclib:chair",
          "ascended_quark:stool",
          "another_furniture:seat",
          "supplementaries:hat_stand",
          "supplementaries:cannonball",
          "mynethersdelight:strider_rock",
          "raided:lightning",
          "snowrealmagic:snow",
          "yungscavebiomes:icicle",
          "minecraft:interaction",
          "minecraft:fireball"
        })
    void shouldNotFilterByNamePattern(String entityTypeLocation) {
      assertFalse(
          EntityTypeManager.shouldFilterEntityTypeByName(entityTypeLocation),
          "Not caught by name pattern (needs MobCategory.MISC or config): " + entityTypeLocation);
    }
  }
}
