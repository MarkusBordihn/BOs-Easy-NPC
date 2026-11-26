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

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

final class RenderEntityTypeSupportDefaults {

  static final Set<String> KNOWN_UNSUPPORTED_ENTITY_TYPES =
      entityTypes(
          """
          minecraft: ender_dragon magma_cube phantom slime
          """);
  static final Set<String> KNOWN_SUPPORTED_ENTITY_TYPES =
      entityTypes(
          """
          minecraft: axolotl bat bee blaze cat cave_spider chicken cod cow creeper dolphin donkey
              drowned elder_guardian enderman endermite evoker fox giant glow_squid goat guardian
              hoglin horse husk illusioner iron_golem llama mooshroom mule ocelot panda parrot pig
              piglin piglin_brute pillager polar_bear pufferfish rabbit ravager salmon sheep shulker
              silverfish skeleton skeleton_horse snow_golem spider squid stray strider trader_llama
              tropical_fish turtle vex villager vindicator wandering_trader witch wither
              wither_skeleton wolf zoglin zombie zombie_horse zombie_villager zombified_piglin
          """);
  static final Set<String> ENTITY_TYPES_WITH_CHANGED_DEFAULT = entityTypes("minecraft: bat");
  static final Set<String> KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES =
      entityTypes(
          """
          ad_astra: ice_spit lander tier_1_rover
          ae2: tiny_tnt_primed
          aether: aerwhale blue_swet cloud_minion evil_whirlwind golden_swet sentry whirlwind zephyr
          aether_redux: ember vanilla_swet
          alexsmobs: cachalot_echo centipede_body centipede_tail cosmic_cod gust hemolymph
              mosquito_spit squid_grapple straddleboard vine_lasso void_portal
          aquamirae: maze_rose pillagers_patrol poisoned_chakra
          armourers_workshop: mannequin
          ba_bt: core_monolith core_obelisk end_monolith end_obelisk explosion_physics
              land_destruction land_monolith land_obelisk nether_monolith nether_obelisk
              ocean_destruction ocean_monolith ocean_obelisk sky_monolith sky_obelisk
          betterend: end_slime
          biomemakeover: blightbat lightning_bottle tumbleweed
          bloodandmadness: augur_of_ebrietas binding_spell blossomed_eye bullet
              call_beyond_aim_helper call_beyond flame red_jewel
          blue_skies: ent_leaf ent_root ent_wall fluctuant_sphere frost_spirit nyctofly spear
              strange_lightning venom_spit
          bosses_of_mass_destruction: comet petal_blade soul_star
          botania: babylon_weapon corporea_spark doppleganger ender_air_bottle ender_air flame_ring
              magic_landmine mana_burst mana_storm pixie player_mover spark thorn_chakram
          brutalbosses: thrownitem
          cataclysm: abyss_mark abyss_portal accretion ancient_desert_stele ashen_breath axe_blade
              blazing_bone charge_watcher coral_bardiche cursed_sandstorm dimensional_rift
              earthquake eye_of_dungeon flame_jet flame_strike lightning_storm octo_ink
              phantom_halberd player_ceraunus sandstorm scylla_ceraunus spark storm_serpent
              the_leviathan_tongue tidal_hook void_howitzer void_rune wall_watcher wave
              wither_howitzer
          cloudstorage: badloon_hand balloon_cargo balloon_tie balloon
          conjurer_illager: throwing_card
          corpse: corpse
          create: contraption crafting_blueprint super_glue
          dannys_expansion: arrow bullet klifour_spit mundane_slime test_dummy
          darkersouls: nameless_king_summons wind_no_collision
          dawncraft: fallen quest_player
          deep_aether: baby_zephyr eots_controller eots_segment gentle_wind
          deeperdarker: sludge
          dummmmmmy: dummy_number target_dummy
          epicfight: dodge_left dropped_nether_star wither_ghost
          farmersdelight: rotten_tomato
          friendsandfoes: ice_chunk player_illusion
          frostiful: freezing_wind frost_spell
          galosphere: pink_salt_pillar spectator_vision
          graveyard: ghouling skull
          habitat: kabloom_fruit
          hexerei: broom
          hunterillager: boomerang
          illageandspillage: imp
          illagerinvasion: flying_magma hatchet invoker_fangs
          infernal: flaming_poop
          infernalexp: glowsilk_moth
          irons_spellbooks: black_hole comet dragon_breath_pool fire_field fireball frozen_humanoid
              healing_aoe poison_breath poison_splash root shield spectral_hammer target_area
              wall_of_fire
          lost_aether_content: aerwhale_king
          majruszsdifficulty: cursed_armor
          meetyourfight: swamp_mine swampjaw
          monsterplus: bloody_slash crystal_clump crystal_spikes opalescent_eye spike_countdown
          moretotems: summoned_bee summoned_zombie
          mowziesmobs: block_swapper_sculptor block_swapper_tunneling block_swapper
              boulder_platform_crumbling boulder_platform dart fissure frozen_controller ice_breath
              pillar_sculptor pillar rock_sling sunstrike super_nova
          mutantmonsters: endersoul_fragment skull_spirit
          netherexp: ecto_slab will_o_wisp
          orcz: decaystrikecustom wither_strikeringcustom
          quark: chest_passenger glass_frame gravisand pickarang skull_pike soul_bead stool totem
          restored_earth: melon_seed rotten_flesh tropical_slime
          revamped_phantoms: shockwave
          savage_and_ravage: burning_banner ice_chunk rune_prison
          simple_mobs: dragon_smoke elemental_chain korok_intro projecttest rumble staff_interact
              staff_usage
          smallships: brigg cog galley
          supplementaries: bomb cannonball hat_stand
          swampier_swamps: swamp_gas
          sword_soaring: rain_cutter_sword rain_screen_sword stellar_sword sword
          techreborn: nuke
          the_bumblezone: bee_stinger cosmic_crystal_entity electric_ring_entity pollen_puff
              purple_spike_entity sentry_watcher
          tumbleweed: tumbleweed
          twigs: pebble
          twilightforest: carminite_ghastguard carminite_ghastling hydra knight_phantom maze_slime
              protection_box slider ur_ghast wraith
          untamedwilds: spit
          wom: antitheus_darkness enderblast
          """);
  static final Set<String> KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES =
      entityTypes(
          """
          alexsmobs: alligator_snapping_turtle anaconda anteater bald_eagle bison blobfish
              bone_serpent bunfungus cachalot_whale capuchin_monkey catfish centipede_head cockroach
              comb_jelly cosmaw crimson_mosquito crocodile crow devils_hole_pupfish dropbear
              elephant emu endergrade enderiophage flutter fly flying_fish frilled_shark
              froststalker gazelle gelada_monkey giant_squid gorilla grizzly_bear guster
              hammerhead_shark hummingbird jerboa kangaroo komodo_dragon laviathan leafcutter_ant
              lobster maned_wolf mantis_shrimp mimic_octopus mimicube moose mungus orca platypus
              raccoon rattlesnake roadrunner rocky_roller sea_bear seagull seal shoebill skelewag
              snow_leopard soul_vulture spectre straddler stradpole sunbird tarantula_hawk
              tasmanian_devil terrapin tiger toucan tusklin void_worm warped_mosco warped_toad
          aquamirae: anglerfish captain_cornelia eel golden_moth luminous_jelly maw maze_mother
              spinefish tortured_soul
          ba_bt: bt_cultist core_golem end_golem land_golem nether_golem ocean_golem
              platinum_skeleton sky_golem sky_minion
          biomemakeover: adjudicator_mimic adjudicator cowboy decayed dragonfly ghost glowfish
              lightning_bug_alternate lightning_bug moth mushroom_trader owl rootling scuttler
              stone_golem tadpole toad
          bloodandmadness: ashen_blood_beast_patient beast_patient brainsucker celestial_emissary
              church_doctor_crucifix church_doctor_flamesprayer church_doctor_lantern
              church_doctor_pistol church_doctor_scythe church_doctor cloaked_beast_patient
              father_gascoigne gascoigne_beast huntsman_axe huntsman_cutlass micolash silverbeast
              skeletal_puppet
          blue_skies: alchemist arachnarch armored_frost_spirit artificial_golem azulfo
              charscale_moki cosmic_fox crogre crynocerous crystal_camel diophyde_prowler emberback
              firefly gatekeeper grittle_flatfish horizofin_tunid infested_swarmer jelly_drifter
              municipal_monkfish nested_spider polargeist reindeer seclam shade_monitor shrumpty
              sliv snow_owl spewter stardust_ram starlit_crusher stonelet summoner supporter_pet
              venom_spider whistleshell_crab
          bosses_of_mass_destruction: gauntlet lich obsidilith void_blossom
          bygonenether: corpor piglin_hunter piglin_prisoner warped_enderman wex
              wither_skeleton_horse wither_skeleton_knight wraither
          cataclysm: ender_golem ender_guardian endermaptera ignis ignited_revenant
              nameless_sorcerer netherite_monstrosity
          cloudstorage: badloon balloon_buddy bloviator
          conjurer_illager: conjurer
          creeperoverhaul: badlands_creeper bamboo_creeper beach_creeper cave_creeper
              dark_oak_creeper desert_creeper dripstone_creeper hills_creeper jungle_creeper
              mushroom_creeper savannah_creeper snowy_creeper spruce_creeper swamp_creeper
          dannys_expansion: cursed_skull desertic_slime floam foamcaster foamie foamspring ghoul
              goblin hollow_armor hollowlet ice_elemental klifour magma_slime monstrous_scorpion
              squig
          darkersouls: nameless_king
          dawncraft: knight_player
          difficultraids: assassin_illager dart_illager electro_illager frost_illager modur_elite
              necromancer_illager nuaos_elite shaman_illager tank_illager voldon_elite
              voldon_familiar warrior_illager xydrax_elite
          dragonmounts: dragon
          ecologics: camel coconut_crab penguin squirrel
          enlightened_end: big_ringling stalker
          epicfight: wither_skeleton_minion
          friendsandfoes: copper_golem glare iceologer mauler moobloom
          goblinsanddungeons: garch gob gobber goblin_king goblo goom hobgob mimic ogre
          goblintraders: goblin_trader vein_goblin_trader
          guardvillagers: guard
          habitat: passerine pooka
          hexerei: crow
          humancompanions: arbalist archer axeguard knight
          hunterillager: hunterillager
          illageandspillage: boss_randomizer crashager dispenser faker igniter illager_soul
              illashooter magispeller mob_spirit spirit_hand spiritcaller twittollager
          infernalexp: basalt_giant blackstone_dwarf blindsight embody glowsquito shroomloin voline
              warpbeetle
          irons_spellbooks: archevoker catacombs_zombie citadel_keeper cryomancer dead_king_corpse
              dead_king debug_wizard magehunter_vindicator necromancer pyromancer spectral_steed
              summoned_polar_bear summoned_skeleton summoned_vex summoned_zombie wisp
          kobolds: kobold_captain kobold_child kobold_enchanter kobold_engineer kobold_pirate
              kobold_rascal kobold_skeleton kobold_warrior kobold_zombie kobold
          lilwings: aponi_butterfly butter_gold_butterfly cloudy_puff_butterfly
              crystal_puff_butterfly gold_applefly_butterfly grayling_butterfly
              painted_panther_butterfly red_applefly_butterfly shroom_skipper_butterfly
              swallow_tail_butterfly swamp_hopper_butterfly white_fox_butterfly
          majruszsdifficulty: cerberus creeperling giant tank
          meetyourfight: bellringer dame_fortuna
          mimic: mimic
          monsterplus: abyssologer crystal_zombie glow_skeleton lava_squid mother_lava_squid
              overgrown_skeleton spectral_skeleton swamp_zombie
          mowziesmobs: baby_foliaath ferrous_wroughtnaut foliaath frostmaw grottol lantern naga
              umvuthana_crane_player umvuthana_crane umvuthana_follower_player
              umvuthana_follower_raptor umvuthana_raptor umvuthana umvuthi
          mutantmonsters: creeper_minion endersoul_clone mutant_creeper mutant_enderman
              mutant_skeleton mutant_snow_golem mutant_zombie spider_pig
          netherskeletons: basalt_skeleton charred_skeleton crimson_skeleton crying_obby_skeleton
              nether_skeleton skelly_ghast soul_skeleton warped_skeleton
          ob_core: ancient_paladin hilda obsidian_shard
          orcz: bigorcboss decaystrike goblin_2sword goblincammo goblinsavagebomb goblinsavagemace
              goblinshield goblinwerid orcirongroupleader orcsavage_2sword orcsavage_2swordiron
              orcsavagegroupleader orcsavagemace orcsavagemaceiron orcsavageweird orcshield
              orcshieldiron savagegoblin savageorc savageorciron spectre_squig squig weirdboinecro
              witherstrikering
          piglinproliferation: piglin_alchemist
          progressivebosses: larva wither_minion
          quark: crab forgotten foxhound frog shiba stoneling toretoise wraith
          quest_giver: quest_guard_villager quest_villager
          restored_earth: bone_spider bouldering_zombie cluckshroom fancy_chicken furnace_golem
              jolly_llama jumbo_rabbit lobber_zombie melon_golem moobloom moolip muddy_pig
              rainbow_sheep skeleton_wolf viler_witch wooly_cow
          savage_and_ravage: creepie executioner griefer iceologer skeleton_villager trickster
          simple_mobs: air asharmor_fire asharmor_ice ball barbarian_1 barbarian_2 barbarian_3
              barbarian_4 barrel_man blackhole blackmarketvillager blood_splash_1 blood_splash_2
              bone bright_elemental caped_minotaur cave_blockhead cave_dweller cave_golem claw_mark
              cloaked_villager corrupted_ogre cube cultist damaginglaser dark_elemental darkwerewolf
              defeating_cube dragging_spell dragon_flame dragon_lightning_breath dragon_lord_stone
              dragon_lord elemental_deity elemental_grab elemental_shield elemental_spawn
              ensnared_soul ent fasttravelcircle fire_drag_proj fire_dragon_ambient fire_dragon_grab
              fire_dragon fire_giant fireball first_twin flame_ball_player gael_wraith_target
              gael_wraith gael gemspike getsuga_65 ghostprojectile giant_skele giant gob gobber
              goblo goom goop_spit grabbed graywerewolf griffingrab gryphon guardian_eye
              half_wither_demon herobrine_fear herobrine_flash herobrine_follower herobrine_spike_1
              herobrine_spike_2 herobrine herobrinese_2 herobrinese_3 herobrinese hobgob
              ibr_17eem_171 ice_dragon_ambient ice_dragon_breath ice_dragon imp_bomb imp_guard
              imp_melee imp_range josslyn_mc jungharam knight_2 knight_3 knight_4 knight korok_1
              korok_2 korok_3 korok_4 korok_5_balloon korok_5 laser_2 lightning_dragon_ambient
              lightning_dragon liver_cirrhosis lorde_rk lost_eye_ogre losteyeproj manuel_poky
              martian merlin_1306 minotaur monika_sunrise monk moon_slash_1 moon_slash_2 morgott
              moss_golem multipartbody_2 multipartbody nine_tails ninja notch_boss notch_grab
              notch_punch notch_sit ogre omen_clone patronwerewolf ppx protector raiddetectwitch
              rat_mid rat_small roar rod_particles samurai_1 samurai_2 samurai_4 samurai_5
              samurai_placeholder sand_wyrm second_twin sentinel_flash sentinel_knight
              sentinel_slash shade shadow_clone shadow_mew_yt shadow shield_element
              shielded_skeleton shogunate_ghost skeleton_demon skeletonlord solar_pixel
              souls_summon_2 souls_summon spawn_protect sphere spikes spiritnick sprout summon_drag
              sword_auto_2 sword_auto tail_dart tiny_necromancer toxic_goop tracking_thunder
              tree_drain_particles twins_stone u_gone vampire weak_ogre whisperer_stem_2
              whisperer_stem whisperer wind wither_demon wither_ray woodenday yes_im_david
          supplementaries: red_merchant
          tinyskeletons: baby_skeleton baby_stray baby_wither_skeleton
          undead_revamp2: axestrom bomber crackleball flame invisiblebidy slaveman sucker
              the_moonflower thebeartamer thebidy thebidyupside thehorrors thehorrorsdecoys
              thehunter theordure thepregnant therod thesmoker thespectre thespitter theswarmer
              thewolf
          untamedwilds: aardvark arowana baleen_whale bear big_cat bison boar butterfly camel
              catfish football_fish giant_clam giant_salamander hippo hyena king_crab large_snake
              manatee monitor newt opossum rhino sawfish shark snake softshell_turtle spadefish
              spitter sunfish tarantula terror_bird tortoise trevally triggerfish whale_shark
              wildebeest
          wildbackport: allay frog tadpole warden
          """);
  static final Set<String> SUPPORTED_ENTITY_TYPES =
      union(KNOWN_SUPPORTED_ENTITY_TYPES, KNOWN_SUPPORTED_THIRD_PARTY_ENTITY_TYPES);
  static final Set<String> UNSUPPORTED_ENTITY_TYPES =
      union(KNOWN_UNSUPPORTED_ENTITY_TYPES, KNOWN_UNSUPPORTED_THIRD_PARTY_ENTITY_TYPES);

  private RenderEntityTypeSupportDefaults() {}

  private static Set<String> entityTypes(String entityTypesByNamespace) {
    Set<String> entityTypes = new HashSet<>();
    String namespace = "";
    for (String token : entityTypesByNamespace.trim().split("\\s+")) {
      if (token.endsWith(":")) {
        namespace = token;
      } else {
        entityTypes.add(namespace + token);
      }
    }
    return Set.copyOf(entityTypes);
  }

  private static Set<String> union(Set<String> entityTypes, Set<String> thirdPartyEntityTypes) {
    return Stream.concat(entityTypes.stream(), thirdPartyEntityTypes.stream())
        .collect(Collectors.toUnmodifiableSet());
  }
}
