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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import java.util.function.Consumer;
import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraftforge.eventbus.api.bus.BusGroup;
import net.minecraftforge.fml.loading.FMLLoader;
import net.minecraftforge.registries.DeferredRegister;

/**
 * Registers the game test functions of this mod.
 *
 * <p>Forge ships the {@code @GameTest} annotation but never scans for it, so every test method is
 * registered here and paired with a {@code data/<mod id>/test_instance} entry.
 */
public final class ModGameTests {

  private static final DeferredRegister<Consumer<GameTestHelper>> TEST_FUNCTIONS =
      DeferredRegister.create(Registries.TEST_FUNCTION, Constants.MOD_ID);

  static {
    TEST_FUNCTIONS.register(
        "item_quantity_conditions", () -> ConditionEvaluationTest::testItemQuantityConditions);
    TEST_FUNCTIONS.register(
        "health_target_conditions", () -> ConditionEvaluationTest::testHealthTargetConditions);
    TEST_FUNCTIONS.register(
        "button_execution_limit_enforced",
        () -> ConditionEvaluationTest::testButtonExecutionLimitEnforced);
    TEST_FUNCTIONS.register(
        "conditional_dialog_open_respects_conditions",
        () -> ConditionEvaluationTest::testConditionalDialogOpenRespectsConditions);
    TEST_FUNCTIONS.register(
        "default_dialog_execution_limit_enforced",
        () -> ConditionEvaluationTest::testDefaultDialogExecutionLimitEnforced);

    TEST_FUNCTIONS.register("open_dialog", () -> DialogScreenTest::testOpenDialog);
    TEST_FUNCTIONS.register("open_basic_dialog", () -> DialogScreenTest::testOpenBasicDialog);
    TEST_FUNCTIONS.register("open_yes_no_dialog", () -> DialogScreenTest::testOpenYesNoDialog);

    TEST_FUNCTIONS.register(
        "no_gravity_is_applied_on_preset_import",
        () -> EnvironmentalAttributeTest::testNoGravityIsAppliedOnPresetImport);
    TEST_FUNCTIONS.register(
        "no_gravity_is_cleared_on_preset_import",
        () -> EnvironmentalAttributeTest::testNoGravityIsClearedOnPresetImport);

    TEST_FUNCTIONS.register(
        "normal_player_teleport_with_gamemaster_allow_list",
        () -> ExecuteAsUserCommandTest::testNormalPlayerTeleportWithGamemasterAllowList);

    TEST_FUNCTIONS.register(
        "faction_defense_targets_outside_attacker",
        () -> FactionDefenseTest::testFactionDefenseTargetsOutsideAttacker);
    TEST_FUNCTIONS.register(
        "faction_defense_ignores_internal_dispute",
        () -> FactionDefenseTest::testFactionDefenseIgnoresInternalDispute);
    TEST_FUNCTIONS.register(
        "faction_defense_defends_faction_player",
        () -> FactionDefenseTest::testFactionDefenseDefendsFactionPlayer);
    TEST_FUNCTIONS.register(
        "faction_defense_requires_faction",
        () -> FactionDefenseTest::testFactionDefenseRequiresFaction);
    TEST_FUNCTIONS.register(
        "defend_self_does_not_alert_others",
        () -> FactionDefenseTest::testDefendSelfDoesNotAlertOthers);

    TEST_FUNCTIONS.register(
        "faction_scoreboard_membership",
        () -> FactionObjectiveTest::testFactionScoreboardMembership);
    TEST_FUNCTIONS.register(
        "directed_hostile_faction_targeting",
        () -> FactionObjectiveTest::testDirectedHostileFactionTargeting);
    TEST_FUNCTIONS.register(
        "hostile_faction_combat_bypass",
        () -> FactionObjectiveTest::testHostileFactionCombatBypass);
    TEST_FUNCTIONS.register(
        "player_faction_combat_bypass", () -> FactionObjectiveTest::testPlayerFactionCombatBypass);
    TEST_FUNCTIONS.register(
        "faction_survives_preset_round_trip",
        () -> FactionObjectiveTest::testFactionSurvivesPresetRoundTrip);

    TEST_FUNCTIONS.register(
        "evoker_crossed_arms_variant_drives_arm_pose",
        () -> IllagerCrossedArmsTest::testEvokerCrossedArmsVariantDrivesArmPose);
    TEST_FUNCTIONS.register(
        "illusioner_crossed_arms_variant_drives_arm_pose",
        () -> IllagerCrossedArmsTest::testIllusionerCrossedArmsVariantDrivesArmPose);
    TEST_FUNCTIONS.register(
        "vindicator_crossed_arms_variant_drives_arm_pose",
        () -> IllagerCrossedArmsTest::testVindicatorCrossedArmsVariantDrivesArmPose);

    TEST_FUNCTIONS.register(
        "allay_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useAllayNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "bogged_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useBoggedNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "cat_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useCatNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "chicken_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useChickenNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "creeper_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useCreeperNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "drowned_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useDrownedNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "enderman_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useEndermanNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "evoker_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useEvokerNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "fox_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useFoxNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "ghast_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useGhastNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "horse_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useHorseNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "horse_skeleton_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useHorseSkeletonNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "horse_zombie_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useHorseZombieNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "humanoid_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useHumanoidNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "humanoid_slim_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useHumanoidSlimNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "illusioner_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useIllusionerNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "iron_golem_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useIronGolemNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "piglin_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::usePiglinNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "piglin_brute_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::usePiglinBruteNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "piglin_zombified_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::usePiglinZombifiedNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "pig_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::usePigNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "pillager_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::usePillagerNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "skeleton_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useSkeletonNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "stray_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useStrayNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "wither_skeleton_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useWitherSkeletonNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "slime_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useSlimeNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "spider_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useSpiderNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "cave_spider_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useCaveSpiderNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "villager_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useVillagerNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "wandering_trader_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useWanderingTraderNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "vex_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useVexNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "vindicator_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useVindicatorNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "witch_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useWitchNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "wolf_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useWolfNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "zombie_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useZombieNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "zombie_husk_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useZombieHuskNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "zombie_villager_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useZombieVillagerNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "doppler_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useDopplerNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "fairy_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useFairyNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "orc_n_p_c_spawn_egg_item", () -> ModSpawnEggItemTest::useOrcNPCSpawnEggItem);
    TEST_FUNCTIONS.register(
        "orc_warrior_n_p_c_spawn_egg_item",
        () -> ModSpawnEggItemTest::useOrcWarriorNPCSpawnEggItem);

    TEST_FUNCTIONS.register(
        "server_registry_isolation", () -> NPCDataIsolationTest::testServerRegistryIsolation);
    TEST_FUNCTIONS.register("pose_isolation", () -> NPCDataIsolationTest::testPoseIsolation);
    TEST_FUNCTIONS.register(
        "pose_name_isolation", () -> NPCDataIsolationTest::testPoseNameIsolation);
    TEST_FUNCTIONS.register(
        "rotation_isolation", () -> NPCDataIsolationTest::testRotationIsolation);
    TEST_FUNCTIONS.register(
        "animation_isolation", () -> NPCDataIsolationTest::testAnimationIsolation);
    TEST_FUNCTIONS.register("scale_isolation", () -> NPCDataIsolationTest::testScaleIsolation);
    TEST_FUNCTIONS.register(
        "root_data_isolation", () -> NPCDataIsolationTest::testRootDataIsolation);
    TEST_FUNCTIONS.register("name_update", () -> NPCDataIsolationTest::testNameUpdate);
    TEST_FUNCTIONS.register("skin_update", () -> NPCDataIsolationTest::testSkinUpdate);
    TEST_FUNCTIONS.register(
        "scale_updates_dimensions", () -> NPCDataIsolationTest::testScaleUpdatesDimensions);
    TEST_FUNCTIONS.register(
        "position_isolation", () -> NPCDataIsolationTest::testPositionIsolation);
    TEST_FUNCTIONS.register(
        "visibility_isolation", () -> NPCDataIsolationTest::testVisibilityIsolation);

    TEST_FUNCTIONS.register(
        "n_p_c_entity_data_initialized",
        () -> NPCEntityLifecycleTest::testNPCEntityDataInitialized);
    TEST_FUNCTIONS.register(
        "n_p_c_is_indexed_on_spawn", () -> NPCEntityLifecycleTest::testNPCIsIndexedOnSpawn);
    TEST_FUNCTIONS.register(
        "owner_index_update", () -> NPCEntityLifecycleTest::testOwnerIndexUpdate);
    TEST_FUNCTIONS.register(
        "repeated_owner_update_is_idempotent",
        () -> NPCEntityLifecycleTest::testRepeatedOwnerUpdateIsIdempotent);
    TEST_FUNCTIONS.register(
        "follow_owner_without_target_is_registered",
        () -> NPCEntityLifecycleTest::testFollowOwnerWithoutTargetIsRegistered);
    TEST_FUNCTIONS.register(
        "custom_identifier_is_queryable",
        () -> NPCEntityLifecycleTest::testCustomIdentifierIsQueryable);
    TEST_FUNCTIONS.register(
        "respawn_keeps_owner", () -> NPCEntityLifecycleTest::testRespawnKeepsOwner);

    TEST_FUNCTIONS.register(
        "spawned_n_p_c_has_home_position",
        () -> NavigationHomePositionTest::testSpawnedNPCHasHomePosition);
    TEST_FUNCTIONS.register(
        "home_position_is_synched", () -> NavigationHomePositionTest::testHomePositionIsSynched);
    TEST_FUNCTIONS.register(
        "home_position_survives_save_and_load",
        () -> NavigationHomePositionTest::testHomePositionSurvivesSaveAndLoad);

    TEST_FUNCTIONS.register(
        "command_block_preset_import",
        () -> PresetCommandSecurityTest::testCommandBlockPresetImport);
    TEST_FUNCTIONS.register(
        "player_preset_import", () -> PresetCommandSecurityTest::testPlayerPresetImport);
    TEST_FUNCTIONS.register(
        "console_preset_import", () -> PresetCommandSecurityTest::testConsolePresetImport);
    TEST_FUNCTIONS.register(
        "console_preset_import_with_owner",
        () -> PresetCommandSecurityTest::testConsolePresetImportWithOwner);

    TEST_FUNCTIONS.register("mod_registered", () -> SmokeTest::testModRegistered);

    TEST_FUNCTIONS.register(
        "trade_use_persistence_after_nbt_round_trip",
        () -> TradingPersistenceTest::testTradeUsePersistenceAfterNbtRoundTrip);
    TEST_FUNCTIONS.register(
        "advanced_max_uses_preserves_current_uses",
        () -> TradingPersistenceTest::testAdvancedMaxUsesPreservesCurrentUses);
    TEST_FUNCTIONS.register(
        "modified_trade_item_persistence",
        () -> TradingPersistenceTest::testModifiedTradeItemPersistence);
    TEST_FUNCTIONS.register(
        "basic_trading_offers_update_applies_correct_fields",
        () -> TradingPersistenceTest::testBasicTradingOffersUpdateAppliesCorrectFields);
    TEST_FUNCTIONS.register(
        "timed_trading_reset_restores_uses",
        () -> TradingPersistenceTest::testTimedTradingResetRestoresUses);

    TEST_FUNCTIONS.register(
        "villager_variant_resolves_profession_and_type",
        () -> VillagerVariantTest::testVillagerVariantResolvesProfessionAndType);

    TEST_FUNCTIONS.register(
        "night_visibility_is_respected",
        () -> VisibilityAttributeTest::testNightVisibilityIsRespected);
    TEST_FUNCTIONS.register(
        "owner_does_not_override_night_visibility",
        () -> VisibilityAttributeTest::testOwnerDoesNotOverrideNightVisibility);
    TEST_FUNCTIONS.register(
        "owner_visibility_is_respected",
        () -> VisibilityAttributeTest::testOwnerVisibilityIsRespected);
    TEST_FUNCTIONS.register(
        "game_mode_visibility_is_respected",
        () -> VisibilityAttributeTest::testGameModeVisibilityIsRespected);
    TEST_FUNCTIONS.register(
        "preview_resolves_simulated_day_time",
        () -> VisibilityAttributeTest::testPreviewResolvesSimulatedDayTime);
    TEST_FUNCTIONS.register(
        "main_visibility_is_respected",
        () -> VisibilityAttributeTest::testMainVisibilityIsRespected);
  }

  private ModGameTests() {}

  public static void register(BusGroup modBusGroup) {
    // Test instances are shipped as data pack entries, so registering the matching test
    // functions outside a development environment would offer the game tests through the
    // /test command of a live world.
    if (FMLLoader.isProduction()) {
      return;
    }

    TEST_FUNCTIONS.register(modBusGroup);
  }
}
