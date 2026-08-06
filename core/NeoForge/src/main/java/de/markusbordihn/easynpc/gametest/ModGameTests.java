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
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.gametest.framework.TestEnvironmentDefinition;
import net.minecraft.resources.Identifier;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.event.RegisterGameTestsEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

/**
 * Registers the game test functions of this mod and turns each of them into a test instance.
 *
 * <p>NeoForge has no annotation based game test discovery, so every test method is registered
 * manually here and paired with the structure it should run in.
 */
@EventBusSubscriber
public final class ModGameTests {

  private static final DeferredRegister<Consumer<GameTestHelper>> TEST_FUNCTIONS =
      DeferredRegister.create(BuiltInRegistries.TEST_FUNCTION, Constants.MOD_ID);

  private static final List<TestEntry> TEST_ENTRIES = new ArrayList<>();
  private static final int DEFAULT_MAX_TICKS = 100;
  private static final Identifier DEFAULT_STRUCTURE = Identifier.parse("easy_npc:gametest.3x3x3");
  private static final Identifier SMOKE_STRUCTURE = Identifier.parse("easy_npc:gametest.1x1x1");

  static {
    register(
        "item_quantity_conditions",
        ConditionEvaluationTest::testItemQuantityConditions,
        DEFAULT_STRUCTURE);
    register(
        "health_target_conditions",
        ConditionEvaluationTest::testHealthTargetConditions,
        DEFAULT_STRUCTURE);
    register(
        "button_execution_limit_enforced",
        ConditionEvaluationTest::testButtonExecutionLimitEnforced,
        DEFAULT_STRUCTURE);
    register(
        "conditional_dialog_open_respects_conditions",
        ConditionEvaluationTest::testConditionalDialogOpenRespectsConditions,
        DEFAULT_STRUCTURE);
    register(
        "default_dialog_execution_limit_enforced",
        ConditionEvaluationTest::testDefaultDialogExecutionLimitEnforced,
        DEFAULT_STRUCTURE);

    register("open_dialog", DialogScreenTest::testOpenDialog, DEFAULT_STRUCTURE);
    register("open_basic_dialog", DialogScreenTest::testOpenBasicDialog, DEFAULT_STRUCTURE);
    register("open_yes_no_dialog", DialogScreenTest::testOpenYesNoDialog, DEFAULT_STRUCTURE);

    register(
        "no_gravity_is_applied_on_preset_import",
        EnvironmentalAttributeTest::testNoGravityIsAppliedOnPresetImport,
        DEFAULT_STRUCTURE);
    register(
        "no_gravity_is_cleared_on_preset_import",
        EnvironmentalAttributeTest::testNoGravityIsClearedOnPresetImport,
        DEFAULT_STRUCTURE);

    register(
        "normal_player_teleport_with_gamemaster_allow_list",
        ExecuteAsUserCommandTest::testNormalPlayerTeleportWithGamemasterAllowList,
        DEFAULT_STRUCTURE);

    register(
        "faction_defense_targets_outside_attacker",
        FactionDefenseTest::testFactionDefenseTargetsOutsideAttacker,
        DEFAULT_STRUCTURE);
    register(
        "faction_defense_ignores_internal_dispute",
        FactionDefenseTest::testFactionDefenseIgnoresInternalDispute,
        DEFAULT_STRUCTURE);
    register(
        "faction_defense_defends_faction_player",
        FactionDefenseTest::testFactionDefenseDefendsFactionPlayer,
        DEFAULT_STRUCTURE);
    register(
        "faction_defense_requires_faction",
        FactionDefenseTest::testFactionDefenseRequiresFaction,
        DEFAULT_STRUCTURE);
    register(
        "defend_self_does_not_alert_others",
        FactionDefenseTest::testDefendSelfDoesNotAlertOthers,
        DEFAULT_STRUCTURE);

    register(
        "faction_scoreboard_membership",
        FactionObjectiveTest::testFactionScoreboardMembership,
        DEFAULT_STRUCTURE);
    register(
        "directed_hostile_faction_targeting",
        FactionObjectiveTest::testDirectedHostileFactionTargeting,
        DEFAULT_STRUCTURE);
    register(
        "hostile_faction_combat_bypass",
        FactionObjectiveTest::testHostileFactionCombatBypass,
        DEFAULT_STRUCTURE);
    register(
        "player_faction_combat_bypass",
        FactionObjectiveTest::testPlayerFactionCombatBypass,
        DEFAULT_STRUCTURE);
    register(
        "faction_survives_preset_round_trip",
        FactionObjectiveTest::testFactionSurvivesPresetRoundTrip,
        DEFAULT_STRUCTURE);

    register(
        "evoker_crossed_arms_variant_drives_arm_pose",
        IllagerCrossedArmsTest::testEvokerCrossedArmsVariantDrivesArmPose,
        DEFAULT_STRUCTURE);
    register(
        "illusioner_crossed_arms_variant_drives_arm_pose",
        IllagerCrossedArmsTest::testIllusionerCrossedArmsVariantDrivesArmPose,
        DEFAULT_STRUCTURE);
    register(
        "vindicator_crossed_arms_variant_drives_arm_pose",
        IllagerCrossedArmsTest::testVindicatorCrossedArmsVariantDrivesArmPose,
        DEFAULT_STRUCTURE);

    register(
        "allay_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useAllayNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "bogged_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useBoggedNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "cat_n_p_c_spawn_egg_item", ModSpawnEggItemTest::useCatNPCSpawnEggItem, DEFAULT_STRUCTURE);
    register(
        "chicken_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useChickenNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "creeper_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useCreeperNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "drowned_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useDrownedNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "enderman_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useEndermanNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "evoker_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useEvokerNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "fox_n_p_c_spawn_egg_item", ModSpawnEggItemTest::useFoxNPCSpawnEggItem, DEFAULT_STRUCTURE);
    register(
        "ghast_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useGhastNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "horse_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useHorseNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "horse_skeleton_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useHorseSkeletonNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "horse_zombie_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useHorseZombieNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "humanoid_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useHumanoidNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "humanoid_slim_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useHumanoidSlimNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "illusioner_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useIllusionerNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "iron_golem_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useIronGolemNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "piglin_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::usePiglinNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "piglin_brute_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::usePiglinBruteNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "piglin_zombified_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::usePiglinZombifiedNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "pig_n_p_c_spawn_egg_item", ModSpawnEggItemTest::usePigNPCSpawnEggItem, DEFAULT_STRUCTURE);
    register(
        "pillager_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::usePillagerNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "skeleton_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useSkeletonNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "stray_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useStrayNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "wither_skeleton_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useWitherSkeletonNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "slime_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useSlimeNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "spider_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useSpiderNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "cave_spider_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useCaveSpiderNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "villager_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useVillagerNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "wandering_trader_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useWanderingTraderNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "vex_n_p_c_spawn_egg_item", ModSpawnEggItemTest::useVexNPCSpawnEggItem, DEFAULT_STRUCTURE);
    register(
        "vindicator_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useVindicatorNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "witch_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useWitchNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "wolf_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useWolfNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "zombie_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useZombieNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "zombie_husk_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useZombieHuskNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "zombie_villager_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useZombieVillagerNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "doppler_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useDopplerNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "fairy_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useFairyNPCSpawnEggItem,
        DEFAULT_STRUCTURE);
    register(
        "orc_n_p_c_spawn_egg_item", ModSpawnEggItemTest::useOrcNPCSpawnEggItem, DEFAULT_STRUCTURE);
    register(
        "orc_warrior_n_p_c_spawn_egg_item",
        ModSpawnEggItemTest::useOrcWarriorNPCSpawnEggItem,
        DEFAULT_STRUCTURE);

    register(
        "server_registry_isolation",
        NPCDataIsolationTest::testServerRegistryIsolation,
        DEFAULT_STRUCTURE);
    register("pose_isolation", NPCDataIsolationTest::testPoseIsolation, DEFAULT_STRUCTURE);
    register("pose_name_isolation", NPCDataIsolationTest::testPoseNameIsolation, DEFAULT_STRUCTURE);
    register("rotation_isolation", NPCDataIsolationTest::testRotationIsolation, DEFAULT_STRUCTURE);
    register(
        "animation_isolation", NPCDataIsolationTest::testAnimationIsolation, DEFAULT_STRUCTURE);
    register("scale_isolation", NPCDataIsolationTest::testScaleIsolation, DEFAULT_STRUCTURE);
    register("root_data_isolation", NPCDataIsolationTest::testRootDataIsolation, DEFAULT_STRUCTURE);
    register("name_update", NPCDataIsolationTest::testNameUpdate, DEFAULT_STRUCTURE);
    register("skin_update", NPCDataIsolationTest::testSkinUpdate, DEFAULT_STRUCTURE);
    register(
        "scale_updates_dimensions",
        NPCDataIsolationTest::testScaleUpdatesDimensions,
        DEFAULT_STRUCTURE);
    register("position_isolation", NPCDataIsolationTest::testPositionIsolation, DEFAULT_STRUCTURE);
    register(
        "visibility_isolation", NPCDataIsolationTest::testVisibilityIsolation, DEFAULT_STRUCTURE);

    register(
        "n_p_c_entity_data_initialized",
        NPCEntityLifecycleTest::testNPCEntityDataInitialized,
        DEFAULT_STRUCTURE);
    register(
        "n_p_c_is_indexed_on_spawn",
        NPCEntityLifecycleTest::testNPCIsIndexedOnSpawn,
        DEFAULT_STRUCTURE);
    register("owner_index_update", NPCEntityLifecycleTest::testOwnerIndexUpdate, DEFAULT_STRUCTURE);
    register(
        "repeated_owner_update_is_idempotent",
        NPCEntityLifecycleTest::testRepeatedOwnerUpdateIsIdempotent,
        DEFAULT_STRUCTURE);
    register(
        "follow_owner_without_target_is_registered",
        NPCEntityLifecycleTest::testFollowOwnerWithoutTargetIsRegistered,
        DEFAULT_STRUCTURE);
    register(
        "custom_identifier_is_queryable",
        NPCEntityLifecycleTest::testCustomIdentifierIsQueryable,
        DEFAULT_STRUCTURE);
    register(
        "respawn_keeps_owner", NPCEntityLifecycleTest::testRespawnKeepsOwner, DEFAULT_STRUCTURE);

    register(
        "spawned_n_p_c_has_home_position",
        NavigationHomePositionTest::testSpawnedNPCHasHomePosition,
        DEFAULT_STRUCTURE);
    register(
        "home_position_is_synched",
        NavigationHomePositionTest::testHomePositionIsSynched,
        DEFAULT_STRUCTURE);
    register(
        "home_position_survives_save_and_load",
        NavigationHomePositionTest::testHomePositionSurvivesSaveAndLoad,
        DEFAULT_STRUCTURE);

    register(
        "every_command_argument_can_be_synchronized",
        CommandArgumentTypeTest::testEveryCommandArgumentCanBeSynchronized,
        SMOKE_STRUCTURE);

    register(
        "duplicate_button_label_is_rejected",
        DialogButtonIdentityTest::testDuplicateButtonLabelIsRejected,
        DEFAULT_STRUCTURE);
    register(
        "button_identity_survives_save_and_load",
        DialogButtonIdentityTest::testButtonIdentitySurvivesSaveAndLoad,
        DEFAULT_STRUCTURE);
    register(
        "edited_button_label_stays_addressable",
        DialogButtonIdentityTest::testEditedButtonLabelStaysAddressable,
        DEFAULT_STRUCTURE);

    register(
        "distance_actions_trigger_per_range",
        DistanceActionTest::testDistanceActionsTriggerPerRange,
        DEFAULT_STRUCTURE);
    register(
        "distance_actions_run_once_per_player",
        DistanceActionTest::testDistanceActionsRunOncePerPlayer,
        DEFAULT_STRUCTURE);
    register(
        "distance_actions_trigger_again_after_leaving",
        DistanceActionTest::testDistanceActionsTriggerAgainAfterLeaving,
        DEFAULT_STRUCTURE);

    register(
        "navigation_type_changes_the_navigation",
        NavigationTypeTest::testNavigationTypeChangesTheNavigation,
        DEFAULT_STRUCTURE);
    register(
        "navigation_is_only_refreshed_on_change",
        NavigationTypeTest::testNavigationIsOnlyRefreshedOnChange,
        DEFAULT_STRUCTURE);
    register(
        "gravity_is_restored_after_flying",
        NavigationTypeTest::testGravityIsRestoredAfterFlying,
        DEFAULT_STRUCTURE);
    register(
        "hover_height_stays_in_range",
        NavigationTypeTest::testHoverHeightStaysInRange,
        DEFAULT_STRUCTURE);
    register(
        "navigation_type_survives_preset_import",
        NavigationTypeTest::testNavigationTypeSurvivesPresetImport,
        DEFAULT_STRUCTURE);

    register(
        "state_action_applies_every_operation",
        NpcStateTest::testStateActionAppliesEveryOperation,
        DEFAULT_STRUCTURE);
    register(
        "invalid_state_action_is_ignored",
        NpcStateTest::testInvalidStateActionIsIgnored,
        DEFAULT_STRUCTURE);
    register(
        "debug_action_still_writes_the_state",
        NpcStateTest::testDebugActionStillWritesTheState,
        DEFAULT_STRUCTURE);
    register(
        "state_survives_save_and_load",
        NpcStateTest::testStateSurvivesSaveAndLoad,
        DEFAULT_STRUCTURE);
    register(
        "state_condition_locks_dialog_button",
        NpcStateTest::testStateConditionLocksDialogButton,
        DEFAULT_STRUCTURE);
    register(
        "state_is_reset_on_preset_import",
        NpcStateTest::testStateIsResetOnPresetImport,
        DEFAULT_STRUCTURE);
    register(
        "state_change_notifies_listener",
        NpcStateTest::testStateChangeNotifiesListener,
        DEFAULT_STRUCTURE);
    register(
        "forge_keeper_preset_uses_states",
        NpcStateTest::testForgeKeeperPresetUsesStates,
        DEFAULT_STRUCTURE);

    register(
        "every_objective_type_creates_its_goal",
        ObjectiveGoalCreationTest::testEveryObjectiveTypeCreatesItsGoal,
        DEFAULT_STRUCTURE);
    register(
        "every_objective_type_survives_ticking",
        ObjectiveGoalCreationTest::testEveryObjectiveTypeSurvivesTicking,
        DEFAULT_STRUCTURE);

    register(
        "objective_values_from_preset_are_clamped",
        ObjectivePresetTest::testObjectiveValuesFromPresetAreClamped,
        DEFAULT_STRUCTURE);
    register(
        "player_target_objective_is_registered_on_join",
        ObjectivePresetTest::testPlayerTargetObjectiveIsRegisteredOnJoin,
        DEFAULT_STRUCTURE);
    register(
        "entity_target_objective_is_released_on_leave",
        ObjectivePresetTest::testEntityTargetObjectiveIsReleasedOnLeave,
        DEFAULT_STRUCTURE);

    register(
        "command_block_preset_import",
        PresetCommandSecurityTest::testCommandBlockPresetImport,
        DEFAULT_STRUCTURE);
    register(
        "player_preset_import",
        PresetCommandSecurityTest::testPlayerPresetImport,
        DEFAULT_STRUCTURE);
    register(
        "console_preset_import",
        PresetCommandSecurityTest::testConsolePresetImport,
        DEFAULT_STRUCTURE);
    register(
        "console_preset_import_with_owner",
        PresetCommandSecurityTest::testConsolePresetImportWithOwner,
        DEFAULT_STRUCTURE);

    register("mod_registered", SmokeTest::testModRegistered, SMOKE_STRUCTURE);

    register(
        "trade_use_persistence_after_nbt_round_trip",
        TradingPersistenceTest::testTradeUsePersistenceAfterNbtRoundTrip,
        DEFAULT_STRUCTURE);
    register(
        "advanced_max_uses_preserves_current_uses",
        TradingPersistenceTest::testAdvancedMaxUsesPreservesCurrentUses,
        DEFAULT_STRUCTURE);
    register(
        "modified_trade_item_persistence",
        TradingPersistenceTest::testModifiedTradeItemPersistence,
        DEFAULT_STRUCTURE);
    register(
        "basic_trading_offers_update_applies_correct_fields",
        TradingPersistenceTest::testBasicTradingOffersUpdateAppliesCorrectFields,
        DEFAULT_STRUCTURE);
    register(
        "timed_trading_reset_restores_uses",
        TradingPersistenceTest::testTimedTradingResetRestoresUses,
        DEFAULT_STRUCTURE);

    register(
        "villager_variant_resolves_profession_and_type",
        VillagerVariantTest::testVillagerVariantResolvesProfessionAndType,
        DEFAULT_STRUCTURE);

    register(
        "night_visibility_is_respected",
        VisibilityAttributeTest::testNightVisibilityIsRespected,
        DEFAULT_STRUCTURE);
    register(
        "owner_does_not_override_night_visibility",
        VisibilityAttributeTest::testOwnerDoesNotOverrideNightVisibility,
        DEFAULT_STRUCTURE);
    register(
        "owner_visibility_is_respected",
        VisibilityAttributeTest::testOwnerVisibilityIsRespected,
        DEFAULT_STRUCTURE);
    register(
        "game_mode_visibility_is_respected",
        VisibilityAttributeTest::testGameModeVisibilityIsRespected,
        DEFAULT_STRUCTURE);
    register(
        "preview_resolves_simulated_day_time",
        VisibilityAttributeTest::testPreviewResolvesSimulatedDayTime,
        DEFAULT_STRUCTURE);
    register(
        "main_visibility_is_respected",
        VisibilityAttributeTest::testMainVisibilityIsRespected,
        DEFAULT_STRUCTURE);

    register(
        "base_preset_exists_for_every_spawn_egg",
        BasePresetTest::testBasePresetExistsForEverySpawnEgg,
        DEFAULT_STRUCTURE);
    register(
        "base_presets_are_written", BasePresetTest::testBasePresetsAreWritten, DEFAULT_STRUCTURE);
    register(
        "shipped_base_presets_are_up_to_date",
        BasePresetTest::testShippedBasePresetsAreUpToDate,
        DEFAULT_STRUCTURE);
    register(
        "base_preset_is_usable_as_parent",
        BasePresetTest::testBasePresetIsUsableAsParent,
        DEFAULT_STRUCTURE);

    register(
        "default_presets_are_valid",
        DefaultPresetRoundTripTest::testDefaultPresetsAreValid,
        DEFAULT_STRUCTURE);
    register(
        "default_presets_survive_regeneration",
        DefaultPresetRoundTripTest::testDefaultPresetsSurviveRegeneration,
        DEFAULT_STRUCTURE);

    register(
        "audience_contains_every_player_in_range",
        IntervalActionAudienceTest::testAudienceContainsEveryPlayerInRange,
        DEFAULT_STRUCTURE);
    register(
        "owner_is_preferred_as_initiator",
        IntervalActionAudienceTest::testOwnerIsPreferredAsInitiator,
        DEFAULT_STRUCTURE);
    register(
        "owner_condition_limits_the_event",
        IntervalActionAudienceTest::testOwnerConditionLimitsTheEvent,
        DEFAULT_STRUCTURE);
    register(
        "nothing_fires_without_the_owner",
        IntervalActionAudienceTest::testNothingFiresWithoutTheOwner,
        DEFAULT_STRUCTURE);

    register(
        "missing_objective_data_keeps_default_objectives",
        PresetDefaultBaselineTest::testMissingObjectiveDataKeepsDefaultObjectives,
        DEFAULT_STRUCTURE);
    register(
        "empty_objective_data_removes_all_objectives",
        PresetDefaultBaselineTest::testEmptyObjectiveDataRemovesAllObjectives,
        DEFAULT_STRUCTURE);
    register(
        "custom_objective_data_replaces_default_objectives",
        PresetDefaultBaselineTest::testCustomObjectiveDataReplacesDefaultObjectives,
        DEFAULT_STRUCTURE);

    register(
        "preset_survives_round_trip",
        PresetRoundTripTest::testPresetSurvivesRoundTrip,
        DEFAULT_STRUCTURE);
    register(
        "preset_survives_round_trip_for_villager",
        PresetRoundTripTest::testPresetSurvivesRoundTripForVillager,
        DEFAULT_STRUCTURE);
    register(
        "configuration_survives_round_trip",
        PresetRoundTripTest::testConfigurationSurvivesRoundTrip,
        DEFAULT_STRUCTURE);
    register("export_is_compact", PresetRoundTripTest::testExportIsCompact, DEFAULT_STRUCTURE);
    register(
        "export_is_compact_for_villager",
        PresetRoundTripTest::testExportIsCompactForVillager,
        DEFAULT_STRUCTURE);

    register(
        "preset_item_spawns_configured_n_p_c",
        PresetSpawnTest::testPresetItemSpawnsConfiguredNPC,
        DEFAULT_STRUCTURE);
    register(
        "preset_item_spawns_configured_villager",
        PresetSpawnTest::testPresetItemSpawnsConfiguredVillager,
        DEFAULT_STRUCTURE);
    register(
        "spawner_spawns_configured_n_p_c",
        PresetSpawnTest::testSpawnerSpawnsConfiguredNPC,
        DEFAULT_STRUCTURE);

    register(
        "npc_without_stored_sounds_still_has_sounds",
        StoredDataTest::testNpcWithoutStoredSoundsStillHasSounds,
        DEFAULT_STRUCTURE);
    register(
        "villager_without_stored_sounds_still_has_sounds",
        StoredDataTest::testVillagerWithoutStoredSoundsStillHasSounds,
        DEFAULT_STRUCTURE);
    register(
        "unchanged_sounds_are_not_stored",
        StoredDataTest::testUnchangedSoundsAreNotStored,
        DEFAULT_STRUCTURE);
    register(
        "objectives_survive_without_target_flags",
        StoredDataTest::testObjectivesSurviveWithoutTargetFlags,
        DEFAULT_STRUCTURE);
    register(
        "unchanged_npc_stores_no_boilerplate",
        StoredDataTest::testUnchangedNpcStoresNoBoilerplate,
        DEFAULT_STRUCTURE);
  }

  private ModGameTests() {}

  public static void register(IEventBus modEventBus) {
    // NeoForge only fires RegisterGameTestsEvent outside production, so the test functions
    // would stay unused there anyway.
    if (FMLEnvironment.isProduction()) {
      return;
    }

    TEST_FUNCTIONS.register(modEventBus);
  }

  private static void register(
      String name, Consumer<GameTestHelper> testFunction, Identifier structure) {
    TEST_ENTRIES.add(new TestEntry(TEST_FUNCTIONS.register(name, () -> testFunction), structure));
  }

  @SubscribeEvent
  public static void registerGameTests(RegisterGameTestsEvent event) {
    Holder<TestEnvironmentDefinition<?>> environment =
        event.registerEnvironment(
            Identifier.fromNamespaceAndPath(Constants.MOD_ID, "default"),
            new TestEnvironmentDefinition.AllOf(List.of()));

    for (TestEntry testEntry : TEST_ENTRIES) {
      event.registerTest(
          testEntry.testFunction().getId(),
          new FunctionGameTestInstance(
              testEntry.testFunction().getKey(),
              new TestData<>(environment, testEntry.structure(), DEFAULT_MAX_TICKS, 0, true)));
    }
  }

  private record TestEntry(
      DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> testFunction,
      Identifier structure) {}
}
