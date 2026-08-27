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
        "short_execution_limit_intervals_enforced",
        ConditionEvaluationTest::testShortExecutionLimitIntervalsEnforced,
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
        "min_hover_height_stays_in_range",
        NavigationTypeTest::testMinHoverHeightStaysInRange,
        DEFAULT_STRUCTURE);
    register(
        "render_entity_type_selects_the_navigation",
        NavigationTypeTest::testRenderEntityTypeSelectsTheNavigation,
        DEFAULT_STRUCTURE);
    register(
        "navigation_type_survives_preset_import",
        NavigationTypeTest::testNavigationTypeSurvivesPresetImport,
        DEFAULT_STRUCTURE);

    register(
        "bat_is_available_as_render_entity",
        RenderEntityTypeTest::testBatIsAvailableAsRenderEntity,
        DEFAULT_STRUCTURE);
    register(
        "pathfinder_mob_lookup_stays_type_safe",
        RenderEntityTypeTest::testPathfinderMobLookupStaysTypeSafe,
        DEFAULT_STRUCTURE);
    register(
        "render_entity_is_reused_per_entity_type",
        RenderEntityTypeTest::testRenderEntityIsReusedPerEntityType,
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
        "preset_import_sets_home", PresetSpawnTest::testPresetImportSetsHome, DEFAULT_STRUCTURE);

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
    register(
        "stored_npc_data_is_deterministic",
        StoredDataTest::testStoredNpcDataIsDeterministic,
        DEFAULT_STRUCTURE);

    register(
        "water_state_is_writable",
        WaterStateAccessTest::testWaterStateIsWritable,
        DEFAULT_STRUCTURE);

    register(
        "fallback_stays_out_when_a_regular_action_fired",
        FallbackActionTest::testFallbackStaysOutWhenARegularActionFired,
        DEFAULT_STRUCTURE);
    register(
        "fallback_runs_without_a_wait",
        FallbackActionTest::testFallbackRunsWithoutAWait,
        DEFAULT_STRUCTURE);
    register(
        "fallback_runs_only_once", FallbackActionTest::testFallbackRunsOnlyOnce, DEFAULT_STRUCTURE);
    register(
        "screen_action_without_a_player_is_skipped",
        FallbackActionTest::testScreenActionWithoutAPlayerIsSkipped,
        DEFAULT_STRUCTURE);

    register(
        "immovable_n_p_c_is_not_pushed",
        ImmovableAttributeTest::testImmovableNPCIsNotPushed,
        DEFAULT_STRUCTURE);
    register(
        "immovable_n_p_c_loses_its_movement_objectives",
        ImmovableAttributeTest::testImmovableNPCLosesItsMovementObjectives,
        DEFAULT_STRUCTURE);

    register(
        "move_keeps_the_chain_running",
        MoveActionTest::testMoveKeepsTheChainRunning,
        DEFAULT_STRUCTURE);
    register(
        "move_and_wait_parks_the_chain",
        MoveActionTest::testMoveAndWaitParksTheChain,
        DEFAULT_STRUCTURE);
    register(
        "arrival_resumes_the_chain", MoveActionTest::testArrivalResumesTheChain, DEFAULT_STRUCTURE);
    register(
        "timeout_resumes_the_chain", MoveActionTest::testTimeoutResumesTheChain, DEFAULT_STRUCTURE);
    register(
        "immovable_n_p_c_does_not_block_the_chain",
        MoveActionTest::testImmovableNPCDoesNotBlockTheChain,
        DEFAULT_STRUCTURE);
    register(
        "second_move_replaces_the_first_goal",
        MoveActionTest::testSecondMoveReplacesTheFirstGoal,
        DEFAULT_STRUCTURE);
    register(
        "move_without_target_is_skipped",
        MoveActionTest::testMoveWithoutTargetIsSkipped,
        DEFAULT_STRUCTURE);

    register(
        "new_n_p_c_is_fully_opaque",
        OpacityAttributeTest::testNewNPCIsFullyOpaque,
        DEFAULT_STRUCTURE);
    register(
        "opacity_action_changes_the_attribute",
        OpacityAttributeTest::testOpacityActionChangesTheAttribute,
        DEFAULT_STRUCTURE);
    register(
        "fully_transparent_opacity_survives_save_and_load",
        OpacityAttributeTest::testFullyTransparentOpacitySurvivesSaveAndLoad,
        DEFAULT_STRUCTURE);
    register(
        "opacity_is_exported_with_the_preset",
        OpacityAttributeTest::testOpacityIsExportedWithThePreset,
        DEFAULT_STRUCTURE);
    register(
        "opacity_is_clamped_to_the_allowed_range",
        OpacityAttributeTest::testOpacityIsClampedToTheAllowedRange,
        DEFAULT_STRUCTURE);

    register(
        "pause_and_resume_of_a_single_n_p_c",
        PauseTest::testPauseAndResumeOfASingleNPC,
        DEFAULT_STRUCTURE);
    register(
        "paused_n_p_c_skips_its_base_tick",
        PauseTest::testPausedNPCSkipsItsBaseTick,
        DEFAULT_STRUCTURE);
    register(
        "pause_survives_save_and_load", PauseTest::testPauseSurvivesSaveAndLoad, DEFAULT_STRUCTURE);
    register(
        "global_pause_covers_every_n_p_c",
        PauseTest::testGlobalPauseCoversEveryNPC,
        DEFAULT_STRUCTURE);
    register(
        "global_resume_keeps_individual_pause",
        PauseTest::testGlobalResumeKeepsIndividualPause,
        DEFAULT_STRUCTURE);
    register(
        "global_pause_ends_with_the_server",
        PauseTest::testGlobalPauseEndsWithTheServer,
        DEFAULT_STRUCTURE);

    register(
        "idle_time_grows_while_the_player_stands_still",
        PlayerIdleTest::testIdleTimeGrowsWhileThePlayerStandsStill,
        DEFAULT_STRUCTURE);
    register(
        "movement_resets_the_idle_time",
        PlayerIdleTest::testMovementResetsTheIdleTime,
        DEFAULT_STRUCTURE);
    register(
        "idle_time_of_an_unknown_player_is_zero",
        PlayerIdleTest::testIdleTimeOfAnUnknownPlayerIsZero,
        DEFAULT_STRUCTURE);

    register(
        "sound_action_plays_a_known_sound",
        SoundActionTest::testSoundActionPlaysAKnownSound,
        DEFAULT_STRUCTURE);
    register(
        "sound_action_keeps_its_source_volume_and_pitch",
        SoundActionTest::testSoundActionKeepsItsSourceVolumeAndPitch,
        DEFAULT_STRUCTURE);
    register(
        "sound_action_without_a_valid_sound_is_skipped",
        SoundActionTest::testSoundActionWithoutAValidSoundIsSkipped,
        DEFAULT_STRUCTURE);

    register(
        "trade_actions_trigger_for_humanoid",
        TradingActionTest::testTradeActionsTriggerForHumanoid,
        DEFAULT_STRUCTURE);
    register(
        "trade_actions_trigger_for_villager",
        TradingActionTest::testTradeActionsTriggerForVillager,
        DEFAULT_STRUCTURE);
    register(
        "trade_actions_trigger_for_wandering_trader",
        TradingActionTest::testTradeActionsTriggerForWanderingTrader,
        DEFAULT_STRUCTURE);

    register(
        "wait_delays_the_following_actions",
        WaitActionTest::testWaitDelaysTheFollowingActions,
        DEFAULT_STRUCTURE);
    register(
        "remaining_wait_survives_save_and_load",
        WaitActionTest::testRemainingWaitSurvivesSaveAndLoad,
        DEFAULT_STRUCTURE);
    register(
        "second_trigger_is_discarded",
        WaitActionTest::testSecondTriggerIsDiscarded,
        DEFAULT_STRUCTURE);
    register(
        "different_events_run_in_parallel",
        WaitActionTest::testDifferentEventsRunInParallel,
        DEFAULT_STRUCTURE);
    register(
        "interval_set_with_wait_runs_in_order",
        WaitActionTest::testIntervalSetWithWaitRunsInOrder,
        DEFAULT_STRUCTURE);
    register(
        "interval_set_without_wait_picks_one_entry",
        WaitActionTest::testIntervalSetWithoutWaitPicksOneEntry,
        DEFAULT_STRUCTURE);
    register(
        "screen_action_is_kept_over_the_wait",
        WaitActionTest::testScreenActionIsKeptOverTheWait,
        DEFAULT_STRUCTURE);
    register(
        "fallback_runs_once_after_the_wait",
        WaitActionTest::testFallbackRunsOnceAfterTheWait,
        DEFAULT_STRUCTURE);
    register(
        "preset_import_cancels_the_chain",
        WaitActionTest::testPresetImportCancelsTheChain,
        DEFAULT_STRUCTURE);
    register(
        "death_cancels_the_chain", WaitActionTest::testDeathCancelsTheChain, DEFAULT_STRUCTURE);
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
