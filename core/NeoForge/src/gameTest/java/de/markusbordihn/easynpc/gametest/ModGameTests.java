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
import de.markusbordihn.easynpc.compat.CompatConstants;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.gametest.framework.TestEnvironmentDefinition;
import net.minecraft.resources.Identifier;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.common.Mod;
import net.neoforged.neoforge.event.RegisterGameTestsEvent;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

/**
 * Registers the game test functions of this mod and turns each of them into a test instance.
 *
 * <p>NeoForge has no annotation based game test discovery, so every test method is registered
 * manually here and paired with the structure it should run in.
 */
@Mod(Constants.MOD_ID + "_gametest")
public final class ModGameTests {

  private static final DeferredRegister<Consumer<GameTestHelper>> TEST_FUNCTIONS =
      DeferredRegister.create(BuiltInRegistries.TEST_FUNCTION, Constants.MOD_ID);

  private static final List<TestEntry> TEST_ENTRIES = new ArrayList<>();
  private static final int DEFAULT_MAX_TICKS = 100;
  private static final int EXTENDED_MAX_TICKS = 1200;
  private static final Identifier DEFAULT_STRUCTURE = Identifier.parse("easy_npc:gametest.3x3x3");
  private static final Identifier SMOKE_STRUCTURE = Identifier.parse("easy_npc:gametest.1x1x1");
  private static final Identifier DEFAULT_ENVIRONMENT =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "default");
  private static final Identifier IDLE_NPC_LOOK_COOLDOWN_ENVIRONMENT =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "idle_npc_look_cooldown");
  private static final Identifier IDLE_NPC_NEARBY_PLAYER_ENVIRONMENT =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "idle_npc_nearby_player");
  private static final Identifier IDLE_NPC_SPECTATOR_ENVIRONMENT =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "idle_npc_spectator");
  private static final Identifier IDLE_NPC_WITHOUT_PLAYER_ENVIRONMENT =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "idle_npc_without_player");
  private static final Identifier NPC_SAVE_SCHEDULER_ENVIRONMENT =
      Identifier.fromNamespaceAndPath(Constants.MOD_ID, "npc_save_scheduler");

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
    register(
        "scale_updates_render_entity_dimensions",
        NPCDataIsolationTest::testScaleUpdatesRenderEntityDimensions,
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
        "removal_reason_is_reset_on_respawn",
        NPCEntityLifecycleTest::testRemovalReasonIsResetOnRespawn,
        DEFAULT_STRUCTURE);
    register(
        "deleted_n_p_c_is_not_restorable_for_owner",
        NPCEntityLifecycleTest::testDeletedNPCIsNotRestorableForOwner,
        DEFAULT_STRUCTURE);
    register(
        "respawn_keeps_index_entry",
        NPCEntityLifecycleTest::testRespawnKeepsIndexEntry,
        DEFAULT_STRUCTURE);
    register(
        "loaded_n_p_c_stays_persistent",
        NPCEntityLifecycleTest::testLoadedNPCStaysPersistent,
        DEFAULT_STRUCTURE);
    register(
        "non_persistent_n_p_c_is_not_stored_on_join_and_unload",
        NPCEntityLifecycleTest::testNonPersistentNPCIsNotStoredOnJoinAndUnload,
        DEFAULT_STRUCTURE);
    register(
        "non_persistent_n_p_c_is_stored_on_explicit_despawn",
        NPCEntityLifecycleTest::testNonPersistentNPCIsStoredOnExplicitDespawn,
        DEFAULT_STRUCTURE);
    register(
        "ensure_initialized_keeps_n_p_c_index",
        NPCEntityLifecycleTest::testEnsureInitializedKeepsNPCIndex,
        DEFAULT_STRUCTURE);
    register(
        "deleted_n_p_c_is_discarded_on_load",
        NPCEntityLifecycleTest::testDeletedNPCIsDiscardedOnLoad,
        DEFAULT_STRUCTURE);
    register(
        "killed_n_p_c_requires_forced_spawn",
        NPCEntityLifecycleTest::testKilledNPCRequiresForcedSpawn,
        DEFAULT_STRUCTURE);
    register(
        "loaded_n_p_c_is_not_spawned_twice",
        NPCEntityLifecycleTest::testLoadedNPCIsNotSpawnedTwice,
        DEFAULT_STRUCTURE);
    register(
        "action_groups_are_removed_on_leave",
        NPCEntityLifecycleTest::testActionGroupsAreRemovedOnLeave,
        DEFAULT_STRUCTURE);
    register(
        "easy_n_p_c_entity_types_are_recognized",
        NPCEntityLifecycleTest::testEasyNPCEntityTypesAreRecognized,
        DEFAULT_STRUCTURE);

    register(
        "backup_is_spread_over_ticks",
        BackupTest::testBackupIsSpreadOverTicks,
        DEFAULT_STRUCTURE,
        EXTENDED_MAX_TICKS);
    register(
        "backup_restores_deleted_n_p_cs",
        BackupTest::testBackupRestoresDeletedNPCs,
        DEFAULT_STRUCTURE,
        EXTENDED_MAX_TICKS);
    register("backup_matches_export", BackupTest::testBackupMatchesExport, DEFAULT_STRUCTURE);
    register(
        "unchanged_n_p_c_is_linked_into_next_day",
        BackupTest::testUnchangedNPCIsLinkedIntoNextDay,
        DEFAULT_STRUCTURE);
    register(
        "changed_n_p_c_is_saved_and_exported",
        BackupTest::testChangedNPCIsSavedAndExported,
        DEFAULT_STRUCTURE);
    register(
        "backup_restores_over_changed_n_p_c",
        BackupTest::testBackupRestoresOverChangedNPC,
        DEFAULT_STRUCTURE);
    register(
        "backup_restores_full_configuration",
        BackupTest::testBackupRestoresFullConfiguration,
        DEFAULT_STRUCTURE);

    register(
        "idle_n_p_c_skips_travel",
        IdleNPCTest::testIdleNPCSkipsTravel,
        DEFAULT_STRUCTURE,
        IDLE_NPC_WITHOUT_PLAYER_ENVIRONMENT);
    register(
        "periodic_tick_runs_travel",
        IdleNPCTest::testPeriodicTickRunsTravel,
        DEFAULT_STRUCTURE,
        IDLE_NPC_WITHOUT_PLAYER_ENVIRONMENT);
    register(
        "moving_n_p_c_runs_travel",
        IdleNPCTest::testMovingNPCRunsTravel,
        DEFAULT_STRUCTURE,
        IDLE_NPC_WITHOUT_PLAYER_ENVIRONMENT);
    register(
        "airborne_or_hurt_n_p_c_runs_travel",
        IdleNPCTest::testAirborneOrHurtNPCRunsTravel,
        DEFAULT_STRUCTURE,
        IDLE_NPC_WITHOUT_PLAYER_ENVIRONMENT);
    register(
        "nearby_player_keeps_travel",
        IdleNPCTest::testNearbyPlayerKeepsTravel,
        DEFAULT_STRUCTURE,
        IDLE_NPC_NEARBY_PLAYER_ENVIRONMENT);
    register(
        "spectator_is_ignored",
        IdleNPCTest::testSpectatorIsIgnored,
        DEFAULT_STRUCTURE,
        IDLE_NPC_SPECTATOR_ENVIRONMENT);
    register(
        "look_goal_waits_after_failed_search",
        IdleNPCTest::testLookGoalWaitsAfterFailedSearch,
        DEFAULT_STRUCTURE,
        IDLE_NPC_LOOK_COOLDOWN_ENVIRONMENT);
    register(
        "change_is_saved_after_minimum_age",
        NPCSaveSchedulerTest::testChangeIsSavedAfterMinimumAge,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);
    register(
        "unchanged_n_p_c_is_not_saved",
        NPCSaveSchedulerTest::testUnchangedNPCIsNotSaved,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);
    register(
        "batch_size_limits_saves_per_run",
        NPCSaveSchedulerTest::testBatchSizeLimitsSavesPerRun,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);
    register(
        "unload_during_pending_write_keeps_newest_data",
        NPCSaveSchedulerTest::testUnloadDuringPendingWriteKeepsNewestData,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);
    register(
        "autosave_writes_changed_n_p_c_file",
        NPCSaveSchedulerTest::testAutosaveWritesChangedNPCFile,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);
    register(
        "disabled_interval_keeps_change_until_unload",
        NPCSaveSchedulerTest::testDisabledIntervalKeepsChangeUntilUnload,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);
    register(
        "missing_index_hash_is_adopted_without_write",
        NPCSaveSchedulerTest::testMissingIndexHashIsAdoptedWithoutWrite,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);
    register(
        "fired_spawn_action_is_saved",
        NPCSaveSchedulerTest::testFiredSpawnActionIsSaved,
        DEFAULT_STRUCTURE,
        NPC_SAVE_SCHEDULER_ENVIRONMENT);

    register(
        "mass_spawn_stays_responsive",
        NPCMassLifecycleTest::testMassSpawnStaysResponsive,
        DEFAULT_STRUCTURE,
        EXTENDED_MAX_TICKS);
    register(
        "mass_player_leave_stays_responsive",
        NPCMassLifecycleTest::testMassPlayerLeaveStaysResponsive,
        DEFAULT_STRUCTURE,
        EXTENDED_MAX_TICKS);
    register(
        "mass_removal_stays_responsive",
        NPCMassLifecycleTest::testMassRemovalStaysResponsive,
        DEFAULT_STRUCTURE,
        EXTENDED_MAX_TICKS);

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
        "equal_button_labels_lock_per_dialog",
        DialogButtonIdentityTest::testEqualButtonLabelsLockPerDialog,
        DEFAULT_STRUCTURE);
    register(
        "equal_button_labels_reach_execution_limit_per_dialog",
        DialogButtonIdentityTest::testEqualButtonLabelsReachExecutionLimitPerDialog,
        DEFAULT_STRUCTURE);
    register(
        "equal_actions_reach_execution_limit_per_button",
        DialogButtonIdentityTest::testEqualActionsReachExecutionLimitPerButton,
        DEFAULT_STRUCTURE);
    register(
        "bank_teller_locks_withdrawals_by_balance",
        DialogButtonIdentityTest::testBankTellerLocksWithdrawalsByBalance,
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
        "render_entity_type_selects_the_navigation",
        NavigationTypeTest::testRenderEntityTypeSelectsTheNavigation,
        DEFAULT_STRUCTURE);
    register(
        "navigation_type_survives_preset_import",
        NavigationTypeTest::testNavigationTypeSurvivesPresetImport,
        DEFAULT_STRUCTURE);
    register(
        "hover_heights_are_set_independently",
        NavigationTypeTest::testHoverHeightsAreSetIndependently,
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
        "default_presets_spawn_and_keep_running",
        DefaultPresetRoundTripTest::testDefaultPresetsSpawnAndKeepRunning,
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
        "export_keeps_identity", PresetIdentityTest::testExportKeepsIdentity, DEFAULT_STRUCTURE);
    register(
        "import_restores_deleted_n_p_c",
        PresetIdentityTest::testImportRestoresDeletedNPC,
        DEFAULT_STRUCTURE);
    register(
        "import_new_creates_new_identity",
        PresetIdentityTest::testImportNewCreatesNewIdentity,
        DEFAULT_STRUCTURE);
    register(
        "import_keeps_target_position",
        PresetIdentityTest::testImportKeepsTargetPosition,
        DEFAULT_STRUCTURE);
    register(
        "export_exposes_identity",
        PresetIdentityTest::testExportExposesIdentity,
        DEFAULT_STRUCTURE);
    register(
        "import_reports_created_entity",
        PresetIdentityTest::testImportReportsCreatedEntity,
        DEFAULT_STRUCTURE);
    register(
        "import_reports_updated_entity",
        PresetIdentityTest::testImportReportsUpdatedEntity,
        DEFAULT_STRUCTURE);
    register(
        "import_reports_replaced_entity",
        PresetIdentityTest::testImportReportsReplacedEntity,
        DEFAULT_STRUCTURE);
    register(
        "preset_item_drops_identity",
        PresetIdentityTest::testPresetItemDropsIdentity,
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
        "preset_handler_creates_preset_item",
        PresetSpawnTest::testPresetHandlerCreatesPresetItem,
        DEFAULT_STRUCTURE);

    register(
        "npc_without_stored_sounds_still_has_sounds",
        StoredDataTest::testNPCWithoutStoredSoundsStillHasSounds,
        DEFAULT_STRUCTURE);
    register(
        "villager_without_stored_sounds_still_has_sounds",
        StoredDataTest::testVillagerWithoutStoredSoundsStillHasSounds,
        DEFAULT_STRUCTURE);
    register(
        "objectives_survive_without_target_flags",
        StoredDataTest::testObjectivesSurviveWithoutTargetFlags,
        DEFAULT_STRUCTURE);
    register(
        "unchanged_npc_stores_no_boilerplate",
        StoredDataTest::testUnchangedNPCStoresNoBoilerplate,
        DEFAULT_STRUCTURE);
    register(
        "stored_npc_data_is_deterministic",
        StoredDataTest::testStoredNPCDataIsDeterministic,
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
        "unmet_conditional_dialog_falls_through_to_the_next_one",
        ConditionalDialogActionTest::testUnmetConditionalDialogFallsThroughToTheNextOne,
        DEFAULT_STRUCTURE);
    register(
        "met_conditional_dialog_blocks_later_screen_actions",
        ConditionalDialogActionTest::testMetConditionalDialogBlocksLaterScreenActions,
        DEFAULT_STRUCTURE);
    register(
        "fallback_runs_when_no_conditional_dialog_matches",
        ConditionalDialogActionTest::testFallbackRunsWhenNoConditionalDialogMatches,
        DEFAULT_STRUCTURE);
    register(
        "action_conditions_select_the_dialog",
        ConditionalDialogActionTest::testActionConditionsSelectTheDialog,
        DEFAULT_STRUCTURE);
    register(
        "action_chance_is_rolled_once",
        ConditionalDialogActionTest::testActionChanceIsRolledOnce,
        DEFAULT_STRUCTURE);
    register(
        "dialog_chance_is_rolled_once",
        ConditionalDialogActionTest::testDialogChanceIsRolledOnce,
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
    register("paused_state_is_synched", PauseTest::testPausedStateIsSynched, DEFAULT_STRUCTURE);

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

    register("owner_moves_n_p_c", MoveEasyNPCItemTest::testOwnerMovesNPC, DEFAULT_STRUCTURE);
    register(
        "creative_player_moves_n_p_c",
        MoveEasyNPCItemTest::testCreativePlayerMovesNPC,
        DEFAULT_STRUCTURE);
    register(
        "stranger_cannot_move_n_p_c",
        MoveEasyNPCItemTest::testStrangerCannotMoveNPC,
        DEFAULT_STRUCTURE);

    register(
        "owner_set_changes_owner", OwnerCommandTest::testOwnerSetChangesOwner, DEFAULT_STRUCTURE);
    register(
        "owner_remove_clears_owner",
        OwnerCommandTest::testOwnerRemoveClearsOwner,
        DEFAULT_STRUCTURE);
    register(
        "owner_set_requires_gamemaster",
        OwnerCommandTest::testOwnerSetRequiresGamemaster,
        DEFAULT_STRUCTURE);
    register(
        "owner_change_updates_owner_index",
        OwnerCommandTest::testOwnerChangeUpdatesOwnerIndex,
        DEFAULT_STRUCTURE);
    register(
        "same_owner_fires_one_event",
        OwnerCommandTest::testSameOwnerFiresOneEvent,
        DEFAULT_STRUCTURE);
    register(
        "owner_set_before_spawn_fires_one_event",
        OwnerCommandTest::testOwnerSetBeforeSpawnFiresOneEvent,
        DEFAULT_STRUCTURE);

    register(
        "assist_owner_targets_what_the_owner_attacks",
        AssistOwnerTest::testAssistOwnerTargetsWhatTheOwnerAttacks,
        DEFAULT_STRUCTURE);
    register(
        "assist_owner_requires_owner",
        AssistOwnerTest::testAssistOwnerRequiresOwner,
        DEFAULT_STRUCTURE);
    register(
        "owner_goals_ignore_allies",
        AssistOwnerTest::testOwnerGoalsIgnoreAllies,
        DEFAULT_STRUCTURE);

    register(
        "boss_bar_is_hidden_by_default",
        BossBarTest::testBossBarIsHiddenByDefault,
        DEFAULT_STRUCTURE);
    register(
        "boss_bar_is_shown_to_player_in_range",
        BossBarTest::testBossBarIsShownToPlayerInRange,
        DEFAULT_STRUCTURE);
    register(
        "boss_bar_is_hidden_from_player_out_of_range",
        BossBarTest::testBossBarIsHiddenFromPlayerOutOfRange,
        DEFAULT_STRUCTURE);
    register(
        "boss_bar_is_removed_after_death",
        BossBarTest::testBossBarIsRemovedAfterDeath,
        DEFAULT_STRUCTURE);
    register(
        "invalid_boss_bar_values_are_rejected",
        BossBarTest::testInvalidBossBarValuesAreRejected,
        DEFAULT_STRUCTURE);
    register(
        "boss_bar_settings_are_exported_with_the_preset",
        BossBarTest::testBossBarSettingsAreExportedWithThePreset,
        DEFAULT_STRUCTURE);

    if (CompatConstants.MOD_EASY_MODEL_ENTITIES_LOADED) {
      register(
          "integration_lists_example_profiles",
          EasyModelEntitiesTest::testIntegrationListsExampleProfiles,
          DEFAULT_STRUCTURE);
      register(
          "default_profile_defines_the_n_p_c",
          EasyModelEntitiesTest::testDefaultProfileDefinesTheNPC,
          DEFAULT_STRUCTURE);
      register(
          "floating_profile_flies",
          EasyModelEntitiesTest::testFloatingProfileFlies,
          DEFAULT_STRUCTURE);
      register(
          "aquatic_profile_swims",
          EasyModelEntitiesTest::testAquaticProfileSwims,
          DEFAULT_STRUCTURE);
      register(
          "profile_survives_preset_import",
          EasyModelEntitiesTest::testProfileSurvivesPresetImport,
          DEFAULT_STRUCTURE);
    }

    register(
        "interaction_action_consumes_the_click",
        InteractionResultTest::testInteractionActionConsumesTheClick,
        DEFAULT_STRUCTURE);
    register(
        "interaction_without_action_passes",
        InteractionResultTest::testInteractionWithoutActionPasses,
        DEFAULT_STRUCTURE);
    register(
        "interaction_with_tool_item_passes",
        InteractionResultTest::testInteractionWithToolItemPasses,
        DEFAULT_STRUCTURE);
    register(
        "pass_interaction_tag_contains_weapons",
        InteractionResultTest::testPassInteractionTagContainsWeapons,
        DEFAULT_STRUCTURE);
    register(
        "pass_interaction_item_without_action_passes",
        InteractionResultTest::testPassInteractionItemWithoutActionPasses,
        DEFAULT_STRUCTURE);
    register(
        "interaction_action_takes_precedence_over_pass_interaction_item",
        InteractionResultTest::testInteractionActionTakesPrecedenceOverPassInteractionItem,
        DEFAULT_STRUCTURE);

    register(
        "spears_are_in_pass_interaction_tag",
        SpearAttackTest::testSpearsAreInPassInteractionTag,
        DEFAULT_STRUCTURE);
    register(
        "attackable_n_p_c_can_be_hit_by_spear",
        SpearAttackTest::testAttackableNPCCanBeHitBySpear,
        DEFAULT_STRUCTURE);
    register(
        "invulnerable_n_p_c_cannot_be_hit_by_spear",
        SpearAttackTest::testInvulnerableNPCCannotBeHitBySpear,
        DEFAULT_STRUCTURE);

    register(
        "items_are_picked_up_into_inventory",
        InventoryTest::testItemsArePickedUpIntoInventory,
        DEFAULT_STRUCTURE);
    register(
        "nothing_is_picked_up_by_default",
        InventoryTest::testNothingIsPickedUpByDefault,
        DEFAULT_STRUCTURE);
    register(
        "full_inventory_rejects_items",
        InventoryTest::testFullInventoryRejectsItems,
        DEFAULT_STRUCTURE);
    register(
        "inventory_is_dropped_on_death_when_enabled",
        InventoryTest::testInventoryIsDroppedOnDeathWhenEnabled,
        DEFAULT_STRUCTURE);
    register(
        "inventory_is_not_dropped_on_death_by_default",
        InventoryTest::testInventoryIsNotDroppedOnDeathByDefault,
        DEFAULT_STRUCTURE);
    register(
        "inventory_is_exported_with_the_preset",
        InventoryTest::testInventoryIsExportedWithThePreset,
        DEFAULT_STRUCTURE);
    register(
        "preset_item_drops_inventory_contents",
        InventoryTest::testPresetItemDropsInventoryContents,
        DEFAULT_STRUCTURE);

    register(
        "owner_objective_without_owner_can_be_removed",
        ObjectiveApiTest::testOwnerObjectiveWithoutOwnerCanBeRemoved,
        DEFAULT_STRUCTURE);
    register(
        "follow_owner_is_added_and_removed_once",
        ObjectiveApiTest::testFollowOwnerIsAddedAndRemovedOnce,
        DEFAULT_STRUCTURE);
    register(
        "follow_nearest_player_is_added_and_removed_once",
        ObjectiveApiTest::testFollowNearestPlayerIsAddedAndRemovedOnce,
        DEFAULT_STRUCTURE);
    register(
        "objective_events_fire_once_per_change",
        ObjectiveApiTest::testObjectiveEventsFireOncePerChange,
        DEFAULT_STRUCTURE);

    register(
        "sit_on_shoulder_stores_n_p_c",
        ShoulderTest::testSitOnShoulderStoresNPC,
        DEFAULT_STRUCTURE);
    register(
        "second_n_p_c_uses_right_shoulder",
        ShoulderTest::testSecondNPCUsesRightShoulder,
        DEFAULT_STRUCTURE);
    register("dismount_respawns_n_p_c", ShoulderTest::testDismountRespawnsNPC, DEFAULT_STRUCTURE);
    register("shoulder_size_limit", ShoulderTest::testShoulderSizeLimit, DEFAULT_STRUCTURE);
    register(
        "attribute_toggles_objective",
        ShoulderTest::testAttributeTogglesObjective,
        DEFAULT_STRUCTURE);

    register(
        "dismount_keeps_n_p_c_when_u_u_i_d_taken",
        ShoulderTest::testDismountKeepsNPCWhenUUIDTaken,
        DEFAULT_STRUCTURE);
    register(
        "dismount_keeps_n_p_c_when_blocked",
        ShoulderTest::testDismountKeepsNPCWhenBlocked,
        DEFAULT_STRUCTURE);
    register(
        "dismount_falls_back_to_player_position",
        ShoulderTest::testDismountFallsBackToPlayerPosition,
        DEFAULT_STRUCTURE);
    register(
        "vanilla_shoulder_release_respawns_n_p_c",
        ShoulderTest::testVanillaShoulderReleaseRespawnsNPC,
        DEFAULT_STRUCTURE);
    register(
        "vanilla_shoulder_release_keeps_blocked_n_p_c",
        ShoulderTest::testVanillaShoulderReleaseKeepsBlockedNPC,
        DEFAULT_STRUCTURE);
    register(
        "player_death_puts_n_p_c_down",
        ShoulderTest::testPlayerDeathPutsNPCDown,
        DEFAULT_STRUCTURE);
    register(
        "blocked_n_p_c_stays_on_shoulder_after_respawn",
        ShoulderTest::testBlockedNPCStaysOnShoulderAfterRespawn,
        DEFAULT_STRUCTURE);
    register(
        "owner_dismount_blocks_remount",
        ShoulderTest::testOwnerDismountBlocksRemount,
        DEFAULT_STRUCTURE);
    register(
        "remount_cooldown_survives_reload",
        ShoulderTest::testRemountCooldownSurvivesReload,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_cooldown_from_objective",
        ShoulderTest::testShoulderCooldownFromObjective,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_goal_respects_max_distance",
        ShoulderTest::testShoulderGoalRespectsMaxDistance,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_goal_stops_without_progress",
        ShoulderTest::testShoulderGoalStopsWithoutProgress,
        DEFAULT_STRUCTURE,
        ShoulderTestHelper.NO_PROGRESS_TEST_TIMEOUT_TICKS);
    register(
        "shoulder_goal_waits_out_of_combat",
        ShoulderTest::testShoulderGoalWaitsOutOfCombat,
        DEFAULT_STRUCTURE);
    register(
        "revive_rejects_n_p_c_on_shoulder",
        ShoulderTest::testReviveRejectsNPCOnShoulder,
        DEFAULT_STRUCTURE);
    register(
        "sit_on_shoulder_rejects_unavailable_n_p_c",
        ShoulderTest::testSitOnShoulderRejectsUnavailableNPC,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_data_sent_to_clients_without_server_data",
        ShoulderTest::testShoulderDataSentToClientsWithoutServerData,
        DEFAULT_STRUCTURE);

    register(
        "light_level_command_sets_light_level",
        AttributeCommandTest::testLightLevelCommandSetsLightLevel,
        DEFAULT_STRUCTURE);
    register(
        "light_level_out_of_range_is_rejected",
        AttributeCommandTest::testLightLevelOutOfRangeIsRejected,
        DEFAULT_STRUCTURE);
    register(
        "max_health_command_sets_max_health_and_health",
        AttributeCommandTest::testMaxHealthCommandSetsMaxHealthAndHealth,
        DEFAULT_STRUCTURE);
    register(
        "max_health_out_of_range_is_rejected",
        AttributeCommandTest::testMaxHealthOutOfRangeIsRejected,
        DEFAULT_STRUCTURE);

    register(
        "scale_command_accepts_root",
        ModelRootCommandTest::testScaleCommandAcceptsRoot,
        DEFAULT_STRUCTURE);
    register(
        "rotate_command_accepts_root",
        ModelRootCommandTest::testRotateCommandAcceptsRoot,
        DEFAULT_STRUCTURE);
    register(
        "position_command_accepts_root",
        ModelRootCommandTest::testPositionCommandAcceptsRoot,
        DEFAULT_STRUCTURE);
    register(
        "scale_command_on_model_part_sets_custom_pose",
        ModelRootCommandTest::testScaleCommandOnModelPartSetsCustomPose,
        DEFAULT_STRUCTURE);

    register("name_clear_hides_name", NameCommandTest::testNameClearHidesName, DEFAULT_STRUCTURE);

    register(
        "unchanged_reload_writes_no_file",
        NPCStorageWriteTest::testUnchangedReloadWritesNoFile,
        DEFAULT_STRUCTURE);
    register(
        "unchanged_reload_with_objectives_writes_no_file",
        NPCStorageWriteTest::testUnchangedReloadWithObjectivesWritesNoFile,
        DEFAULT_STRUCTURE);
    register(
        "unchanged_reload_with_trading_writes_no_file",
        NPCStorageWriteTest::testUnchangedReloadWithTradingWritesNoFile,
        DEFAULT_STRUCTURE);
    register(
        "equipment_change_is_saved",
        NPCStorageWriteTest::testEquipmentChangeIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "custom_name_change_is_saved",
        NPCStorageWriteTest::testCustomNameChangeIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "custom_name_removal_is_saved",
        NPCStorageWriteTest::testCustomNameRemovalIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "inventory_change_is_saved",
        NPCStorageWriteTest::testInventoryChangeIsSaved,
        DEFAULT_STRUCTURE);
    register("item_pickup_is_saved", NPCStorageWriteTest::testItemPickupIsSaved, DEFAULT_STRUCTURE);
    register(
        "preset_import_is_saved", NPCStorageWriteTest::testPresetImportIsSaved, DEFAULT_STRUCTURE);
    register(
        "preset_import_with_type_change_is_saved",
        NPCStorageWriteTest::testPresetImportWithTypeChangeIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "load_on_running_n_p_c_is_saved",
        NPCStorageWriteTest::testLoadOnRunningNPCIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "killed_n_p_c_is_still_saved",
        NPCStorageWriteTest::testKilledNPCIsStillSaved,
        DEFAULT_STRUCTURE);
    register(
        "unload_with_player_is_still_saved",
        NPCStorageWriteTest::testUnloadWithPlayerIsStillSaved,
        DEFAULT_STRUCTURE);
    register(
        "dimension_change_is_still_saved",
        NPCStorageWriteTest::testDimensionChangeIsStillSaved,
        DEFAULT_STRUCTURE);
    register(
        "backup_run_skips_unchanged_n_p_c",
        NPCStorageWriteTest::testBackupRunSkipsUnchangedNPC,
        DEFAULT_STRUCTURE);
    register(
        "backup_run_exports_changed_n_p_c",
        NPCStorageWriteTest::testBackupRunExportsChangedNPC,
        DEFAULT_STRUCTURE);
    register(
        "list_command_reads_no_n_p_c_file",
        NPCStorageWriteTest::testListCommandReadsNoNPCFile,
        DEFAULT_STRUCTURE);
    register(
        "repeated_save_keeps_index_clean",
        NPCStorageWriteTest::testRepeatedSaveKeepsIndexClean,
        DEFAULT_STRUCTURE);
    register(
        "unchanged_pause_state_keeps_n_p_c_clean",
        NPCStorageWriteTest::testUnchangedPauseStateKeepsNPCClean,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_round_trip_writes_no_unchanged_file",
        NPCStorageWriteTest::testShoulderRoundTripWritesNoUnchangedFile,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_round_trips_keep_latest_changes",
        NPCStorageWriteTest::testShoulderRoundTripsKeepLatestChanges,
        DEFAULT_STRUCTURE);
    register(
        "dialog_change_is_saved", NPCStorageWriteTest::testDialogChangeIsSaved, DEFAULT_STRUCTURE);
    register(
        "dialog_removal_is_saved",
        NPCStorageWriteTest::testDialogRemovalIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "objective_change_is_saved",
        NPCStorageWriteTest::testObjectiveChangeIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "objective_removal_is_saved",
        NPCStorageWriteTest::testObjectiveRemovalIsSaved,
        DEFAULT_STRUCTURE);
    register(
        "objective_refresh_keeps_n_p_c_clean",
        NPCStorageWriteTest::testObjectiveRefreshKeepsNPCClean,
        DEFAULT_STRUCTURE);
    register(
        "hourly_flush_keeps_unchanged_n_p_c_clean",
        NPCStorageWriteTest::testHourlyFlushKeepsUnchangedNPCClean,
        DEFAULT_STRUCTURE);
    register(
        "hourly_flush_saves_unmarked_change",
        NPCStorageWriteTest::testHourlyFlushSavesUnmarkedChange,
        DEFAULT_STRUCTURE);
    register(
        "backup_run_exports_unmarked_change",
        NPCStorageWriteTest::testBackupRunExportsUnmarkedChange,
        DEFAULT_STRUCTURE);
    register(
        "file_storage_stats_command_runs",
        NPCStorageWriteTest::testFileStorageStatsCommandRuns,
        DEFAULT_STRUCTURE);
    register(
        "mass_reload_writes_no_unchanged_n_p_c_file",
        NPCStorageWriteTest::testMassReloadWritesNoUnchangedNPCFile,
        DEFAULT_STRUCTURE,
        EXTENDED_MAX_TICKS);

    register(
        "killed_n_p_c_can_be_revived",
        NPCEntityLifecycleTest::testKilledNPCCanBeRevived,
        DEFAULT_STRUCTURE);
    register(
        "revive_uses_given_position",
        NPCEntityLifecycleTest::testReviveUsesGivenPosition,
        DEFAULT_STRUCTURE);
    register(
        "revived_n_p_c_has_full_health",
        NPCEntityLifecycleTest::testRevivedNPCHasFullHealth,
        DEFAULT_STRUCTURE);
    register(
        "revive_rejects_deleted_n_p_c",
        NPCEntityLifecycleTest::testReviveRejectsDeletedNPC,
        DEFAULT_STRUCTURE);
    register(
        "revive_rejects_loaded_n_p_c",
        NPCEntityLifecycleTest::testReviveRejectsLoadedNPC,
        DEFAULT_STRUCTURE);
    register(
        "default_data_applied_on_every_creation_path",
        NPCEntityLifecycleTest::testDefaultDataAppliedOnEveryCreationPath,
        DEFAULT_STRUCTURE);

    register(
        "explicit_priority_reaches_goal_selector",
        ObjectiveApiTest::testExplicitPriorityReachesGoalSelector,
        DEFAULT_STRUCTURE);

    register(
        "melee_attack_uses_attack_speed",
        ObjectiveApiTest::testMeleeAttackUsesAttackSpeed,
        DEFAULT_STRUCTURE);

    register(
        "respawn_command_replaces_n_p_c",
        NPCLifecycleCommandTest::testRespawnCommandReplacesNPC,
        DEFAULT_STRUCTURE);
    register(
        "revive_command_revives_killed_n_p_c",
        NPCLifecycleCommandTest::testReviveCommandRevivesKilledNPC,
        DEFAULT_STRUCTURE);
    register(
        "revive_command_uses_given_position",
        NPCLifecycleCommandTest::testReviveCommandUsesGivenPosition,
        DEFAULT_STRUCTURE);
    register(
        "revive_command_requires_access",
        NPCLifecycleCommandTest::testReviveCommandRequiresAccess,
        DEFAULT_STRUCTURE);
    register(
        "revive_command_rejects_n_p_c_on_shoulder",
        NPCLifecycleCommandTest::testReviveCommandRejectsNPCOnShoulder,
        DEFAULT_STRUCTURE);

    register(
        "shoulder_objective_command_sets_options",
        ObjectiveCommandTest::testShoulderObjectiveCommandSetsOptions,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_objective_command_requires_enabled_shoulder",
        ObjectiveCommandTest::testShoulderObjectiveCommandRequiresEnabledShoulder,
        DEFAULT_STRUCTURE);
    register(
        "priority_command_reaches_goal_selector",
        ObjectiveCommandTest::testPriorityCommandReachesGoalSelector,
        DEFAULT_STRUCTURE);
    register(
        "priority_command_rejects_unknown_objective",
        ObjectiveCommandTest::testPriorityCommandRejectsUnknownObjective,
        DEFAULT_STRUCTURE);
    register(
        "repeated_follow_command_keeps_one_goal",
        ObjectiveCommandTest::testRepeatedFollowCommandKeepsOneGoal,
        DEFAULT_STRUCTURE);
    register(
        "attack_command_adds_and_removes_objective",
        ObjectiveCommandTest::testAttackCommandAddsAndRemovesObjective,
        DEFAULT_STRUCTURE);

    register(
        "sit_command_puts_n_p_c_on_shoulder",
        ShoulderCommandTest::testSitCommandPutsNPCOnShoulder,
        DEFAULT_STRUCTURE);
    register(
        "dismount_command_uses_side_and_cooldown",
        ShoulderCommandTest::testDismountCommandUsesSideAndCooldown,
        DEFAULT_STRUCTURE);
    register(
        "dismount_command_without_side_frees_both_shoulders",
        ShoulderCommandTest::testDismountCommandWithoutSideFreesBothShoulders,
        DEFAULT_STRUCTURE);
    register(
        "shoulder_command_only_controls_own_player",
        ShoulderCommandTest::testShoulderCommandOnlyControlsOwnPlayer,
        DEFAULT_STRUCTURE);
    register(
        "enable_command_toggles_objective",
        ShoulderCommandTest::testEnableCommandTogglesObjective,
        DEFAULT_STRUCTURE);
    register(
        "offset_command_travels_over_shoulder",
        ShoulderCommandTest::testOffsetCommandTravelsOverShoulder,
        DEFAULT_STRUCTURE);
  }

  public ModGameTests(IEventBus modEventBus) {
    TEST_FUNCTIONS.register(modEventBus);
    modEventBus.addListener(ModGameTests::registerGameTests);
  }

  private static void register(
      String name, Consumer<GameTestHelper> testFunction, Identifier structure) {
    register(name, testFunction, structure, DEFAULT_MAX_TICKS);
  }

  private static void register(
      String name, Consumer<GameTestHelper> testFunction, Identifier structure, int maxTicks) {
    register(name, testFunction, structure, maxTicks, DEFAULT_ENVIRONMENT);
  }

  private static void register(
      String name,
      Consumer<GameTestHelper> testFunction,
      Identifier structure,
      Identifier environment) {
    register(name, testFunction, structure, DEFAULT_MAX_TICKS, environment);
  }

  // Minecraft 1.21.5+ batches game tests by environment, so tests that must not share a batch
  // with other tests need their own environment.
  private static void register(
      String name,
      Consumer<GameTestHelper> testFunction,
      Identifier structure,
      int maxTicks,
      Identifier environment) {
    TEST_ENTRIES.add(
        new TestEntry(
            TEST_FUNCTIONS.register(name, () -> testFunction), structure, maxTicks, environment));
  }

  private static void registerGameTests(RegisterGameTestsEvent event) {
    Map<Identifier, Holder<TestEnvironmentDefinition>> environments = new HashMap<>();
    for (TestEntry testEntry : TEST_ENTRIES) {
      Holder<TestEnvironmentDefinition> environment =
          environments.computeIfAbsent(
              testEntry.environment(),
              environmentId ->
                  event.registerEnvironment(
                      environmentId, new TestEnvironmentDefinition.AllOf(List.of())));
      event.registerTest(
          testEntry.testFunction().getId(),
          new FunctionGameTestInstance(
              testEntry.testFunction().getKey(),
              new TestData<>(environment, testEntry.structure(), testEntry.maxTicks(), 0, true)));
    }
  }

  private record TestEntry(
      DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> testFunction,
      Identifier structure,
      int maxTicks,
      Identifier environment) {}
}
