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
        "short_execution_limit_intervals_enforced",
        () -> ConditionEvaluationTest::testShortExecutionLimitIntervalsEnforced);
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
        "removal_reason_is_reset_on_respawn",
        () -> NPCEntityLifecycleTest::testRemovalReasonIsResetOnRespawn);
    TEST_FUNCTIONS.register(
        "deleted_n_p_c_is_not_restorable_for_owner",
        () -> NPCEntityLifecycleTest::testDeletedNPCIsNotRestorableForOwner);
    TEST_FUNCTIONS.register(
        "respawn_keeps_index_entry", () -> NPCEntityLifecycleTest::testRespawnKeepsIndexEntry);

    TEST_FUNCTIONS.register(
        "backup_is_spread_over_ticks", () -> BackupTest::testBackupIsSpreadOverTicks);
    TEST_FUNCTIONS.register(
        "backup_restores_deleted_n_p_cs", () -> BackupTest::testBackupRestoresDeletedNPCs);

    TEST_FUNCTIONS.register(
        "mass_spawn_stays_responsive", () -> NPCMassLifecycleTest::testMassSpawnStaysResponsive);
    TEST_FUNCTIONS.register(
        "mass_player_leave_stays_responsive",
        () -> NPCMassLifecycleTest::testMassPlayerLeaveStaysResponsive);
    TEST_FUNCTIONS.register(
        "mass_removal_stays_responsive", () -> NPCMassLifecycleTest::testMassRemovalStaysResponsive);

    TEST_FUNCTIONS.register(
        "spawned_n_p_c_has_home_position",
        () -> NavigationHomePositionTest::testSpawnedNPCHasHomePosition);
    TEST_FUNCTIONS.register(
        "home_position_is_synched", () -> NavigationHomePositionTest::testHomePositionIsSynched);
    TEST_FUNCTIONS.register(
        "home_position_survives_save_and_load",
        () -> NavigationHomePositionTest::testHomePositionSurvivesSaveAndLoad);

    TEST_FUNCTIONS.register(
        "every_command_argument_can_be_synchronized",
        () -> CommandArgumentTypeTest::testEveryCommandArgumentCanBeSynchronized);

    TEST_FUNCTIONS.register(
        "duplicate_button_label_is_rejected",
        () -> DialogButtonIdentityTest::testDuplicateButtonLabelIsRejected);
    TEST_FUNCTIONS.register(
        "button_identity_survives_save_and_load",
        () -> DialogButtonIdentityTest::testButtonIdentitySurvivesSaveAndLoad);
    TEST_FUNCTIONS.register(
        "edited_button_label_stays_addressable",
        () -> DialogButtonIdentityTest::testEditedButtonLabelStaysAddressable);

    TEST_FUNCTIONS.register(
        "distance_actions_trigger_per_range",
        () -> DistanceActionTest::testDistanceActionsTriggerPerRange);
    TEST_FUNCTIONS.register(
        "distance_actions_run_once_per_player",
        () -> DistanceActionTest::testDistanceActionsRunOncePerPlayer);
    TEST_FUNCTIONS.register(
        "distance_actions_trigger_again_after_leaving",
        () -> DistanceActionTest::testDistanceActionsTriggerAgainAfterLeaving);

    TEST_FUNCTIONS.register(
        "navigation_type_changes_the_navigation",
        () -> NavigationTypeTest::testNavigationTypeChangesTheNavigation);
    TEST_FUNCTIONS.register(
        "navigation_is_only_refreshed_on_change",
        () -> NavigationTypeTest::testNavigationIsOnlyRefreshedOnChange);
    TEST_FUNCTIONS.register(
        "gravity_is_restored_after_flying",
        () -> NavigationTypeTest::testGravityIsRestoredAfterFlying);
    TEST_FUNCTIONS.register(
        "hover_height_stays_in_range", () -> NavigationTypeTest::testHoverHeightStaysInRange);
    TEST_FUNCTIONS.register(
        "min_hover_height_stays_in_range",
        () -> NavigationTypeTest::testMinHoverHeightStaysInRange);
    TEST_FUNCTIONS.register(
        "render_entity_type_selects_the_navigation",
        () -> NavigationTypeTest::testRenderEntityTypeSelectsTheNavigation);
    TEST_FUNCTIONS.register(
        "navigation_type_survives_preset_import",
        () -> NavigationTypeTest::testNavigationTypeSurvivesPresetImport);

    TEST_FUNCTIONS.register(
        "bat_is_available_as_render_entity",
        () -> RenderEntityTypeTest::testBatIsAvailableAsRenderEntity);
    TEST_FUNCTIONS.register(
        "pathfinder_mob_lookup_stays_type_safe",
        () -> RenderEntityTypeTest::testPathfinderMobLookupStaysTypeSafe);
    TEST_FUNCTIONS.register(
        "render_entity_is_reused_per_entity_type",
        () -> RenderEntityTypeTest::testRenderEntityIsReusedPerEntityType);

    TEST_FUNCTIONS.register(
        "state_action_applies_every_operation",
        () -> NpcStateTest::testStateActionAppliesEveryOperation);
    TEST_FUNCTIONS.register(
        "invalid_state_action_is_ignored", () -> NpcStateTest::testInvalidStateActionIsIgnored);
    TEST_FUNCTIONS.register(
        "debug_action_still_writes_the_state",
        () -> NpcStateTest::testDebugActionStillWritesTheState);
    TEST_FUNCTIONS.register(
        "state_survives_save_and_load", () -> NpcStateTest::testStateSurvivesSaveAndLoad);
    TEST_FUNCTIONS.register(
        "state_condition_locks_dialog_button",
        () -> NpcStateTest::testStateConditionLocksDialogButton);
    TEST_FUNCTIONS.register(
        "state_is_reset_on_preset_import", () -> NpcStateTest::testStateIsResetOnPresetImport);
    TEST_FUNCTIONS.register(
        "state_change_notifies_listener", () -> NpcStateTest::testStateChangeNotifiesListener);
    TEST_FUNCTIONS.register(
        "forge_keeper_preset_uses_states", () -> NpcStateTest::testForgeKeeperPresetUsesStates);

    TEST_FUNCTIONS.register(
        "every_objective_type_creates_its_goal",
        () -> ObjectiveGoalCreationTest::testEveryObjectiveTypeCreatesItsGoal);
    TEST_FUNCTIONS.register(
        "every_objective_type_survives_ticking",
        () -> ObjectiveGoalCreationTest::testEveryObjectiveTypeSurvivesTicking);

    TEST_FUNCTIONS.register(
        "objective_values_from_preset_are_clamped",
        () -> ObjectivePresetTest::testObjectiveValuesFromPresetAreClamped);
    TEST_FUNCTIONS.register(
        "player_target_objective_is_registered_on_join",
        () -> ObjectivePresetTest::testPlayerTargetObjectiveIsRegisteredOnJoin);
    TEST_FUNCTIONS.register(
        "entity_target_objective_is_released_on_leave",
        () -> ObjectivePresetTest::testEntityTargetObjectiveIsReleasedOnLeave);

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

    TEST_FUNCTIONS.register(
        "base_preset_exists_for_every_spawn_egg",
        () -> BasePresetTest::testBasePresetExistsForEverySpawnEgg);
    TEST_FUNCTIONS.register(
        "base_presets_are_written", () -> BasePresetTest::testBasePresetsAreWritten);
    TEST_FUNCTIONS.register(
        "shipped_base_presets_are_up_to_date",
        () -> BasePresetTest::testShippedBasePresetsAreUpToDate);
    TEST_FUNCTIONS.register(
        "base_preset_is_usable_as_parent", () -> BasePresetTest::testBasePresetIsUsableAsParent);

    TEST_FUNCTIONS.register(
        "default_presets_are_valid", () -> DefaultPresetRoundTripTest::testDefaultPresetsAreValid);
    TEST_FUNCTIONS.register(
        "default_presets_survive_regeneration",
        () -> DefaultPresetRoundTripTest::testDefaultPresetsSurviveRegeneration);

    TEST_FUNCTIONS.register(
        "audience_contains_every_player_in_range",
        () -> IntervalActionAudienceTest::testAudienceContainsEveryPlayerInRange);
    TEST_FUNCTIONS.register(
        "owner_is_preferred_as_initiator",
        () -> IntervalActionAudienceTest::testOwnerIsPreferredAsInitiator);
    TEST_FUNCTIONS.register(
        "owner_condition_limits_the_event",
        () -> IntervalActionAudienceTest::testOwnerConditionLimitsTheEvent);
    TEST_FUNCTIONS.register(
        "nothing_fires_without_the_owner",
        () -> IntervalActionAudienceTest::testNothingFiresWithoutTheOwner);

    TEST_FUNCTIONS.register(
        "missing_objective_data_keeps_default_objectives",
        () -> PresetDefaultBaselineTest::testMissingObjectiveDataKeepsDefaultObjectives);
    TEST_FUNCTIONS.register(
        "empty_objective_data_removes_all_objectives",
        () -> PresetDefaultBaselineTest::testEmptyObjectiveDataRemovesAllObjectives);
    TEST_FUNCTIONS.register(
        "custom_objective_data_replaces_default_objectives",
        () -> PresetDefaultBaselineTest::testCustomObjectiveDataReplacesDefaultObjectives);

    TEST_FUNCTIONS.register(
        "preset_survives_round_trip", () -> PresetRoundTripTest::testPresetSurvivesRoundTrip);
    TEST_FUNCTIONS.register(
        "preset_survives_round_trip_for_villager",
        () -> PresetRoundTripTest::testPresetSurvivesRoundTripForVillager);
    TEST_FUNCTIONS.register(
        "configuration_survives_round_trip",
        () -> PresetRoundTripTest::testConfigurationSurvivesRoundTrip);
    TEST_FUNCTIONS.register("export_is_compact", () -> PresetRoundTripTest::testExportIsCompact);
    TEST_FUNCTIONS.register(
        "export_is_compact_for_villager",
        () -> PresetRoundTripTest::testExportIsCompactForVillager);

    TEST_FUNCTIONS.register(
        "preset_item_spawns_configured_n_p_c",
        () -> PresetSpawnTest::testPresetItemSpawnsConfiguredNPC);
    TEST_FUNCTIONS.register(
        "preset_item_spawns_configured_villager",
        () -> PresetSpawnTest::testPresetItemSpawnsConfiguredVillager);
    TEST_FUNCTIONS.register(
        "spawner_spawns_configured_n_p_c", () -> PresetSpawnTest::testSpawnerSpawnsConfiguredNPC);
    TEST_FUNCTIONS.register(
        "preset_import_sets_home", () -> PresetSpawnTest::testPresetImportSetsHome);

    TEST_FUNCTIONS.register(
        "npc_without_stored_sounds_still_has_sounds",
        () -> StoredDataTest::testNpcWithoutStoredSoundsStillHasSounds);
    TEST_FUNCTIONS.register(
        "villager_without_stored_sounds_still_has_sounds",
        () -> StoredDataTest::testVillagerWithoutStoredSoundsStillHasSounds);
    TEST_FUNCTIONS.register(
        "unchanged_sounds_are_not_stored", () -> StoredDataTest::testUnchangedSoundsAreNotStored);
    TEST_FUNCTIONS.register(
        "objectives_survive_without_target_flags",
        () -> StoredDataTest::testObjectivesSurviveWithoutTargetFlags);
    TEST_FUNCTIONS.register(
        "unchanged_npc_stores_no_boilerplate",
        () -> StoredDataTest::testUnchangedNpcStoresNoBoilerplate);
    TEST_FUNCTIONS.register(
        "stored_npc_data_is_deterministic", () -> StoredDataTest::testStoredNpcDataIsDeterministic);

    TEST_FUNCTIONS.register(
        "water_state_is_writable", () -> WaterStateAccessTest::testWaterStateIsWritable);

    TEST_FUNCTIONS.register(
        "fallback_stays_out_when_a_regular_action_fired",
        () -> FallbackActionTest::testFallbackStaysOutWhenARegularActionFired);
    TEST_FUNCTIONS.register(
        "fallback_runs_without_a_wait", () -> FallbackActionTest::testFallbackRunsWithoutAWait);
    TEST_FUNCTIONS.register(
        "fallback_runs_only_once", () -> FallbackActionTest::testFallbackRunsOnlyOnce);
    TEST_FUNCTIONS.register(
        "screen_action_without_a_player_is_skipped",
        () -> FallbackActionTest::testScreenActionWithoutAPlayerIsSkipped);

    TEST_FUNCTIONS.register(
        "immovable_n_p_c_is_not_pushed", () -> ImmovableAttributeTest::testImmovableNPCIsNotPushed);
    TEST_FUNCTIONS.register(
        "immovable_n_p_c_loses_its_movement_objectives",
        () -> ImmovableAttributeTest::testImmovableNPCLosesItsMovementObjectives);

    TEST_FUNCTIONS.register(
        "move_keeps_the_chain_running", () -> MoveActionTest::testMoveKeepsTheChainRunning);
    TEST_FUNCTIONS.register(
        "move_and_wait_parks_the_chain", () -> MoveActionTest::testMoveAndWaitParksTheChain);
    TEST_FUNCTIONS.register(
        "arrival_resumes_the_chain", () -> MoveActionTest::testArrivalResumesTheChain);
    TEST_FUNCTIONS.register(
        "timeout_resumes_the_chain", () -> MoveActionTest::testTimeoutResumesTheChain);
    TEST_FUNCTIONS.register(
        "immovable_n_p_c_does_not_block_the_chain",
        () -> MoveActionTest::testImmovableNPCDoesNotBlockTheChain);
    TEST_FUNCTIONS.register(
        "second_move_replaces_the_first_goal",
        () -> MoveActionTest::testSecondMoveReplacesTheFirstGoal);
    TEST_FUNCTIONS.register(
        "move_without_target_is_skipped", () -> MoveActionTest::testMoveWithoutTargetIsSkipped);

    TEST_FUNCTIONS.register(
        "new_n_p_c_is_fully_opaque", () -> OpacityAttributeTest::testNewNPCIsFullyOpaque);
    TEST_FUNCTIONS.register(
        "opacity_action_changes_the_attribute",
        () -> OpacityAttributeTest::testOpacityActionChangesTheAttribute);
    TEST_FUNCTIONS.register(
        "fully_transparent_opacity_survives_save_and_load",
        () -> OpacityAttributeTest::testFullyTransparentOpacitySurvivesSaveAndLoad);
    TEST_FUNCTIONS.register(
        "opacity_is_exported_with_the_preset",
        () -> OpacityAttributeTest::testOpacityIsExportedWithThePreset);
    TEST_FUNCTIONS.register(
        "opacity_is_clamped_to_the_allowed_range",
        () -> OpacityAttributeTest::testOpacityIsClampedToTheAllowedRange);

    TEST_FUNCTIONS.register(
        "pause_and_resume_of_a_single_n_p_c", () -> PauseTest::testPauseAndResumeOfASingleNPC);
    TEST_FUNCTIONS.register(
        "paused_n_p_c_skips_its_base_tick", () -> PauseTest::testPausedNPCSkipsItsBaseTick);
    TEST_FUNCTIONS.register(
        "pause_survives_save_and_load", () -> PauseTest::testPauseSurvivesSaveAndLoad);
    TEST_FUNCTIONS.register(
        "global_pause_covers_every_n_p_c", () -> PauseTest::testGlobalPauseCoversEveryNPC);
    TEST_FUNCTIONS.register(
        "global_resume_keeps_individual_pause",
        () -> PauseTest::testGlobalResumeKeepsIndividualPause);
    TEST_FUNCTIONS.register(
        "global_pause_ends_with_the_server", () -> PauseTest::testGlobalPauseEndsWithTheServer);

    TEST_FUNCTIONS.register(
        "idle_time_grows_while_the_player_stands_still",
        () -> PlayerIdleTest::testIdleTimeGrowsWhileThePlayerStandsStill);
    TEST_FUNCTIONS.register(
        "movement_resets_the_idle_time", () -> PlayerIdleTest::testMovementResetsTheIdleTime);
    TEST_FUNCTIONS.register(
        "idle_time_of_an_unknown_player_is_zero",
        () -> PlayerIdleTest::testIdleTimeOfAnUnknownPlayerIsZero);

    TEST_FUNCTIONS.register(
        "sound_action_plays_a_known_sound", () -> SoundActionTest::testSoundActionPlaysAKnownSound);
    TEST_FUNCTIONS.register(
        "sound_action_keeps_its_source_volume_and_pitch",
        () -> SoundActionTest::testSoundActionKeepsItsSourceVolumeAndPitch);
    TEST_FUNCTIONS.register(
        "sound_action_without_a_valid_sound_is_skipped",
        () -> SoundActionTest::testSoundActionWithoutAValidSoundIsSkipped);

    TEST_FUNCTIONS.register(
        "trade_actions_trigger_for_humanoid",
        () -> TradingActionTest::testTradeActionsTriggerForHumanoid);
    TEST_FUNCTIONS.register(
        "trade_actions_trigger_for_villager",
        () -> TradingActionTest::testTradeActionsTriggerForVillager);
    TEST_FUNCTIONS.register(
        "trade_actions_trigger_for_wandering_trader",
        () -> TradingActionTest::testTradeActionsTriggerForWanderingTrader);

    TEST_FUNCTIONS.register(
        "wait_delays_the_following_actions",
        () -> WaitActionTest::testWaitDelaysTheFollowingActions);
    TEST_FUNCTIONS.register(
        "remaining_wait_survives_save_and_load",
        () -> WaitActionTest::testRemainingWaitSurvivesSaveAndLoad);
    TEST_FUNCTIONS.register(
        "second_trigger_is_discarded", () -> WaitActionTest::testSecondTriggerIsDiscarded);
    TEST_FUNCTIONS.register(
        "different_events_run_in_parallel", () -> WaitActionTest::testDifferentEventsRunInParallel);
    TEST_FUNCTIONS.register(
        "interval_set_with_wait_runs_in_order",
        () -> WaitActionTest::testIntervalSetWithWaitRunsInOrder);
    TEST_FUNCTIONS.register(
        "interval_set_without_wait_picks_one_entry",
        () -> WaitActionTest::testIntervalSetWithoutWaitPicksOneEntry);
    TEST_FUNCTIONS.register(
        "screen_action_is_kept_over_the_wait",
        () -> WaitActionTest::testScreenActionIsKeptOverTheWait);
    TEST_FUNCTIONS.register(
        "fallback_runs_once_after_the_wait",
        () -> WaitActionTest::testFallbackRunsOnceAfterTheWait);
    TEST_FUNCTIONS.register(
        "preset_import_cancels_the_chain", () -> WaitActionTest::testPresetImportCancelsTheChain);
    TEST_FUNCTIONS.register(
        "death_cancels_the_chain", () -> WaitActionTest::testDeathCancelsTheChain);
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
