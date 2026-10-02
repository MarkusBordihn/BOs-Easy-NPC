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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.Constants;
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
        "abilities_attribute_configuration_screen",
        () -> ConfigurationScreenTest::testAbilitiesAttributeConfigurationScreen);
    TEST_FUNCTIONS.register(
        "advanced_dialog_configuration_screen",
        () -> ConfigurationScreenTest::testAdvancedDialogConfigurationScreen);
    TEST_FUNCTIONS.register(
        "advanced_pose_configuration_screen",
        () -> ConfigurationScreenTest::testAdvancedPoseConfigurationScreen);
    TEST_FUNCTIONS.register(
        "advanced_skin_configuration_screen",
        () -> ConfigurationScreenTest::testAdvancedSkinConfigurationScreen);
    TEST_FUNCTIONS.register(
        "advanced_trading_configuration_screen",
        () -> ConfigurationScreenTest::testAdvancedTradingConfigurationScreen);
    TEST_FUNCTIONS.register(
        "attack_objective_configuration_screen",
        () -> ConfigurationScreenTest::testAttackObjectiveConfigurationScreen);
    TEST_FUNCTIONS.register(
        "target_objective_configuration_screen",
        () -> ConfigurationScreenTest::testTargetObjectiveConfigurationScreen);
    TEST_FUNCTIONS.register(
        "flee_objective_configuration_screen",
        () -> ConfigurationScreenTest::testFleeObjectiveConfigurationScreen);
    TEST_FUNCTIONS.register(
        "base_attribute_configuration_screen",
        () -> ConfigurationScreenTest::testBaseAttributeConfigurationScreen);
    TEST_FUNCTIONS.register(
        "basic_action_configuration_screen",
        () -> ConfigurationScreenTest::testBasicActionConfigurationScreen);
    TEST_FUNCTIONS.register(
        "basic_dialog_configuration_screen",
        () -> ConfigurationScreenTest::testBasicDialogConfigurationScreen);
    TEST_FUNCTIONS.register(
        "basic_objective_configuration_screen",
        () -> ConfigurationScreenTest::testBasicObjectiveConfigurationScreen);
    TEST_FUNCTIONS.register(
        "basic_pose_configuration_screen",
        () -> ConfigurationScreenTest::testBasicPoseConfigurationScreen);
    TEST_FUNCTIONS.register(
        "basic_trading_configuration_screen",
        () -> ConfigurationScreenTest::testBasicTradingConfigurationScreen);
    TEST_FUNCTIONS.register(
        "combat_attribute_configuration_screen",
        () -> ConfigurationScreenTest::testCombatAttributeConfigurationScreen);
    TEST_FUNCTIONS.register(
        "custom_pose_configuration_screen",
        () -> ConfigurationScreenTest::testCustomPoseConfigurationScreen);
    TEST_FUNCTIONS.register(
        "custom_preset_export_configuration_screen",
        () -> ConfigurationScreenTest::testCustomPresetExportConfigurationScreen);
    TEST_FUNCTIONS.register(
        "local_preset_export_configuration_screen",
        () -> ConfigurationScreenTest::testLocalPresetExportConfigurationScreen);
    TEST_FUNCTIONS.register(
        "custom_preset_import_configuration_screen",
        () -> ConfigurationScreenTest::testCustomPresetImportConfigurationScreen);
    TEST_FUNCTIONS.register(
        "custom_skin_configuration_screen",
        () -> ConfigurationScreenTest::testCustomSkinConfigurationScreen);
    TEST_FUNCTIONS.register(
        "custom_trading_configuration_screen",
        () -> ConfigurationScreenTest::testCustomTradingConfigurationScreen);
    TEST_FUNCTIONS.register(
        "cobblemon_model_configuration_screen",
        () -> ConfigurationScreenTest::testCobblemonModelConfigurationScreen);
    TEST_FUNCTIONS.register(
        "custom_model_configuration_screen",
        () -> ConfigurationScreenTest::testCustomModelConfigurationScreen);
    TEST_FUNCTIONS.register(
        "easy_model_entities_model_configuration_screen",
        () -> ConfigurationScreenTest::testEasyModelEntitiesModelConfigurationScreen);
    TEST_FUNCTIONS.register(
        "default_model_configuration_screen",
        () -> ConfigurationScreenTest::testDefaultModelConfigurationScreen);
    TEST_FUNCTIONS.register(
        "default_pose_configuration_screen",
        () -> ConfigurationScreenTest::testDefaultPoseConfigurationScreen);
    TEST_FUNCTIONS.register(
        "default_position_configuration_screen",
        () -> ConfigurationScreenTest::testDefaultPositionConfigurationScreen);
    TEST_FUNCTIONS.register(
        "default_preset_import_configuration_screen",
        () -> ConfigurationScreenTest::testDefaultPresetImportConfigurationScreen);
    TEST_FUNCTIONS.register(
        "default_rotation_configuration_screen",
        () -> ConfigurationScreenTest::testDefaultRotationConfigurationScreen);
    TEST_FUNCTIONS.register(
        "default_skin_configuration_screen",
        () -> ConfigurationScreenTest::testDefaultSkinConfigurationScreen);
    TEST_FUNCTIONS.register(
        "dialog_action_configuration_screen",
        () -> ConfigurationScreenTest::testDialogActionConfigurationScreen);
    TEST_FUNCTIONS.register(
        "display_attribute_configuration_screen",
        () -> ConfigurationScreenTest::testDisplayAttributeConfigurationScreen);
    TEST_FUNCTIONS.register(
        "distance_action_configuration_screen",
        () -> ConfigurationScreenTest::testDistanceActionConfigurationScreen);
    TEST_FUNCTIONS.register(
        "equipment_configuration_screen",
        () -> ConfigurationScreenTest::testEquipmentConfigurationScreen);
    TEST_FUNCTIONS.register(
        "follow_objective_configuration_screen",
        () -> ConfigurationScreenTest::testFollowObjectiveConfigurationScreen);
    TEST_FUNCTIONS.register(
        "local_preset_import_configuration_screen",
        () -> ConfigurationScreenTest::testLocalPresetImportConfigurationScreen);
    TEST_FUNCTIONS.register(
        "look_objective_configuration_screen",
        () -> ConfigurationScreenTest::testLookObjectiveConfigurationScreen);
    TEST_FUNCTIONS.register(
        "main_configuration_screen", () -> ConfigurationScreenTest::testMainConfigurationScreen);
    TEST_FUNCTIONS.register(
        "misc_attribute_configuration_screen",
        () -> ConfigurationScreenTest::testMiscAttributeConfigurationScreen);
    TEST_FUNCTIONS.register(
        "none_dialog_configuration_screen",
        () -> ConfigurationScreenTest::testNoneDialogConfigurationScreen);
    TEST_FUNCTIONS.register(
        "none_trading_configuration_screen",
        () -> ConfigurationScreenTest::testNoneTradingConfigurationScreen);
    TEST_FUNCTIONS.register(
        "player_skin_configuration_screen",
        () -> ConfigurationScreenTest::testPlayerSkinConfigurationScreen);
    TEST_FUNCTIONS.register(
        "scaling_configuration_screen",
        () -> ConfigurationScreenTest::testScalingConfigurationScreen);
    TEST_FUNCTIONS.register(
        "url_skin_configuration_screen",
        () -> ConfigurationScreenTest::testUrlSkinConfigurationScreen);
    TEST_FUNCTIONS.register(
        "world_preset_export_configuration_screen",
        () -> ConfigurationScreenTest::testWorldPresetExportConfigurationScreen);
    TEST_FUNCTIONS.register(
        "world_preset_import_configuration_screen",
        () -> ConfigurationScreenTest::testWorldPresetImportConfigurationScreen);
    TEST_FUNCTIONS.register(
        "yes_no_dialog_configuration_screen",
        () -> ConfigurationScreenTest::testYesNoDialogConfigurationScreen);

    TEST_FUNCTIONS.register(
        "action_data_editor_screen", () -> EditorScreenTest::testActionDataEditorScreen);
    TEST_FUNCTIONS.register(
        "action_data_entry_editor_screen", () -> EditorScreenTest::testActionDataEntryEditorScreen);
    TEST_FUNCTIONS.register(
        "condition_data_editor_screen", () -> EditorScreenTest::testConditionDataEditorScreen);
    TEST_FUNCTIONS.register(
        "condition_data_entry_editor_screen",
        () -> EditorScreenTest::testConditionDataEntryEditorScreen);
    TEST_FUNCTIONS.register("dialog_editor_screen", () -> EditorScreenTest::testDialogEditorScreen);
    TEST_FUNCTIONS.register(
        "dialog_button_editor_screen", () -> EditorScreenTest::testDialogButtonEditorScreen);
    TEST_FUNCTIONS.register(
        "dialog_options_editor_screen", () -> EditorScreenTest::testDialogOptionsEditorScreen);
    TEST_FUNCTIONS.register(
        "faction_editor_screen", () -> EditorScreenTest::testFactionEditorScreen);
    TEST_FUNCTIONS.register(
        "factions_editor_screen", () -> EditorScreenTest::testFactionsEditorScreen);
    TEST_FUNCTIONS.register(
        "dialog_text_editor_screen", () -> EditorScreenTest::testDialogTextEditorScreen);

    TEST_FUNCTIONS.register(
        "missing_configuration_type", () -> MenuManagerTest::testMissingConfigurationType);
    TEST_FUNCTIONS.register("missing_editor_type", () -> MenuManagerTest::testMissingEditorType);

    TEST_FUNCTIONS.register("mod_registered", () -> SmokeTest::testModRegistered);

    TEST_FUNCTIONS.register("max_health_change", () -> AttributeMessageTest::testMaxHealthChange);
    TEST_FUNCTIONS.register("visibility_change", () -> AttributeMessageTest::testVisibilityChange);
    TEST_FUNCTIONS.register("opacity_change", () -> AttributeMessageTest::testOpacityChange);
    TEST_FUNCTIONS.register("combat_flag_change", () -> AttributeMessageTest::testCombatFlagChange);
    TEST_FUNCTIONS.register(
        "health_regeneration_change", () -> AttributeMessageTest::testHealthRegenerationChange);
    TEST_FUNCTIONS.register(
        "movement_flag_change", () -> AttributeMessageTest::testMovementFlagChange);
    TEST_FUNCTIONS.register(
        "hover_height_change", () -> AttributeMessageTest::testHoverHeightChange);
    TEST_FUNCTIONS.register(
        "navigation_type_change", () -> AttributeMessageTest::testNavigationTypeChange);
    TEST_FUNCTIONS.register(
        "interaction_flag_change", () -> AttributeMessageTest::testInteractionFlagChange);
    TEST_FUNCTIONS.register(
        "environmental_flag_change", () -> AttributeMessageTest::testEnvironmentalFlagChange);
    TEST_FUNCTIONS.register("silent_change", () -> AttributeMessageTest::testSilentChange);
    TEST_FUNCTIONS.register("name_change", () -> AttributeMessageTest::testNameChange);

    TEST_FUNCTIONS.register("dialog_set_save", () -> DialogActionMessageTest::testDialogSetSave);
    TEST_FUNCTIONS.register("dialog_save", () -> DialogActionMessageTest::testDialogSave);
    TEST_FUNCTIONS.register(
        "harmless_dialog_button_save", () -> DialogActionMessageTest::testHarmlessDialogButtonSave);
    TEST_FUNCTIONS.register(
        "command_dialog_button_save_requires_creative",
        () -> DialogActionMessageTest::testCommandDialogButtonSaveRequiresCreative);
    TEST_FUNCTIONS.register("dialog_remove", () -> DialogActionMessageTest::testDialogRemove);
    TEST_FUNCTIONS.register(
        "dialog_button_remove", () -> DialogActionMessageTest::testDialogButtonRemove);
    TEST_FUNCTIONS.register(
        "harmless_action_event_change",
        () -> DialogActionMessageTest::testHarmlessActionEventChange);
    TEST_FUNCTIONS.register(
        "command_action_event_change_requires_creative",
        () -> DialogActionMessageTest::testCommandActionEventChangeRequiresCreative);
    TEST_FUNCTIONS.register(
        "command_trading_offer_action_requires_creative",
        () -> DialogActionMessageTest::testCommandTradingOfferActionRequiresCreative);
    TEST_FUNCTIONS.register(
        "own_execution_limit_reset", () -> DialogActionMessageTest::testOwnExecutionLimitReset);
    TEST_FUNCTIONS.register(
        "all_players_execution_limit_reset_requires_permission",
        () -> DialogActionMessageTest::testAllPlayersExecutionLimitResetRequiresPermission);
    TEST_FUNCTIONS.register(
        "execution_limit_reset_requires_access",
        () -> DialogActionMessageTest::testExecutionLimitResetRequiresAccess);

    TEST_FUNCTIONS.register(
        "trading_type_change", () -> TradingObjectiveFactionMessageTest::testTradingTypeChange);
    TEST_FUNCTIONS.register(
        "basic_trading_max_uses_change",
        () -> TradingObjectiveFactionMessageTest::testBasicTradingMaxUsesChange);
    TEST_FUNCTIONS.register(
        "basic_trading_reset_interval_change",
        () -> TradingObjectiveFactionMessageTest::testBasicTradingResetIntervalChange);
    TEST_FUNCTIONS.register(
        "advanced_trading_price_multiplier_change",
        () -> TradingObjectiveFactionMessageTest::testAdvancedTradingPriceMultiplierChange);
    TEST_FUNCTIONS.register(
        "objective_addition", () -> TradingObjectiveFactionMessageTest::testObjectiveAddition);
    TEST_FUNCTIONS.register(
        "objective_removal", () -> TradingObjectiveFactionMessageTest::testObjectiveRemoval);
    TEST_FUNCTIONS.register(
        "faction_assignment", () -> TradingObjectiveFactionMessageTest::testFactionAssignment);
    TEST_FUNCTIONS.register(
        "faction_unassignment", () -> TradingObjectiveFactionMessageTest::testFactionUnassignment);
    TEST_FUNCTIONS.register(
        "faction_creation", () -> TradingObjectiveFactionMessageTest::testFactionCreation);
    TEST_FUNCTIONS.register(
        "faction_color_change", () -> TradingObjectiveFactionMessageTest::testFactionColorChange);
    TEST_FUNCTIONS.register(
        "faction_relation_change",
        () -> TradingObjectiveFactionMessageTest::testFactionRelationChange);
    TEST_FUNCTIONS.register(
        "faction_entry_removal", () -> TradingObjectiveFactionMessageTest::testFactionEntryRemoval);

    TEST_FUNCTIONS.register(
        "player_skin_change", () -> AppearanceMessageTest::testPlayerSkinChange);
    TEST_FUNCTIONS.register(
        "remote_skin_change", () -> AppearanceMessageTest::testRemoteSkinChange);
    TEST_FUNCTIONS.register("profession_change", () -> AppearanceMessageTest::testProfessionChange);
    TEST_FUNCTIONS.register("renderer_change", () -> AppearanceMessageTest::testRendererChange);
    TEST_FUNCTIONS.register("sound_change", () -> AppearanceMessageTest::testSoundChange);
    TEST_FUNCTIONS.register("sound_reset", () -> AppearanceMessageTest::testSoundReset);
    TEST_FUNCTIONS.register("position_change", () -> AppearanceMessageTest::testPositionChange);
    TEST_FUNCTIONS.register(
        "home_position_change", () -> AppearanceMessageTest::testHomePositionChange);
    TEST_FUNCTIONS.register("n_p_c_respawn", () -> AppearanceMessageTest::testNPCRespawn);
    TEST_FUNCTIONS.register("n_p_c_removal", () -> AppearanceMessageTest::testNPCRemoval);

    TEST_FUNCTIONS.register("model_pose_change", () -> ModelMessageTest::testModelPoseChange);
    TEST_FUNCTIONS.register(
        "model_part_position_change", () -> ModelMessageTest::testModelPartPositionChange);
    TEST_FUNCTIONS.register(
        "model_root_rotation_change", () -> ModelMessageTest::testModelRootRotationChange);
    TEST_FUNCTIONS.register(
        "model_part_rotation_change", () -> ModelMessageTest::testModelPartRotationChange);
    TEST_FUNCTIONS.register(
        "model_root_scale_change", () -> ModelMessageTest::testModelRootScaleChange);
    TEST_FUNCTIONS.register(
        "model_part_scale_change", () -> ModelMessageTest::testModelPartScaleChange);
    TEST_FUNCTIONS.register(
        "model_part_visibility_change", () -> ModelMessageTest::testModelPartVisibilityChange);
    TEST_FUNCTIONS.register(
        "model_animation_behavior_change",
        () -> ModelMessageTest::testModelAnimationBehaviorChange);
    TEST_FUNCTIONS.register(
        "equipment_visibility_change", () -> ModelMessageTest::testEquipmentVisibilityChange);
    TEST_FUNCTIONS.register("pose_change", () -> ModelMessageTest::testPoseChange);
    TEST_FUNCTIONS.register("named_pose_change", () -> ModelMessageTest::testNamedPoseChange);

    TEST_FUNCTIONS.register(
        "allay_t_pose_persistence", () -> PosePersistenceTest::testAllayTPosePersistence);
    TEST_FUNCTIONS.register(
        "bogged_t_pose_persistence", () -> PosePersistenceTest::testBoggedTPosePersistence);
    TEST_FUNCTIONS.register(
        "cat_t_pose_persistence", () -> PosePersistenceTest::testCatTPosePersistence);
    TEST_FUNCTIONS.register(
        "chicken_t_pose_persistence", () -> PosePersistenceTest::testChickenTPosePersistence);
    TEST_FUNCTIONS.register(
        "creeper_t_pose_persistence", () -> PosePersistenceTest::testCreeperTPosePersistence);
    TEST_FUNCTIONS.register(
        "drowned_t_pose_persistence", () -> PosePersistenceTest::testDrownedTPosePersistence);
    TEST_FUNCTIONS.register(
        "enderman_t_pose_persistence", () -> PosePersistenceTest::testEndermanTPosePersistence);
    TEST_FUNCTIONS.register(
        "evoker_t_pose_persistence", () -> PosePersistenceTest::testEvokerTPosePersistence);
    TEST_FUNCTIONS.register(
        "fox_t_pose_persistence", () -> PosePersistenceTest::testFoxTPosePersistence);
    TEST_FUNCTIONS.register(
        "ghast_t_pose_persistence", () -> PosePersistenceTest::testGhastTPosePersistence);
    TEST_FUNCTIONS.register(
        "horse_t_pose_persistence", () -> PosePersistenceTest::testHorseTPosePersistence);
    TEST_FUNCTIONS.register(
        "horse_skeleton_t_pose_persistence",
        () -> PosePersistenceTest::testHorseSkeletonTPosePersistence);
    TEST_FUNCTIONS.register(
        "horse_zombie_t_pose_persistence",
        () -> PosePersistenceTest::testHorseZombieTPosePersistence);
    TEST_FUNCTIONS.register(
        "humanoid_t_pose_persistence", () -> PosePersistenceTest::testHumanoidTPosePersistence);
    TEST_FUNCTIONS.register(
        "humanoid_slim_t_pose_persistence",
        () -> PosePersistenceTest::testHumanoidSlimTPosePersistence);
    TEST_FUNCTIONS.register(
        "illusioner_t_pose_persistence", () -> PosePersistenceTest::testIllusionerTPosePersistence);
    TEST_FUNCTIONS.register(
        "iron_golem_t_pose_persistence", () -> PosePersistenceTest::testIronGolemTPosePersistence);
    TEST_FUNCTIONS.register(
        "piglin_t_pose_persistence", () -> PosePersistenceTest::testPiglinTPosePersistence);
    TEST_FUNCTIONS.register(
        "piglin_brute_t_pose_persistence",
        () -> PosePersistenceTest::testPiglinBruteTPosePersistence);
    TEST_FUNCTIONS.register(
        "piglin_zombified_t_pose_persistence",
        () -> PosePersistenceTest::testPiglinZombifiedTPosePersistence);
    TEST_FUNCTIONS.register(
        "pig_t_pose_persistence", () -> PosePersistenceTest::testPigTPosePersistence);
    TEST_FUNCTIONS.register(
        "pillager_t_pose_persistence", () -> PosePersistenceTest::testPillagerTPosePersistence);
    TEST_FUNCTIONS.register(
        "skeleton_t_pose_persistence", () -> PosePersistenceTest::testSkeletonTPosePersistence);
    TEST_FUNCTIONS.register(
        "stray_t_pose_persistence", () -> PosePersistenceTest::testStrayTPosePersistence);
    TEST_FUNCTIONS.register(
        "wither_skeleton_t_pose_persistence",
        () -> PosePersistenceTest::testWitherSkeletonTPosePersistence);
    TEST_FUNCTIONS.register(
        "slime_t_pose_persistence", () -> PosePersistenceTest::testSlimeTPosePersistence);
    TEST_FUNCTIONS.register(
        "spider_t_pose_persistence", () -> PosePersistenceTest::testSpiderTPosePersistence);
    TEST_FUNCTIONS.register(
        "cave_spider_t_pose_persistence",
        () -> PosePersistenceTest::testCaveSpiderTPosePersistence);
    TEST_FUNCTIONS.register(
        "villager_t_pose_persistence", () -> PosePersistenceTest::testVillagerTPosePersistence);
    TEST_FUNCTIONS.register(
        "wandering_trader_t_pose_persistence",
        () -> PosePersistenceTest::testWanderingTraderTPosePersistence);
    TEST_FUNCTIONS.register(
        "vex_t_pose_persistence", () -> PosePersistenceTest::testVexTPosePersistence);
    TEST_FUNCTIONS.register(
        "vindicator_t_pose_persistence", () -> PosePersistenceTest::testVindicatorTPosePersistence);
    TEST_FUNCTIONS.register(
        "witch_t_pose_persistence", () -> PosePersistenceTest::testWitchTPosePersistence);
    TEST_FUNCTIONS.register(
        "wolf_t_pose_persistence", () -> PosePersistenceTest::testWolfTPosePersistence);
    TEST_FUNCTIONS.register(
        "zombie_t_pose_persistence", () -> PosePersistenceTest::testZombieTPosePersistence);
    TEST_FUNCTIONS.register(
        "zombie_husk_t_pose_persistence",
        () -> PosePersistenceTest::testZombieHuskTPosePersistence);
    TEST_FUNCTIONS.register(
        "zombie_villager_t_pose_persistence",
        () -> PosePersistenceTest::testZombieVillagerTPosePersistence);
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
