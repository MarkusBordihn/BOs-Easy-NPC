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
  private static final Identifier DEFAULT_STRUCTURE =
      Identifier.parse("easy_npc_config_ui:gametest.3x3x3");
  private static final Identifier SMOKE_STRUCTURE =
      Identifier.parse("easy_npc_config_ui:gametest.1x1x1");

  static {
    register(
        "abilities_attribute_configuration_screen",
        ConfigurationScreenTest::testAbilitiesAttributeConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "advanced_dialog_configuration_screen",
        ConfigurationScreenTest::testAdvancedDialogConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "advanced_pose_configuration_screen",
        ConfigurationScreenTest::testAdvancedPoseConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "advanced_skin_configuration_screen",
        ConfigurationScreenTest::testAdvancedSkinConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "advanced_trading_configuration_screen",
        ConfigurationScreenTest::testAdvancedTradingConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "attack_objective_configuration_screen",
        ConfigurationScreenTest::testAttackObjectiveConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "target_objective_configuration_screen",
        ConfigurationScreenTest::testTargetObjectiveConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "flee_objective_configuration_screen",
        ConfigurationScreenTest::testFleeObjectiveConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "base_attribute_configuration_screen",
        ConfigurationScreenTest::testBaseAttributeConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "basic_action_configuration_screen",
        ConfigurationScreenTest::testBasicActionConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "basic_dialog_configuration_screen",
        ConfigurationScreenTest::testBasicDialogConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "basic_objective_configuration_screen",
        ConfigurationScreenTest::testBasicObjectiveConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "basic_pose_configuration_screen",
        ConfigurationScreenTest::testBasicPoseConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "basic_trading_configuration_screen",
        ConfigurationScreenTest::testBasicTradingConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "combat_attribute_configuration_screen",
        ConfigurationScreenTest::testCombatAttributeConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "custom_pose_configuration_screen",
        ConfigurationScreenTest::testCustomPoseConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "custom_preset_export_configuration_screen",
        ConfigurationScreenTest::testCustomPresetExportConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "local_preset_export_configuration_screen",
        ConfigurationScreenTest::testLocalPresetExportConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "custom_preset_import_configuration_screen",
        ConfigurationScreenTest::testCustomPresetImportConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "custom_skin_configuration_screen",
        ConfigurationScreenTest::testCustomSkinConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "custom_trading_configuration_screen",
        ConfigurationScreenTest::testCustomTradingConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "cobblemon_model_configuration_screen",
        ConfigurationScreenTest::testCobblemonModelConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "custom_model_configuration_screen",
        ConfigurationScreenTest::testCustomModelConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "easy_model_entities_model_configuration_screen",
        ConfigurationScreenTest::testEasyModelEntitiesModelConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "default_model_configuration_screen",
        ConfigurationScreenTest::testDefaultModelConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "default_pose_configuration_screen",
        ConfigurationScreenTest::testDefaultPoseConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "default_position_configuration_screen",
        ConfigurationScreenTest::testDefaultPositionConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "default_preset_import_configuration_screen",
        ConfigurationScreenTest::testDefaultPresetImportConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "default_rotation_configuration_screen",
        ConfigurationScreenTest::testDefaultRotationConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "default_skin_configuration_screen",
        ConfigurationScreenTest::testDefaultSkinConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "dialog_action_configuration_screen",
        ConfigurationScreenTest::testDialogActionConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "display_attribute_configuration_screen",
        ConfigurationScreenTest::testDisplayAttributeConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "distance_action_configuration_screen",
        ConfigurationScreenTest::testDistanceActionConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "equipment_configuration_screen",
        ConfigurationScreenTest::testEquipmentConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "follow_objective_configuration_screen",
        ConfigurationScreenTest::testFollowObjectiveConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "local_preset_import_configuration_screen",
        ConfigurationScreenTest::testLocalPresetImportConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "look_objective_configuration_screen",
        ConfigurationScreenTest::testLookObjectiveConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "main_configuration_screen",
        ConfigurationScreenTest::testMainConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "misc_attribute_configuration_screen",
        ConfigurationScreenTest::testMiscAttributeConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "none_dialog_configuration_screen",
        ConfigurationScreenTest::testNoneDialogConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "none_trading_configuration_screen",
        ConfigurationScreenTest::testNoneTradingConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "player_skin_configuration_screen",
        ConfigurationScreenTest::testPlayerSkinConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "scaling_configuration_screen",
        ConfigurationScreenTest::testScalingConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "url_skin_configuration_screen",
        ConfigurationScreenTest::testUrlSkinConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "world_preset_export_configuration_screen",
        ConfigurationScreenTest::testWorldPresetExportConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "world_preset_import_configuration_screen",
        ConfigurationScreenTest::testWorldPresetImportConfigurationScreen,
        DEFAULT_STRUCTURE);
    register(
        "yes_no_dialog_configuration_screen",
        ConfigurationScreenTest::testYesNoDialogConfigurationScreen,
        DEFAULT_STRUCTURE);

    register(
        "action_data_editor_screen",
        EditorScreenTest::testActionDataEditorScreen,
        DEFAULT_STRUCTURE);
    register(
        "action_data_entry_editor_screen",
        EditorScreenTest::testActionDataEntryEditorScreen,
        DEFAULT_STRUCTURE);
    register(
        "condition_data_editor_screen",
        EditorScreenTest::testConditionDataEditorScreen,
        DEFAULT_STRUCTURE);
    register(
        "condition_data_entry_editor_screen",
        EditorScreenTest::testConditionDataEntryEditorScreen,
        DEFAULT_STRUCTURE);
    register("dialog_editor_screen", EditorScreenTest::testDialogEditorScreen, DEFAULT_STRUCTURE);
    register(
        "dialog_button_editor_screen",
        EditorScreenTest::testDialogButtonEditorScreen,
        DEFAULT_STRUCTURE);
    register(
        "dialog_options_editor_screen",
        EditorScreenTest::testDialogOptionsEditorScreen,
        DEFAULT_STRUCTURE);
    register("faction_editor_screen", EditorScreenTest::testFactionEditorScreen, DEFAULT_STRUCTURE);
    register(
        "factions_editor_screen", EditorScreenTest::testFactionsEditorScreen, DEFAULT_STRUCTURE);
    register(
        "dialog_text_editor_screen",
        EditorScreenTest::testDialogTextEditorScreen,
        DEFAULT_STRUCTURE);

    register(
        "missing_configuration_type",
        MenuManagerTest::testMissingConfigurationType,
        DEFAULT_STRUCTURE);
    register("missing_editor_type", MenuManagerTest::testMissingEditorType, DEFAULT_STRUCTURE);

    register("mod_registered", SmokeTest::testModRegistered, SMOKE_STRUCTURE);

    register("max_health_change", AttributeMessageTest::testMaxHealthChange, DEFAULT_STRUCTURE);
    register("visibility_change", AttributeMessageTest::testVisibilityChange, DEFAULT_STRUCTURE);
    register("opacity_change", AttributeMessageTest::testOpacityChange, DEFAULT_STRUCTURE);
    register("combat_flag_change", AttributeMessageTest::testCombatFlagChange, DEFAULT_STRUCTURE);
    register(
        "health_regeneration_change",
        AttributeMessageTest::testHealthRegenerationChange,
        DEFAULT_STRUCTURE);
    register(
        "movement_flag_change", AttributeMessageTest::testMovementFlagChange, DEFAULT_STRUCTURE);
    register("hover_height_change", AttributeMessageTest::testHoverHeightChange, DEFAULT_STRUCTURE);
    register(
        "navigation_type_change",
        AttributeMessageTest::testNavigationTypeChange,
        DEFAULT_STRUCTURE);
    register(
        "interaction_flag_change",
        AttributeMessageTest::testInteractionFlagChange,
        DEFAULT_STRUCTURE);
    register(
        "environmental_flag_change",
        AttributeMessageTest::testEnvironmentalFlagChange,
        DEFAULT_STRUCTURE);
    register("silent_change", AttributeMessageTest::testSilentChange, DEFAULT_STRUCTURE);
    register("name_change", AttributeMessageTest::testNameChange, DEFAULT_STRUCTURE);

    register("dialog_set_save", DialogActionMessageTest::testDialogSetSave, DEFAULT_STRUCTURE);
    register("dialog_save", DialogActionMessageTest::testDialogSave, DEFAULT_STRUCTURE);
    register(
        "harmless_dialog_button_save",
        DialogActionMessageTest::testHarmlessDialogButtonSave,
        DEFAULT_STRUCTURE);
    register(
        "command_dialog_button_save_requires_creative",
        DialogActionMessageTest::testCommandDialogButtonSaveRequiresCreative,
        DEFAULT_STRUCTURE);
    register("dialog_remove", DialogActionMessageTest::testDialogRemove, DEFAULT_STRUCTURE);
    register(
        "dialog_button_remove", DialogActionMessageTest::testDialogButtonRemove, DEFAULT_STRUCTURE);
    register(
        "harmless_action_event_change",
        DialogActionMessageTest::testHarmlessActionEventChange,
        DEFAULT_STRUCTURE);
    register(
        "command_action_event_change_requires_creative",
        DialogActionMessageTest::testCommandActionEventChangeRequiresCreative,
        DEFAULT_STRUCTURE);
    register(
        "command_trading_offer_action_requires_creative",
        DialogActionMessageTest::testCommandTradingOfferActionRequiresCreative,
        DEFAULT_STRUCTURE);
    register(
        "own_execution_limit_reset",
        DialogActionMessageTest::testOwnExecutionLimitReset,
        DEFAULT_STRUCTURE);
    register(
        "all_players_execution_limit_reset_requires_permission",
        DialogActionMessageTest::testAllPlayersExecutionLimitResetRequiresPermission,
        DEFAULT_STRUCTURE);
    register(
        "execution_limit_reset_requires_access",
        DialogActionMessageTest::testExecutionLimitResetRequiresAccess,
        DEFAULT_STRUCTURE);

    register(
        "trading_type_change",
        TradingObjectiveFactionMessageTest::testTradingTypeChange,
        DEFAULT_STRUCTURE);
    register(
        "basic_trading_max_uses_change",
        TradingObjectiveFactionMessageTest::testBasicTradingMaxUsesChange,
        DEFAULT_STRUCTURE);
    register(
        "basic_trading_reset_interval_change",
        TradingObjectiveFactionMessageTest::testBasicTradingResetIntervalChange,
        DEFAULT_STRUCTURE);
    register(
        "advanced_trading_price_multiplier_change",
        TradingObjectiveFactionMessageTest::testAdvancedTradingPriceMultiplierChange,
        DEFAULT_STRUCTURE);
    register(
        "objective_addition",
        TradingObjectiveFactionMessageTest::testObjectiveAddition,
        DEFAULT_STRUCTURE);
    register(
        "objective_removal",
        TradingObjectiveFactionMessageTest::testObjectiveRemoval,
        DEFAULT_STRUCTURE);
    register(
        "faction_assignment",
        TradingObjectiveFactionMessageTest::testFactionAssignment,
        DEFAULT_STRUCTURE);
    register(
        "faction_unassignment",
        TradingObjectiveFactionMessageTest::testFactionUnassignment,
        DEFAULT_STRUCTURE);
    register(
        "faction_creation",
        TradingObjectiveFactionMessageTest::testFactionCreation,
        DEFAULT_STRUCTURE);
    register(
        "faction_color_change",
        TradingObjectiveFactionMessageTest::testFactionColorChange,
        DEFAULT_STRUCTURE);
    register(
        "faction_relation_change",
        TradingObjectiveFactionMessageTest::testFactionRelationChange,
        DEFAULT_STRUCTURE);
    register(
        "faction_entry_removal",
        TradingObjectiveFactionMessageTest::testFactionEntryRemoval,
        DEFAULT_STRUCTURE);

    register("player_skin_change", AppearanceMessageTest::testPlayerSkinChange, DEFAULT_STRUCTURE);
    register("remote_skin_change", AppearanceMessageTest::testRemoteSkinChange, DEFAULT_STRUCTURE);
    register("profession_change", AppearanceMessageTest::testProfessionChange, DEFAULT_STRUCTURE);
    register("renderer_change", AppearanceMessageTest::testRendererChange, DEFAULT_STRUCTURE);
    register("sound_change", AppearanceMessageTest::testSoundChange, DEFAULT_STRUCTURE);
    register("sound_reset", AppearanceMessageTest::testSoundReset, DEFAULT_STRUCTURE);
    register("position_change", AppearanceMessageTest::testPositionChange, DEFAULT_STRUCTURE);
    register(
        "home_position_change", AppearanceMessageTest::testHomePositionChange, DEFAULT_STRUCTURE);
    register("n_p_c_respawn", AppearanceMessageTest::testNPCRespawn, DEFAULT_STRUCTURE);
    register("n_p_c_removal", AppearanceMessageTest::testNPCRemoval, DEFAULT_STRUCTURE);

    register("model_pose_change", ModelMessageTest::testModelPoseChange, DEFAULT_STRUCTURE);
    register(
        "model_part_position_change",
        ModelMessageTest::testModelPartPositionChange,
        DEFAULT_STRUCTURE);
    register(
        "model_root_rotation_change",
        ModelMessageTest::testModelRootRotationChange,
        DEFAULT_STRUCTURE);
    register(
        "model_part_rotation_change",
        ModelMessageTest::testModelPartRotationChange,
        DEFAULT_STRUCTURE);
    register(
        "model_root_scale_change", ModelMessageTest::testModelRootScaleChange, DEFAULT_STRUCTURE);
    register(
        "model_part_scale_change", ModelMessageTest::testModelPartScaleChange, DEFAULT_STRUCTURE);
    register(
        "model_part_visibility_change",
        ModelMessageTest::testModelPartVisibilityChange,
        DEFAULT_STRUCTURE);
    register(
        "model_animation_behavior_change",
        ModelMessageTest::testModelAnimationBehaviorChange,
        DEFAULT_STRUCTURE);
    register(
        "equipment_visibility_change",
        ModelMessageTest::testEquipmentVisibilityChange,
        DEFAULT_STRUCTURE);
    register("pose_change", ModelMessageTest::testPoseChange, DEFAULT_STRUCTURE);
    register("named_pose_change", ModelMessageTest::testNamedPoseChange, DEFAULT_STRUCTURE);

    register(
        "allay_t_pose_persistence",
        PosePersistenceTest::testAllayTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "bogged_t_pose_persistence",
        PosePersistenceTest::testBoggedTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "cat_t_pose_persistence",
        PosePersistenceTest::testCatTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "chicken_t_pose_persistence",
        PosePersistenceTest::testChickenTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "creeper_t_pose_persistence",
        PosePersistenceTest::testCreeperTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "drowned_t_pose_persistence",
        PosePersistenceTest::testDrownedTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "enderman_t_pose_persistence",
        PosePersistenceTest::testEndermanTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "evoker_t_pose_persistence",
        PosePersistenceTest::testEvokerTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "fox_t_pose_persistence",
        PosePersistenceTest::testFoxTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "ghast_t_pose_persistence",
        PosePersistenceTest::testGhastTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "horse_t_pose_persistence",
        PosePersistenceTest::testHorseTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "horse_skeleton_t_pose_persistence",
        PosePersistenceTest::testHorseSkeletonTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "horse_zombie_t_pose_persistence",
        PosePersistenceTest::testHorseZombieTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "humanoid_t_pose_persistence",
        PosePersistenceTest::testHumanoidTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "humanoid_slim_t_pose_persistence",
        PosePersistenceTest::testHumanoidSlimTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "illusioner_t_pose_persistence",
        PosePersistenceTest::testIllusionerTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "iron_golem_t_pose_persistence",
        PosePersistenceTest::testIronGolemTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "piglin_t_pose_persistence",
        PosePersistenceTest::testPiglinTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "piglin_brute_t_pose_persistence",
        PosePersistenceTest::testPiglinBruteTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "piglin_zombified_t_pose_persistence",
        PosePersistenceTest::testPiglinZombifiedTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "pig_t_pose_persistence",
        PosePersistenceTest::testPigTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "pillager_t_pose_persistence",
        PosePersistenceTest::testPillagerTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "skeleton_t_pose_persistence",
        PosePersistenceTest::testSkeletonTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "stray_t_pose_persistence",
        PosePersistenceTest::testStrayTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "wither_skeleton_t_pose_persistence",
        PosePersistenceTest::testWitherSkeletonTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "slime_t_pose_persistence",
        PosePersistenceTest::testSlimeTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "spider_t_pose_persistence",
        PosePersistenceTest::testSpiderTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "cave_spider_t_pose_persistence",
        PosePersistenceTest::testCaveSpiderTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "villager_t_pose_persistence",
        PosePersistenceTest::testVillagerTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "wandering_trader_t_pose_persistence",
        PosePersistenceTest::testWanderingTraderTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "vex_t_pose_persistence",
        PosePersistenceTest::testVexTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "vindicator_t_pose_persistence",
        PosePersistenceTest::testVindicatorTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "witch_t_pose_persistence",
        PosePersistenceTest::testWitchTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "wolf_t_pose_persistence",
        PosePersistenceTest::testWolfTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "zombie_t_pose_persistence",
        PosePersistenceTest::testZombieTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "zombie_husk_t_pose_persistence",
        PosePersistenceTest::testZombieHuskTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
    register(
        "zombie_villager_t_pose_persistence",
        PosePersistenceTest::testZombieVillagerTPosePersistence,
        DEFAULT_STRUCTURE,
        PosePersistenceTestHelper.TIMEOUT_TICKS);
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
    register(name, testFunction, structure, DEFAULT_MAX_TICKS);
  }

  private static void register(
      String name, Consumer<GameTestHelper> testFunction, Identifier structure, int maxTicks) {
    TEST_ENTRIES.add(
        new TestEntry(TEST_FUNCTIONS.register(name, () -> testFunction), structure, maxTicks));
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
              new TestData<>(environment, testEntry.structure(), testEntry.maxTicks(), 0, true)));
    }
  }

  private record TestEntry(
      DeferredHolder<Consumer<GameTestHelper>, Consumer<GameTestHelper>> testFunction,
      Identifier structure,
      int maxTicks) {}
}
