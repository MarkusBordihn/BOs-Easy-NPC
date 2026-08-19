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
