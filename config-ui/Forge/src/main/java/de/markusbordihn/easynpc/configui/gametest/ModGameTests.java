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
