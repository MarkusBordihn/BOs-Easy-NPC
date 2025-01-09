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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;

@SuppressWarnings("unused")
public class ConfigurationScreenTest {

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenAbilitiesAttributeConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.ABILITIES_ATTRIBUTE,
        ModMenuTypes.ABILITIES_ATTRIBUTE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenAdvancedDialogConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.ADVANCED_DIALOG,
        ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenAdvancedPoseConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.ADVANCED_POSE,
        ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenAdvancedTradingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.ADVANCED_TRADING,
        ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenAttackObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.ATTACK_OBJECTIVE,
        ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenBaseAttributeConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.BASE_ATTRIBUTE,
        ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenBasicActionConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.BASIC_ACTION,
        ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenBasicDialogConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.BASIC_DIALOG,
        ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenBasicObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.BASIC_OBJECTIVE,
        ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenBasicTradingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.BASIC_TRADING,
        ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenCustomPoseConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.CUSTOM_POSE,
        ModMenuTypes.CUSTOM_POSE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenCustomPresetExportConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.CUSTOM_PRESET_EXPORT,
        ModMenuTypes.CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenCustomPresetImportConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.CUSTOM_PRESET_IMPORT,
        ModMenuTypes.CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenCustomSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.CUSTOM_SKIN,
        ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenCustomTradingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.CUSTOM_TRADING,
        ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenCustomModelConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.CUSTOM_MODEL,
        ModMenuTypes.CUSTOM_MODEL_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDefaultModelConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DEFAULT_MODEL,
        ModMenuTypes.DEFAULT_MODEL_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDefaultPoseConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DEFAULT_POSE,
        ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDefaultPositionConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DEFAULT_POSITION,
        ModMenuTypes.DEFAULT_POSITION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDefaultPresetImportConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DEFAULT_PRESET_IMPORT,
        ModMenuTypes.DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDefaultRotationConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DEFAULT_ROTATION,
        ModMenuTypes.DEFAULT_ROTATION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDefaultSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DEFAULT_SKIN,
        ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDialogActionConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DIALOG_ACTION,
        ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDisplayAttributeConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DISPLAY_ATTRIBUTE,
        ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenDistanceActionConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.DISTANCE_ACTION,
        ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenEquipmentConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.EQUIPMENT,
        ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenFollowObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.FOLLOW_OBJECTIVE,
        ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenLocalPresetImportConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.LOCAL_PRESET_IMPORT,
        ModMenuTypes.LOCAL_IMPORT_PRESET_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenLookObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.LOOK_OBJECTIVE,
        ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenMainConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.MAIN,
        ModMenuTypes.MAIN_CONFIGURATION_MENU);
  }

  public void testOpenNoneDialogConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.NONE_DIALOG,
        ModMenuTypes.NONE_DIALOG_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenNoneSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.NONE_SKIN,
        ModMenuTypes.NONE_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenNoneTradingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.NONE_TRADING,
        ModMenuTypes.NONE_TRADING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenPlayerSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.PLAYER_SKIN,
        ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenScalingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.SCALING,
        ModMenuTypes.SCALING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenUrlSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.URL_SKIN,
        ModMenuTypes.URL_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenWorldPresetExportConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.WORLD_PRESET_EXPORT,
        ModMenuTypes.WORLD_EXPORT_PRESET_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenWorldPresetImportConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.WORLD_PRESET_IMPORT,
        ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testOpenYesNoDialogConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.HUMANOID,
        ConfigurationType.YES_NO_DIALOG,
        ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU);
  }
}
