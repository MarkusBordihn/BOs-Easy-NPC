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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.configui.menu.ModMenuTypes;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.entity.ModEntityType;
import de.markusbordihn.easynpc.entity.ModNPCEntityType;
import net.minecraft.gametest.framework.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;

@SuppressWarnings("unused")
public class ConfigurationScreenTest {

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testMainConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.MAIN,
        ModMenuTypes.MAIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testAbilitiesAttributeConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.ABILITIES_ATTRIBUTE,
        ModMenuTypes.ABILITIES_ATTRIBUTE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBaseAttributeConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.BASE_ATTRIBUTE,
        ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDisplayAttributeConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.DISPLAY_ATTRIBUTE,
        ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBasicActionConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.BASIC_ACTION,
        ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDialogActionConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.DIALOG_ACTION,
        ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDistanceActionConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.DISTANCE_ACTION,
        ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBasicDialogConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.BASIC_DIALOG,
        ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testAdvancedDialogConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.ADVANCED_DIALOG,
        ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testYesNoDialogConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.YES_NO_DIALOG,
        ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBasicTradingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.BASIC_TRADING,
        ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testAdvancedTradingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.ADVANCED_TRADING,
        ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testCustomTradingConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.CUSTOM_TRADING,
        ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testEquipmentConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.EQUIPMENT,
        ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDefaultSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.DEFAULT_SKIN,
        ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testPlayerSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.PLAYER_SKIN,
        ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testCustomSkinConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.CUSTOM_SKIN,
        ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testDefaultPoseConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.DEFAULT_POSE,
        ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testAdvancedPoseConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.ADVANCED_POSE,
        ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testBasicObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.BASIC_OBJECTIVE,
        ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testAttackObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.ATTACK_OBJECTIVE,
        ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testFollowObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.FOLLOW_OBJECTIVE,
        ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU);
  }

  @GameTest(template = "easy_npc:gametest.3x3x3")
  public void testLookObjectiveConfigurationScreen(GameTestHelper helper) {
    ConfigurationScreenTestHelper.testConfigurationScreen(
        helper,
        ModEntityType.getEntityType(ModNPCEntityType.HUMANOID),
        ConfigurationType.LOOK_OBJECTIVE,
        ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU);
  }
}
