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

package de.markusbordihn.easynpc.client.screen;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.configuration.actions.BasicActionConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.actions.DialogActionConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.actions.DistanceActionConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.attribute.AbilitiesAttributeConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.attribute.BaseAttributeConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.attribute.DisplayAttributeConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.AdvancedDialogConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.BasicDialogConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.NoneDialogConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.YesNoDialogConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.equipment.EquipmentConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.main.MainConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.objective.AttackObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.objective.BasicObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.objective.FollowObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.objective.LookObjectiveConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.pose.AdvancedPoseConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.pose.CustomPoseConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.pose.DefaultPoseConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.position.DefaultPositionConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ExportCustomPresetConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ExportWorldPresetConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ImportCustomPresetConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ImportDefaultPresetConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ImportWorldPresetConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.rotation.DefaultRotationConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.scaling.ScalingConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.skin.CustomSkinConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.skin.DefaultSkinConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.skin.NoneSkinConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.skin.PlayerSkinConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.skin.UrlSkinConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.trading.AdvancedTradingConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.trading.BasicTradingConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.trading.CustomTradingConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.configuration.trading.NoneTradingConfigurationScreen;
import de.markusbordihn.easynpc.client.screen.dialog.DialogScreen;
import de.markusbordihn.easynpc.client.screen.editor.DialogButtonEditorScreen;
import de.markusbordihn.easynpc.client.screen.editor.DialogEditorScreen;
import de.markusbordihn.easynpc.client.screen.editor.DialogTextEditorScreen;
import de.markusbordihn.easynpc.client.screen.spawner.SpawnerScreenWrapper;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import net.minecraft.client.gui.screens.MenuScreens;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ClientScreens {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected ClientScreens() {}

  public static void registerScreens(final FMLClientSetupEvent event) {
    log.info("{} Client Screens ...", Constants.LOG_REGISTER_PREFIX);

    event.enqueueWork(
        () -> {
          // Dialog Screen
          MenuScreens.register(ModMenuTypes.DIALOG_MENU.get(), DialogScreen::new);

          // Dialog Editor Screen
          MenuScreens.register(ModMenuTypes.DIALOG_EDITOR_MENU.get(), DialogEditorScreen::new);

          // Dialog Button Editor Screen
          MenuScreens.register(
              ModMenuTypes.DIALOG_BUTTON_EDITOR_MENU.get(), DialogButtonEditorScreen::new);

          // Dialog Text Editor Screen
          MenuScreens.register(
              ModMenuTypes.DIALOG_TEXT_EDITOR_MENU.get(), DialogTextEditorScreen::new);

          // Configuration Screen
          MenuScreens.register(
              ModMenuTypes.MAIN_CONFIGURATION_MENU.get(), MainConfigurationScreen::new);

          // Action Configuration Screens
          MenuScreens.register(
              ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU.get(),
              BasicActionConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU.get(),
              DialogActionConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU.get(),
              DistanceActionConfigurationScreen::new);

          // Attribute Configuration Screen
          MenuScreens.register(
              ModMenuTypes.BASIC_ATTRIBUTE_CONFIGURATION_MENU.get(),
              AbilitiesAttributeConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU.get(),
              BaseAttributeConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU.get(),
              DisplayAttributeConfigurationScreen::new);

          // Dialog Configuration Screens
          MenuScreens.register(
              ModMenuTypes.NONE_DIALOG_CONFIGURATION_MENU.get(),
              NoneDialogConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU.get(),
              BasicDialogConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU.get(),
              YesNoDialogConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU.get(),
              AdvancedDialogConfigurationScreen::new);

          // Equipment Configuration Screen
          MenuScreens.register(
              ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU.get(), EquipmentConfigurationScreen::new);

          // Objectives Configuration Screen
          MenuScreens.register(
              ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU.get(),
              BasicObjectiveConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU.get(),
              FollowObjectiveConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU.get(),
              AttackObjectiveConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU.get(),
              LookObjectiveConfigurationScreen::new);

          // Pose Configuration Screen
          MenuScreens.register(
              ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU.get(),
              DefaultPoseConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU.get(),
              AdvancedPoseConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_POSE_CONFIGURATION_MENU.get(),
              CustomPoseConfigurationScreen::new);

          // Position Configuration Screen
          MenuScreens.register(
              ModMenuTypes.DEFAULT_POSITION_CONFIGURATION_MENU.get(),
              DefaultPositionConfigurationScreen::new);

          // Preset Configuration Screen
          MenuScreens.register(
              ModMenuTypes.CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU.get(),
              ExportCustomPresetConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.WORLD_EXPORT_PRESET_CONFIGURATION_MENU.get(),
              ExportWorldPresetConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU.get(),
              ImportCustomPresetConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU.get(),
              ImportDefaultPresetConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU.get(),
              ImportWorldPresetConfigurationScreen::new);

          // Rotation Configuration Screen
          MenuScreens.register(
              ModMenuTypes.DEFAULT_ROTATION_CONFIGURATION_MENU.get(),
              DefaultRotationConfigurationScreen::new);

          // Skin Configuration Screens
          MenuScreens.register(
              ModMenuTypes.NONE_SKIN_CONFIGURATION_MENU.get(), NoneSkinConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU.get(),
              CustomSkinConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU.get(),
              DefaultSkinConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU.get(),
              PlayerSkinConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.URL_SKIN_CONFIGURATION_MENU.get(), UrlSkinConfigurationScreen::new);

          // Scaling Configuration Screen
          MenuScreens.register(
              ModMenuTypes.SCALING_CONFIGURATION_MENU.get(), ScalingConfigurationScreen::new);

          // Trades Configuration Screens
          MenuScreens.register(
              ModMenuTypes.NONE_TRADING_CONFIGURATION_MENU.get(),
              NoneTradingConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU.get(),
              BasicTradingConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU.get(),
              AdvancedTradingConfigurationScreen::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU.get(),
              CustomTradingConfigurationScreen::new);

          // Spawner Screen
          MenuScreens.register(ModMenuTypes.SPAWNER_MENU.get(), SpawnerScreenWrapper::new);
        });
  }
}
