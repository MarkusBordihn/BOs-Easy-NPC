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
import de.markusbordihn.easynpc.client.screen.configuration.actions.BasicActionConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.actions.DialogActionConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.actions.DistanceActionConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.attribute.AbilitiesAttributeConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.attribute.BaseAttributeConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.attribute.DisplayAttributeConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.AdvancedDialogConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.BasicDialogConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.NoneDialogConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.dialog.YesNoDialogConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.equipment.EquipmentConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.main.MainConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.objective.AttackObjectiveConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.objective.BasicObjectiveConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.objective.FollowObjectiveConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.objective.LookObjectiveConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.pose.AdvancedPoseConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.pose.CustomPoseConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.pose.DefaultPoseConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.position.DefaultPositionConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ExportCustomPresetConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ExportWorldPresetConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ImportCustomPresetConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ImportDefaultPresetConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ImportLocalPresetConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.preset.ImportWorldPresetConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.rotation.DefaultRotationConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.scaling.ScalingConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.skin.CustomSkinConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.skin.DefaultSkinConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.skin.NoneSkinConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.skin.PlayerSkinConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.skin.UrlSkinConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.trading.AdvancedTradingConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.trading.BasicTradingConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.trading.CustomTradingConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.trading.NoneTradingConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.dialog.DialogScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.DialogButtonEditorScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.DialogEditorScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.DialogTextEditorScreenWrapper;
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
          MenuScreens.register(
              ModMenuTypes.ABILITIES_ATTRIBUTE_CONFIGURATION_MENU.get(),
              AbilitiesAttributeConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU.get(),
              AdvancedDialogConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU.get(),
              AdvancedPoseConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU.get(),
              AdvancedTradingConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU.get(),
              AttackObjectiveConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU.get(),
              BaseAttributeConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU.get(),
              BasicActionConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU.get(),
              BasicDialogConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU.get(),
              BasicObjectiveConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU.get(),
              BasicTradingConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU.get(),
              ExportCustomPresetConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU.get(),
              ImportCustomPresetConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_POSE_CONFIGURATION_MENU.get(),
              CustomPoseConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU.get(),
              CustomSkinConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU.get(),
              CustomTradingConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU.get(),
              ImportDefaultPresetConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU.get(),
              DefaultPoseConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DEFAULT_POSITION_CONFIGURATION_MENU.get(),
              DefaultPositionConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DEFAULT_ROTATION_CONFIGURATION_MENU.get(),
              DefaultRotationConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU.get(),
              DefaultSkinConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU.get(),
              DialogActionConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DIALOG_BUTTON_EDITOR_MENU.get(), DialogButtonEditorScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DIALOG_EDITOR_MENU.get(), DialogEditorScreenWrapper::new);
          MenuScreens.register(ModMenuTypes.DIALOG_MENU.get(), DialogScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DIALOG_TEXT_EDITOR_MENU.get(), DialogTextEditorScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU.get(),
              DisplayAttributeConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU.get(),
              DistanceActionConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU.get(),
              EquipmentConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU.get(),
              FollowObjectiveConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.LOCAL_IMPORT_PRESET_CONFIGURATION_MENU.get(),
              ImportLocalPresetConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU.get(),
              LookObjectiveConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.MAIN_CONFIGURATION_MENU.get(), MainConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.NONE_DIALOG_CONFIGURATION_MENU.get(),
              NoneDialogConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.NONE_SKIN_CONFIGURATION_MENU.get(),
              NoneSkinConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.NONE_TRADING_CONFIGURATION_MENU.get(),
              NoneTradingConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU.get(),
              PlayerSkinConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.SCALING_CONFIGURATION_MENU.get(),
              ScalingConfigurationScreenWrapper::new);
          MenuScreens.register(ModMenuTypes.SPAWNER_MENU.get(), SpawnerScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.URL_SKIN_CONFIGURATION_MENU.get(),
              UrlSkinConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.WORLD_EXPORT_PRESET_CONFIGURATION_MENU.get(),
              ExportWorldPresetConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU.get(),
              ImportWorldPresetConfigurationScreenWrapper::new);
          MenuScreens.register(
              ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU.get(),
              YesNoDialogConfigurationScreenWrapper::new);
        });
  }
}
