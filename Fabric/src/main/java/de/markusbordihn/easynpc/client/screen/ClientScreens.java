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
import de.markusbordihn.easynpc.client.screen.configuration.equipment.EquipmentConfigurationContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.main.MainConfigurationContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.model.CustomModelConfigurationScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.model.DefaultModelConfigurationScreenWrapper;
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
import de.markusbordihn.easynpc.client.screen.configuration.trading.AdvancedTradingConfigurationContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.trading.BasicTradingConfigurationContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.trading.CustomTradingConfigurationContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.configuration.trading.NoneTradingConfigurationContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.dialog.DialogContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEditorContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEntryEditorContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.dialog.DialogButtonEditorContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.dialog.DialogEditorContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.dialog.DialogTextEditorContainerScreenWrapper;
import de.markusbordihn.easynpc.client.screen.spawner.SpawnerScreenWrapper;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import net.minecraft.client.gui.screens.MenuScreens;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ClientScreens {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ClientScreens() {}

  public static void registerScreens() {
    log.info("{} Client Screens ...", Constants.LOG_REGISTER_PREFIX);

    MenuScreens.register(
        ModMenuTypes.ABILITIES_ATTRIBUTE_CONFIGURATION_MENU,
        AbilitiesAttributeConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.ACTION_DATA_EDITOR_MENU, ActionDataEditorContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.ACTION_DATA_ENTRY_EDITOR_MENU,
        ActionDataEntryEditorContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU,
        AdvancedDialogConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU, AdvancedPoseConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU,
        AdvancedTradingConfigurationContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU,
        AttackObjectiveConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU,
        BaseAttributeConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU, BasicActionConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU, BasicDialogConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU,
        BasicObjectiveConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU,
        BasicTradingConfigurationContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU,
        ExportCustomPresetConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportCustomPresetConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_POSE_CONFIGURATION_MENU, CustomPoseConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU, CustomSkinConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU,
        CustomTradingConfigurationContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.CUSTOM_MODEL_CONFIGURATION_MENU, CustomModelConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportDefaultPresetConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_MODEL_CONFIGURATION_MENU, DefaultModelConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU, DefaultPoseConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_POSITION_CONFIGURATION_MENU,
        DefaultPositionConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_ROTATION_CONFIGURATION_MENU,
        DefaultRotationConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU, DefaultSkinConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU, DialogActionConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DIALOG_BUTTON_EDITOR_MENU, DialogButtonEditorContainerScreenWrapper::new);
    MenuScreens.register(ModMenuTypes.DIALOG_EDITOR_MENU, DialogEditorContainerScreenWrapper::new);
    MenuScreens.register(ModMenuTypes.DIALOG_MENU, DialogContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DIALOG_TEXT_EDITOR_MENU, DialogTextEditorContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU,
        DisplayAttributeConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU,
        DistanceActionConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU,
        EquipmentConfigurationContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU,
        FollowObjectiveConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.LOCAL_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportLocalPresetConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU,
        LookObjectiveConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.MAIN_CONFIGURATION_MENU, MainConfigurationContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.NONE_DIALOG_CONFIGURATION_MENU, NoneDialogConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.NONE_SKIN_CONFIGURATION_MENU, NoneSkinConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.NONE_TRADING_CONFIGURATION_MENU,
        NoneTradingConfigurationContainerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU, PlayerSkinConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.SCALING_CONFIGURATION_MENU, ScalingConfigurationScreenWrapper::new);
    MenuScreens.register(ModMenuTypes.SPAWNER_MENU, SpawnerScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.URL_SKIN_CONFIGURATION_MENU, UrlSkinConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.WORLD_EXPORT_PRESET_CONFIGURATION_MENU,
        ExportWorldPresetConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU,
        ImportWorldPresetConfigurationScreenWrapper::new);
    MenuScreens.register(
        ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU, YesNoDialogConfigurationScreenWrapper::new);
  }
}
