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
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEditorScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.action.ActionDataEntryEditorScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.dialog.DialogButtonEditorScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.dialog.DialogEditorScreenWrapper;
import de.markusbordihn.easynpc.client.screen.editor.dialog.DialogTextEditorScreenWrapper;
import de.markusbordihn.easynpc.client.screen.spawner.SpawnerScreenWrapper;
import de.markusbordihn.easynpc.menu.ModMenuTypes;
import net.neoforged.neoforge.client.event.RegisterMenuScreensEvent;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ClientScreens {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected ClientScreens() {}

  public static void registerScreens(RegisterMenuScreensEvent event) {
    log.info("{} Client Screens ...", Constants.LOG_REGISTER_PREFIX);
    event.register(
        ModMenuTypes.ABILITIES_ATTRIBUTE_CONFIGURATION_MENU.get(),
        AbilitiesAttributeConfigurationScreenWrapper::new);
    event.register(ModMenuTypes.ACTION_DATA_EDITOR_MENU.get(), ActionDataEditorScreenWrapper::new);
    event.register(
        ModMenuTypes.ACTION_DATA_ENTRY_EDITOR_MENU.get(), ActionDataEntryEditorScreenWrapper::new);
    event.register(
        ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU.get(),
        AdvancedDialogConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU.get(),
        AdvancedPoseConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU.get(),
        AdvancedTradingConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU.get(),
        AttackObjectiveConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU.get(),
        BaseAttributeConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU.get(),
        BasicActionConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU.get(),
        BasicDialogConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU.get(),
        BasicObjectiveConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU.get(),
        BasicTradingConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU.get(),
        ExportCustomPresetConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU.get(),
        ImportCustomPresetConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.CUSTOM_POSE_CONFIGURATION_MENU.get(),
        CustomPoseConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU.get(),
        CustomSkinConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU.get(),
        CustomTradingConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU.get(),
        ImportDefaultPresetConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU.get(),
        DefaultPoseConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DEFAULT_POSITION_CONFIGURATION_MENU.get(),
        DefaultPositionConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DEFAULT_ROTATION_CONFIGURATION_MENU.get(),
        DefaultRotationConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU.get(),
        DefaultSkinConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU.get(),
        DialogActionConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DIALOG_BUTTON_EDITOR_MENU.get(), DialogButtonEditorScreenWrapper::new);
    event.register(ModMenuTypes.DIALOG_EDITOR_MENU.get(), DialogEditorScreenWrapper::new);
    event.register(ModMenuTypes.DIALOG_MENU.get(), DialogScreenWrapper::new);
    event.register(ModMenuTypes.DIALOG_TEXT_EDITOR_MENU.get(), DialogTextEditorScreenWrapper::new);
    event.register(
        ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU.get(),
        DisplayAttributeConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU.get(),
        DistanceActionConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU.get(), EquipmentConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU.get(),
        FollowObjectiveConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.LOCAL_IMPORT_PRESET_CONFIGURATION_MENU.get(),
        ImportLocalPresetConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU.get(),
        LookObjectiveConfigurationScreenWrapper::new);
    event.register(ModMenuTypes.MAIN_CONFIGURATION_MENU.get(), MainConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.NONE_DIALOG_CONFIGURATION_MENU.get(),
        NoneDialogConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.NONE_SKIN_CONFIGURATION_MENU.get(), NoneSkinConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.NONE_TRADING_CONFIGURATION_MENU.get(),
        NoneTradingConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU.get(),
        PlayerSkinConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.SCALING_CONFIGURATION_MENU.get(), ScalingConfigurationScreenWrapper::new);
    event.register(ModMenuTypes.SPAWNER_MENU.get(), SpawnerScreenWrapper::new);
    event.register(
        ModMenuTypes.URL_SKIN_CONFIGURATION_MENU.get(), UrlSkinConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.WORLD_EXPORT_PRESET_CONFIGURATION_MENU.get(),
        ExportWorldPresetConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU.get(),
        ImportWorldPresetConfigurationScreenWrapper::new);
    event.register(
        ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU.get(),
        YesNoDialogConfigurationScreenWrapper::new);
  }
}
