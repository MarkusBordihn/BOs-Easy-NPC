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

package de.markusbordihn.easynpc.menu;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.editor.EditorType;
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.action.DialogActionConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.action.DistanceActionConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.attribute.AbilitiesAttributeConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.attribute.BaseAttributeConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.attribute.DisplayAttributeConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.dialog.AdvancedDialogConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.dialog.BasicDialogConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.dialog.NoneDialogConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.dialog.YesNoDialogConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.main.MainConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.model.CustomModelConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.model.DefaultModelConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.objective.AttackObjectiveConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.objective.BasicObjectiveConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.objective.FollowObjectiveConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.objective.LookObjectiveConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.pose.AdvancedPoseConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.pose.DefaultPoseConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.position.DefaultPositionConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.preset.ExportCustomPresetConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.preset.ExportWorldPresetConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.preset.ImportCustomPresetConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.preset.ImportDefaultPresetConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.preset.ImportLocalPresetConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.preset.ImportWorldPresetConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.rotation.DefaultRotationConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.scaling.ScalingConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.skin.CustomSkinConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.skin.DefaultSkinConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.skin.NoneSkinConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.skin.PlayerSkinConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.skin.UrlSkinConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.trading.AdvancedTradingConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.trading.CustomTradingConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.configuration.trading.NoneTradingConfigurationMenuWrapper;
import de.markusbordihn.easynpc.menu.dialog.DialogMenuWrapper;
import de.markusbordihn.easynpc.menu.editor.ActionDataEditorMenuWrapper;
import de.markusbordihn.easynpc.menu.editor.ActionDataEntryEditorMenuWrapper;
import de.markusbordihn.easynpc.menu.editor.DialogButtonEditorMenuWrapper;
import de.markusbordihn.easynpc.menu.editor.DialogEditorMenuWrapper;
import de.markusbordihn.easynpc.menu.editor.DialogTextEditorMenuWrapper;
import de.markusbordihn.easynpc.menu.spawner.SpawnerMenuWrapper;
import net.fabricmc.fabric.api.screenhandler.v1.ScreenHandlerRegistry;
import net.minecraft.world.inventory.MenuType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class ModMenuTypes {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private ModMenuTypes() {}

  public static void register() {
    log.info("{} Menu Types ...", Constants.LOG_REGISTER_PREFIX);
  }

  public static final MenuType<AbilitiesAttributeConfigurationMenuWrapper>
      ABILITIES_ATTRIBUTE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.ABILITIES_ATTRIBUTE.getId(),
              AbilitiesAttributeConfigurationMenuWrapper::new);
  public static final MenuType<ActionDataEditorMenuWrapper> ACTION_DATA_EDITOR_MENU =
      ScreenHandlerRegistry.registerSimple(
          EditorType.ACTION_DATA.getId(), ActionDataEditorMenuWrapper::new);
  public static final MenuType<ActionDataEntryEditorMenuWrapper> ACTION_DATA_ENTRY_EDITOR_MENU =
      ScreenHandlerRegistry.registerSimple(
          EditorType.ACTION_DATA_ENTRY.getId(), ActionDataEntryEditorMenuWrapper::new);
  public static final MenuType<AdvancedDialogConfigurationMenuWrapper>
      ADVANCED_DIALOG_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.ADVANCED_DIALOG.getId(),
              AdvancedDialogConfigurationMenuWrapper::new);
  public static final MenuType<AdvancedPoseConfigurationMenuWrapper>
      ADVANCED_POSE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.ADVANCED_POSE.getId(), AdvancedPoseConfigurationMenuWrapper::new);
  public static final MenuType<AdvancedTradingConfigurationMenuWrapper>
      ADVANCED_TRADING_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.ADVANCED_TRADING.getId(),
              AdvancedTradingConfigurationMenuWrapper::new);
  public static final MenuType<AttackObjectiveConfigurationMenuWrapper>
      ATTACK_OBJECTIVE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.ATTACK_OBJECTIVE.getId(),
              AttackObjectiveConfigurationMenuWrapper::new);
  public static final MenuType<BaseAttributeConfigurationMenuWrapper>
      BASE_ATTRIBUTE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.BASE_ATTRIBUTE.getId(), BaseAttributeConfigurationMenuWrapper::new);
  public static final MenuType<BasicActionConfigurationMenuWrapper>
      BASIC_ACTION_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.BASIC_ACTION.getId(), BasicActionConfigurationMenuWrapper::new);
  public static final MenuType<BasicDialogConfigurationMenuWrapper>
      BASIC_DIALOG_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.BASIC_DIALOG.getId(), BasicDialogConfigurationMenuWrapper::new);
  public static final MenuType<BasicObjectiveConfigurationMenuWrapper>
      BASIC_OBJECTIVE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.BASIC_OBJECTIVE.getId(),
              BasicObjectiveConfigurationMenuWrapper::new);
  public static final MenuType<BasicTradingConfigurationMenuWrapper>
      BASIC_TRADING_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.BASIC_TRADING.getId(), BasicTradingConfigurationMenuWrapper::new);
  public static final MenuType<CustomPoseConfigurationMenuWrapper> CUSTOM_POSE_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.CUSTOM_POSE.getId(), CustomPoseConfigurationMenuWrapper::new);
  public static final MenuType<ExportCustomPresetConfigurationMenuWrapper>
      CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.CUSTOM_PRESET_EXPORT.getId(),
              ExportCustomPresetConfigurationMenuWrapper::new);
  public static final MenuType<ImportCustomPresetConfigurationMenuWrapper>
      CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.CUSTOM_PRESET_IMPORT.getId(),
              ImportCustomPresetConfigurationMenuWrapper::new);
  public static final MenuType<CustomSkinConfigurationMenuWrapper> CUSTOM_SKIN_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.CUSTOM_SKIN.getId(), CustomSkinConfigurationMenuWrapper::new);
  public static final MenuType<CustomTradingConfigurationMenuWrapper>
      CUSTOM_TRADING_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.CUSTOM_TRADING.getId(), CustomTradingConfigurationMenuWrapper::new);
  public static final MenuType<CustomModelConfigurationMenuWrapper>
      CUSTOM_MODEL_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.CUSTOM_MODEL.getId(), CustomModelConfigurationMenuWrapper::new);
  public static final MenuType<DefaultModelConfigurationMenuWrapper>
      DEFAULT_MODEL_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DEFAULT_MODEL.getId(), DefaultModelConfigurationMenuWrapper::new);
  public static final MenuType<DefaultPoseConfigurationMenuWrapper>
      DEFAULT_POSE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DEFAULT_POSE.getId(), DefaultPoseConfigurationMenuWrapper::new);
  public static final MenuType<DefaultPositionConfigurationMenuWrapper>
      DEFAULT_POSITION_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DEFAULT_POSITION.getId(),
              DefaultPositionConfigurationMenuWrapper::new);
  public static final MenuType<ImportDefaultPresetConfigurationMenuWrapper>
      DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DEFAULT_PRESET_IMPORT.getId(),
              ImportDefaultPresetConfigurationMenuWrapper::new);
  public static final MenuType<DefaultRotationConfigurationMenuWrapper>
      DEFAULT_ROTATION_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DEFAULT_ROTATION.getId(),
              DefaultRotationConfigurationMenuWrapper::new);
  public static final MenuType<DefaultSkinConfigurationMenuWrapper>
      DEFAULT_SKIN_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DEFAULT_SKIN.getId(), DefaultSkinConfigurationMenuWrapper::new);
  public static final MenuType<DialogEditorMenuWrapper> DIALOG_EDITOR_MENU =
      ScreenHandlerRegistry.registerSimple(EditorType.DIALOG.getId(), DialogEditorMenuWrapper::new);
  public static final MenuType<DialogButtonEditorMenuWrapper> DIALOG_BUTTON_EDITOR_MENU =
      ScreenHandlerRegistry.registerSimple(
          EditorType.DIALOG_BUTTON.getId(), DialogButtonEditorMenuWrapper::new);
  public static final MenuType<DialogTextEditorMenuWrapper> DIALOG_TEXT_EDITOR_MENU =
      ScreenHandlerRegistry.registerSimple(
          EditorType.DIALOG_TEXT.getId(), DialogTextEditorMenuWrapper::new);
  public static final MenuType<DialogMenuWrapper> DIALOG_MENU =
      ScreenHandlerRegistry.registerSimple(ModMenuType.DIALOG.getId(), DialogMenuWrapper::new);
  public static final MenuType<DialogActionConfigurationMenuWrapper>
      DIALOG_ACTION_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DIALOG_ACTION.getId(), DialogActionConfigurationMenuWrapper::new);
  public static final MenuType<DisplayAttributeConfigurationMenuWrapper>
      DISPLAY_ATTRIBUTE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DISPLAY_ATTRIBUTE.getId(),
              DisplayAttributeConfigurationMenuWrapper::new);
  public static final MenuType<DistanceActionConfigurationMenuWrapper>
      DISTANCE_ACTION_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.DISTANCE_ACTION.getId(),
              DistanceActionConfigurationMenuWrapper::new);
  public static final MenuType<EquipmentConfigurationMenuWrapper> EQUIPMENT_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.EQUIPMENT.getId(), EquipmentConfigurationMenuWrapper::new);
  public static final MenuType<FollowObjectiveConfigurationMenuWrapper>
      FOLLOW_OBJECTIVE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.FOLLOW_OBJECTIVE.getId(),
              FollowObjectiveConfigurationMenuWrapper::new);
  public static final MenuType<ImportLocalPresetConfigurationMenuWrapper>
      LOCAL_IMPORT_PRESET_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.LOCAL_PRESET_IMPORT.getId(),
              ImportLocalPresetConfigurationMenuWrapper::new);
  public static final MenuType<LookObjectiveConfigurationMenuWrapper>
      LOOK_OBJECTIVE_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.LOOK_OBJECTIVE.getId(), LookObjectiveConfigurationMenuWrapper::new);
  public static final MenuType<MainConfigurationMenuWrapper> MAIN_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.MAIN.getId(), MainConfigurationMenuWrapper::new);
  public static final MenuType<NoneDialogConfigurationMenuWrapper> NONE_DIALOG_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.NONE_DIALOG.getId(), NoneDialogConfigurationMenuWrapper::new);
  public static final MenuType<NoneSkinConfigurationMenuWrapper> NONE_SKIN_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.NONE_SKIN.getId(), NoneSkinConfigurationMenuWrapper::new);
  public static final MenuType<NoneTradingConfigurationMenuWrapper>
      NONE_TRADING_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.NONE_TRADING.getId(), NoneTradingConfigurationMenuWrapper::new);
  public static final MenuType<PlayerSkinConfigurationMenuWrapper> PLAYER_SKIN_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.PLAYER_SKIN.getId(), PlayerSkinConfigurationMenuWrapper::new);
  public static final MenuType<ScalingConfigurationMenuWrapper> SCALING_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.SCALING.getId(), ScalingConfigurationMenuWrapper::new);
  public static final MenuType<SpawnerMenuWrapper> SPAWNER_MENU =
      ScreenHandlerRegistry.registerSimple(ModMenuType.SPAWNER.getId(), SpawnerMenuWrapper::new);
  public static final MenuType<UrlSkinConfigurationMenuWrapper> URL_SKIN_CONFIGURATION_MENU =
      ScreenHandlerRegistry.registerSimple(
          ConfigurationType.URL_SKIN.getId(), UrlSkinConfigurationMenuWrapper::new);
  public static final MenuType<ExportWorldPresetConfigurationMenuWrapper>
      WORLD_EXPORT_PRESET_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.WORLD_PRESET_EXPORT.getId(),
              ExportWorldPresetConfigurationMenuWrapper::new);
  public static final MenuType<ImportWorldPresetConfigurationMenuWrapper>
      WORLD_IMPORT_PRESET_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.WORLD_PRESET_IMPORT.getId(),
              ImportWorldPresetConfigurationMenuWrapper::new);
  public static final MenuType<YesNoDialogConfigurationMenuWrapper>
      YES_NO_DIALOG_CONFIGURATION_MENU =
          ScreenHandlerRegistry.registerSimple(
              ConfigurationType.YES_NO_DIALOG.getId(), YesNoDialogConfigurationMenuWrapper::new);
}
