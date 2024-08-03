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
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.flag.FeatureFlagSet;
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
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.ABILITIES_ATTRIBUTE.getId(),
              new MenuType<>(AbilitiesAttributeConfigurationMenuWrapper::new, FeatureFlagSet.of()));

  public static final MenuType<ActionDataEditorMenuWrapper> ACTION_DATA_EDITOR_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          EditorType.ACTION_DATA.getId(),
          new MenuType<>(ActionDataEditorMenuWrapper::new, FeatureFlagSet.of()));

  public static final MenuType<SpawnerMenuWrapper> SPAWNER_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ModMenuType.SPAWNER.getId(),
          new MenuType<>(SpawnerMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ActionDataEntryEditorMenuWrapper> ACTION_DATA_ENTRY_EDITOR_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          EditorType.ACTION_DATA_ENTRY.getId(),
          new MenuType<>(ActionDataEntryEditorMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<AdvancedDialogConfigurationMenuWrapper>
      ADVANCED_DIALOG_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.ADVANCED_DIALOG.getId(),
              new MenuType<>(AdvancedDialogConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<AdvancedPoseConfigurationMenuWrapper>
      ADVANCED_POSE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.ADVANCED_POSE.getId(),
              new MenuType<>(AdvancedPoseConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<AdvancedTradingConfigurationMenuWrapper>
      ADVANCED_TRADING_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.ADVANCED_TRADING.getId(),
              new MenuType<>(AdvancedTradingConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<AttackObjectiveConfigurationMenuWrapper>
      ATTACK_OBJECTIVE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.ATTACK_OBJECTIVE.getId(),
              new MenuType<>(AttackObjectiveConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<BaseAttributeConfigurationMenuWrapper>
      BASE_ATTRIBUTE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.BASE_ATTRIBUTE.getId(),
              new MenuType<>(BaseAttributeConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<BasicActionConfigurationMenuWrapper>
      BASIC_ACTION_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.BASIC_ACTION.getId(),
              new MenuType<>(BasicActionConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<BasicDialogConfigurationMenuWrapper>
      BASIC_DIALOG_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.BASIC_DIALOG.getId(),
              new MenuType<>(BasicDialogConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<BasicObjectiveConfigurationMenuWrapper>
      BASIC_OBJECTIVE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.BASIC_OBJECTIVE.getId(),
              new MenuType<>(BasicObjectiveConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<BasicTradingConfigurationMenuWrapper>
      BASIC_TRADING_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.BASIC_TRADING.getId(),
              new MenuType<>(BasicTradingConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<CustomPoseConfigurationMenuWrapper> CUSTOM_POSE_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.CUSTOM_POSE.getId(),
          new MenuType<>(CustomPoseConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ExportCustomPresetConfigurationMenuWrapper>
      CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.CUSTOM_PRESET_EXPORT.getId(),
              new MenuType<>(ExportCustomPresetConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ImportCustomPresetConfigurationMenuWrapper>
      CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.CUSTOM_PRESET_IMPORT.getId(),
              new MenuType<>(ImportCustomPresetConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<CustomSkinConfigurationMenuWrapper> CUSTOM_SKIN_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.CUSTOM_SKIN.getId(),
          new MenuType<>(CustomSkinConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<CustomTradingConfigurationMenuWrapper>
      CUSTOM_TRADING_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.CUSTOM_TRADING.getId(),
              new MenuType<>(CustomTradingConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<CustomModelConfigurationMenuWrapper>
      CUSTOM_MODEL_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.CUSTOM_MODEL.getId(),
              new MenuType<>(CustomModelConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DefaultModelConfigurationMenuWrapper>
      DEFAULT_MODEL_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DEFAULT_MODEL.getId(),
              new MenuType<>(DefaultModelConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DefaultPoseConfigurationMenuWrapper>
      DEFAULT_POSE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DEFAULT_POSE.getId(),
              new MenuType<>(DefaultPoseConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DefaultPositionConfigurationMenuWrapper>
      DEFAULT_POSITION_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DEFAULT_POSITION.getId(),
              new MenuType<>(DefaultPositionConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ImportDefaultPresetConfigurationMenuWrapper>
      DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DEFAULT_PRESET_IMPORT.getId(),
              new MenuType<>(
                  ImportDefaultPresetConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DefaultRotationConfigurationMenuWrapper>
      DEFAULT_ROTATION_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DEFAULT_ROTATION.getId(),
              new MenuType<>(DefaultRotationConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DefaultSkinConfigurationMenuWrapper>
      DEFAULT_SKIN_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DEFAULT_SKIN.getId(),
              new MenuType<>(DefaultSkinConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DialogEditorMenuWrapper> DIALOG_EDITOR_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          EditorType.DIALOG.getId(),
          new MenuType<>(DialogEditorMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DialogButtonEditorMenuWrapper> DIALOG_BUTTON_EDITOR_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          EditorType.DIALOG_BUTTON.getId(),
          new MenuType<>(DialogButtonEditorMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DialogTextEditorMenuWrapper> DIALOG_TEXT_EDITOR_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          EditorType.DIALOG_TEXT.getId(),
          new MenuType<>(DialogTextEditorMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DialogMenuWrapper> DIALOG_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ModMenuType.DIALOG.getId(),
          new MenuType<>(DialogMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DialogActionConfigurationMenuWrapper>
      DIALOG_ACTION_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DIALOG_ACTION.getId(),
              new MenuType<>(DialogActionConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DisplayAttributeConfigurationMenuWrapper>
      DISPLAY_ATTRIBUTE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DISPLAY_ATTRIBUTE.getId(),
              new MenuType<>(DisplayAttributeConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<DistanceActionConfigurationMenuWrapper>
      DISTANCE_ACTION_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.DISTANCE_ACTION.getId(),
              new MenuType<>(DistanceActionConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<EquipmentConfigurationMenuWrapper> EQUIPMENT_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.EQUIPMENT.getId(),
          new MenuType<>(EquipmentConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<FollowObjectiveConfigurationMenuWrapper>
      FOLLOW_OBJECTIVE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.FOLLOW_OBJECTIVE.getId(),
              new MenuType<>(FollowObjectiveConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ImportLocalPresetConfigurationMenuWrapper>
      LOCAL_IMPORT_PRESET_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.LOCAL_PRESET_IMPORT.getId(),
              new MenuType<>(ImportLocalPresetConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<LookObjectiveConfigurationMenuWrapper>
      LOOK_OBJECTIVE_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.LOOK_OBJECTIVE.getId(),
              new MenuType<>(LookObjectiveConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<MainConfigurationMenuWrapper> MAIN_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.MAIN.getId(),
          new MenuType<>(MainConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<NoneDialogConfigurationMenuWrapper> NONE_DIALOG_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.NONE_DIALOG.getId(),
          new MenuType<>(NoneDialogConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<NoneSkinConfigurationMenuWrapper> NONE_SKIN_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.NONE_SKIN.getId(),
          new MenuType<>(NoneSkinConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<NoneTradingConfigurationMenuWrapper>
      NONE_TRADING_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.NONE_TRADING.getId(),
              new MenuType<>(NoneTradingConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<PlayerSkinConfigurationMenuWrapper> PLAYER_SKIN_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.PLAYER_SKIN.getId(),
          new MenuType<>(PlayerSkinConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ScalingConfigurationMenuWrapper> SCALING_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.SCALING.getId(),
          new MenuType<>(ScalingConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<UrlSkinConfigurationMenuWrapper> URL_SKIN_CONFIGURATION_MENU =
      Registry.register(
          BuiltInRegistries.MENU,
          ConfigurationType.URL_SKIN.getId(),
          new MenuType<>(UrlSkinConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ExportWorldPresetConfigurationMenuWrapper>
      WORLD_EXPORT_PRESET_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.WORLD_PRESET_EXPORT.getId(),
              new MenuType<>(ExportWorldPresetConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<ImportWorldPresetConfigurationMenuWrapper>
      WORLD_IMPORT_PRESET_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.WORLD_PRESET_IMPORT.getId(),
              new MenuType<>(ImportWorldPresetConfigurationMenuWrapper::new, FeatureFlagSet.of()));
  public static final MenuType<YesNoDialogConfigurationMenuWrapper>
      YES_NO_DIALOG_CONFIGURATION_MENU =
          Registry.register(
              BuiltInRegistries.MENU,
              ConfigurationType.YES_NO_DIALOG.getId(),
              new MenuType<>(YesNoDialogConfigurationMenuWrapper::new, FeatureFlagSet.of()));
}
