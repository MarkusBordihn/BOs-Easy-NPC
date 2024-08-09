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
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.inventory.MenuType;
import net.neoforged.neoforge.common.extensions.IMenuTypeExtension;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

public class ModMenuTypes {

  public static final DeferredRegister<MenuType<?>> MENU_TYPES =
      DeferredRegister.create(BuiltInRegistries.MENU, Constants.MOD_ID);
  public static final DeferredHolder<MenuType<?>, MenuType<SpawnerMenuWrapper>> SPAWNER_MENU =
      MENU_TYPES.register(
          ModMenuType.SPAWNER.getName(), () -> IMenuTypeExtension.create(SpawnerMenuWrapper::new));

  private ModMenuTypes() {}

  public static final DeferredHolder<
          MenuType<?>, MenuType<AbilitiesAttributeConfigurationMenuWrapper>>
      ABILITIES_ATTRIBUTE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.ABILITIES_ATTRIBUTE.getName(),
              () -> IMenuTypeExtension.create(AbilitiesAttributeConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<ActionDataEditorMenuWrapper>>
      ACTION_DATA_EDITOR_MENU =
          MENU_TYPES.register(
              EditorType.ACTION_DATA.getName(),
              () -> IMenuTypeExtension.create(ActionDataEditorMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<ActionDataEntryEditorMenuWrapper>>
      ACTION_DATA_ENTRY_EDITOR_MENU =
          MENU_TYPES.register(
              EditorType.ACTION_DATA_ENTRY.getName(),
              () -> IMenuTypeExtension.create(ActionDataEntryEditorMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<AdvancedDialogConfigurationMenuWrapper>>
      ADVANCED_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.ADVANCED_DIALOG.getName(),
              () -> IMenuTypeExtension.create(AdvancedDialogConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<AdvancedPoseConfigurationMenuWrapper>>
      ADVANCED_POSE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.ADVANCED_POSE.getName(),
              () -> IMenuTypeExtension.create(AdvancedPoseConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<AdvancedTradingConfigurationMenuWrapper>>
      ADVANCED_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.ADVANCED_TRADING.getName(),
              () -> IMenuTypeExtension.create(AdvancedTradingConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<AttackObjectiveConfigurationMenuWrapper>>
      ATTACK_OBJECTIVE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.ATTACK_OBJECTIVE.getName(),
              () -> IMenuTypeExtension.create(AttackObjectiveConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<BaseAttributeConfigurationMenuWrapper>>
      BASE_ATTRIBUTE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.BASE_ATTRIBUTE.getName(),
              () -> IMenuTypeExtension.create(BaseAttributeConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<BasicActionConfigurationMenuWrapper>>
      BASIC_ACTION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.BASIC_ACTION.getName(),
              () -> IMenuTypeExtension.create(BasicActionConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<BasicDialogConfigurationMenuWrapper>>
      BASIC_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.BASIC_DIALOG.getName(),
              () -> IMenuTypeExtension.create(BasicDialogConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<BasicObjectiveConfigurationMenuWrapper>>
      BASIC_OBJECTIVE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.BASIC_OBJECTIVE.getName(),
              () -> IMenuTypeExtension.create(BasicObjectiveConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<BasicTradingConfigurationMenuWrapper>>
      BASIC_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.BASIC_TRADING.getName(),
              () -> IMenuTypeExtension.create(BasicTradingConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<CustomPoseConfigurationMenuWrapper>>
      CUSTOM_POSE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.CUSTOM_POSE.getName(),
              () -> IMenuTypeExtension.create(CustomPoseConfigurationMenuWrapper::new));
  public static final DeferredHolder<
          MenuType<?>, MenuType<ExportCustomPresetConfigurationMenuWrapper>>
      CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.CUSTOM_PRESET_EXPORT.getName(),
              () -> IMenuTypeExtension.create(ExportCustomPresetConfigurationMenuWrapper::new));
  public static final DeferredHolder<
          MenuType<?>, MenuType<ImportCustomPresetConfigurationMenuWrapper>>
      CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.CUSTOM_PRESET_IMPORT.getName(),
              () -> IMenuTypeExtension.create(ImportCustomPresetConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<CustomSkinConfigurationMenuWrapper>>
      CUSTOM_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.CUSTOM_SKIN.getName(),
              () -> IMenuTypeExtension.create(CustomSkinConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<CustomTradingConfigurationMenuWrapper>>
      CUSTOM_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.CUSTOM_TRADING.getName(),
              () -> IMenuTypeExtension.create(CustomTradingConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<CustomModelConfigurationMenuWrapper>>
      CUSTOM_MODEL_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.CUSTOM_MODEL.getName(),
              () -> IMenuTypeExtension.create(CustomModelConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DefaultModelConfigurationMenuWrapper>>
      DEFAULT_MODEL_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DEFAULT_MODEL.getName(),
              () -> IMenuTypeExtension.create(DefaultModelConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DefaultPoseConfigurationMenuWrapper>>
      DEFAULT_POSE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DEFAULT_POSE.getName(),
              () -> IMenuTypeExtension.create(DefaultPoseConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DefaultPositionConfigurationMenuWrapper>>
      DEFAULT_POSITION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DEFAULT_POSITION.getName(),
              () -> IMenuTypeExtension.create(DefaultPositionConfigurationMenuWrapper::new));
  public static final DeferredHolder<
          MenuType<?>, MenuType<ImportDefaultPresetConfigurationMenuWrapper>>
      DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DEFAULT_PRESET_IMPORT.getName(),
              () -> IMenuTypeExtension.create(ImportDefaultPresetConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DefaultRotationConfigurationMenuWrapper>>
      DEFAULT_ROTATION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DEFAULT_ROTATION.getName(),
              () -> IMenuTypeExtension.create(DefaultRotationConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DefaultSkinConfigurationMenuWrapper>>
      DEFAULT_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DEFAULT_SKIN.getName(),
              () -> IMenuTypeExtension.create(DefaultSkinConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DialogEditorMenuWrapper>>
      DIALOG_EDITOR_MENU =
          MENU_TYPES.register(
              EditorType.DIALOG.getName(),
              () -> IMenuTypeExtension.create(DialogEditorMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DialogButtonEditorMenuWrapper>>
      DIALOG_BUTTON_EDITOR_MENU =
          MENU_TYPES.register(
              EditorType.DIALOG_BUTTON.getName(),
              () -> IMenuTypeExtension.create(DialogButtonEditorMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DialogTextEditorMenuWrapper>>
      DIALOG_TEXT_EDITOR_MENU =
          MENU_TYPES.register(
              EditorType.DIALOG_TEXT.getName(),
              () -> IMenuTypeExtension.create(DialogTextEditorMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DialogMenuWrapper>> DIALOG_MENU =
      MENU_TYPES.register(
          ModMenuType.DIALOG.getName(), () -> IMenuTypeExtension.create(DialogMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DialogActionConfigurationMenuWrapper>>
      DIALOG_ACTION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DIALOG_ACTION.getName(),
              () -> IMenuTypeExtension.create(DialogActionConfigurationMenuWrapper::new));
  public static final DeferredHolder<
          MenuType<?>, MenuType<DisplayAttributeConfigurationMenuWrapper>>
      DISPLAY_ATTRIBUTE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DISPLAY_ATTRIBUTE.getName(),
              () -> IMenuTypeExtension.create(DisplayAttributeConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<DistanceActionConfigurationMenuWrapper>>
      DISTANCE_ACTION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.DISTANCE_ACTION.getName(),
              () -> IMenuTypeExtension.create(DistanceActionConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<EquipmentConfigurationMenuWrapper>>
      EQUIPMENT_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.EQUIPMENT.getName(),
              () -> IMenuTypeExtension.create(EquipmentConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<FollowObjectiveConfigurationMenuWrapper>>
      FOLLOW_OBJECTIVE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.FOLLOW_OBJECTIVE.getName(),
              () -> IMenuTypeExtension.create(FollowObjectiveConfigurationMenuWrapper::new));
  public static final DeferredHolder<
          MenuType<?>, MenuType<ImportLocalPresetConfigurationMenuWrapper>>
      LOCAL_IMPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.LOCAL_PRESET_IMPORT.getName(),
              () -> IMenuTypeExtension.create(ImportLocalPresetConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<LookObjectiveConfigurationMenuWrapper>>
      LOOK_OBJECTIVE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.LOOK_OBJECTIVE.getName(),
              () -> IMenuTypeExtension.create(LookObjectiveConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<MainConfigurationMenuWrapper>>
      MAIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.MAIN.getName(),
              () -> IMenuTypeExtension.create(MainConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<NoneDialogConfigurationMenuWrapper>>
      NONE_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.NONE_DIALOG.getName(),
              () -> IMenuTypeExtension.create(NoneDialogConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<NoneSkinConfigurationMenuWrapper>>
      NONE_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.NONE_SKIN.getName(),
              () -> IMenuTypeExtension.create(NoneSkinConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<NoneTradingConfigurationMenuWrapper>>
      NONE_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.NONE_TRADING.getName(),
              () -> IMenuTypeExtension.create(NoneTradingConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<PlayerSkinConfigurationMenuWrapper>>
      PLAYER_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.PLAYER_SKIN.getName(),
              () -> IMenuTypeExtension.create(PlayerSkinConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<ScalingConfigurationMenuWrapper>>
      SCALING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.SCALING.getName(),
              () -> IMenuTypeExtension.create(ScalingConfigurationMenuWrapper::new));

  public static final DeferredHolder<MenuType<?>, MenuType<UrlSkinConfigurationMenuWrapper>>
      URL_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.URL_SKIN.getName(),
              () -> IMenuTypeExtension.create(UrlSkinConfigurationMenuWrapper::new));
  public static final DeferredHolder<
          MenuType<?>, MenuType<ExportWorldPresetConfigurationMenuWrapper>>
      WORLD_EXPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.WORLD_PRESET_EXPORT.getName(),
              () -> IMenuTypeExtension.create(ExportWorldPresetConfigurationMenuWrapper::new));
  public static final DeferredHolder<
          MenuType<?>, MenuType<ImportWorldPresetConfigurationMenuWrapper>>
      WORLD_IMPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.WORLD_PRESET_IMPORT.getName(),
              () -> IMenuTypeExtension.create(ImportWorldPresetConfigurationMenuWrapper::new));
  public static final DeferredHolder<MenuType<?>, MenuType<YesNoDialogConfigurationMenuWrapper>>
      YES_NO_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              ConfigurationType.YES_NO_DIALOG.getName(),
              () -> IMenuTypeExtension.create(YesNoDialogConfigurationMenuWrapper::new));
}
