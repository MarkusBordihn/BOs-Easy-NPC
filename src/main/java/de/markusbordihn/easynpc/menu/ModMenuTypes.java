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
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.action.DialogActionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.action.DistanceActionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.attribute.BasicAttributeConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.AdvancedDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.NoneDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.YesNoDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.main.MainConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.objective.AttackObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.objective.BasicObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.objective.FollowObjectiveConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.AdvancedPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.DefaultPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.position.DefaultPositionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.CustomExportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.CustomImportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.DefaultImportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.WorldExportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.preset.WorldImportPresetConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.rotation.DefaultRotationConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.scaling.ScalingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.CustomSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.DefaultSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.PlayerSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.AdvancedTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.BasicTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.CustomTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.trading.NoneTradingConfigurationMenu;
import de.markusbordihn.easynpc.menu.dialog.DialogMenu;
import de.markusbordihn.easynpc.menu.editor.DialogButtonEditorMenu;
import de.markusbordihn.easynpc.menu.editor.DialogEditorMenu;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.common.extensions.IForgeMenuType;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegistryObject;

public class ModMenuTypes {

  public static final DeferredRegister<MenuType<?>> MENU_TYPES =
      DeferredRegister.create(ForgeRegistries.MENU_TYPES, Constants.MOD_ID);

  protected ModMenuTypes() {}

  // Dialog
  public static final RegistryObject<MenuType<DialogMenu>> DIALOG_MENU =
      MENU_TYPES.register("dialog_menu", () -> IForgeMenuType.create(DialogMenu::new));

  // Dialog Editor
  public static final RegistryObject<MenuType<DialogEditorMenu>> DIALOG_EDITOR_MENU =
      MENU_TYPES.register("dialog_editor_menu", () -> IForgeMenuType.create(DialogEditorMenu::new));

  // Dialog Button Editor
  public static final RegistryObject<MenuType<DialogButtonEditorMenu>> DIALOG_BUTTON_EDITOR_MENU =
      MENU_TYPES.register(
          "dialog_button_editor_menu", () -> IForgeMenuType.create(DialogButtonEditorMenu::new));

  // Attribute
  public static final RegistryObject<MenuType<BasicAttributeConfigurationMenu>>
      BASIC_ATTRIBUTE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "basic_attribute_configuration_menu",
              () -> IForgeMenuType.create(BasicAttributeConfigurationMenu::new));

  // Configuration
  public static final RegistryObject<MenuType<MainConfigurationMenu>> MAIN_CONFIGURATION_MENU =
      MENU_TYPES.register(
          "main_configuration_menu", () -> IForgeMenuType.create(MainConfigurationMenu::new));

  // Action Configuration
  public static final RegistryObject<MenuType<BasicActionConfigurationMenu>>
      BASIC_ACTION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "basic_action_configuration_menu",
              () -> IForgeMenuType.create(BasicActionConfigurationMenu::new));
  public static final RegistryObject<MenuType<DialogActionConfigurationMenu>>
      DIALOG_ACTION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "dialog_action_configuration_menu",
              () -> IForgeMenuType.create(DialogActionConfigurationMenu::new));
  public static final RegistryObject<MenuType<DistanceActionConfigurationMenu>>
      DISTANCE_ACTION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "distance_action_configuration_menu",
              () -> IForgeMenuType.create(DistanceActionConfigurationMenu::new));

  // Dialog Configuration
  public static final RegistryObject<MenuType<NoneDialogConfigurationMenu>>
      NONE_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "none_dialog_configuration_menu",
              () -> IForgeMenuType.create(NoneDialogConfigurationMenu::new));
  public static final RegistryObject<MenuType<BasicDialogConfigurationMenu>>
      BASIC_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "basic_dialog_configuration_menu",
              () -> IForgeMenuType.create(BasicDialogConfigurationMenu::new));
  public static final RegistryObject<MenuType<YesNoDialogConfigurationMenu>>
      YES_NO_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "yes_no_dialog_configuration_menu",
              () -> IForgeMenuType.create(YesNoDialogConfigurationMenu::new));

  public static final RegistryObject<MenuType<AdvancedDialogConfigurationMenu>>
      ADVANCED_DIALOG_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "advanced_dialog_configuration_menu",
              () -> IForgeMenuType.create(AdvancedDialogConfigurationMenu::new));

  // Equipment Configuration
  public static final RegistryObject<MenuType<EquipmentConfigurationMenu>>
      EQUIPMENT_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "equipment_configuration_menu",
              () -> IForgeMenuType.create(EquipmentConfigurationMenu::new));

  // Objective Configuration
  public static final RegistryObject<MenuType<BasicObjectiveConfigurationMenu>>
      BASIC_OBJECTIVE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "basic_objective_configuration_menu",
              () -> IForgeMenuType.create(BasicObjectiveConfigurationMenu::new));
  public static final RegistryObject<MenuType<FollowObjectiveConfigurationMenu>>
      FOLLOW_OBJECTIVE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "follow_objective_configuration_menu",
              () -> IForgeMenuType.create(FollowObjectiveConfigurationMenu::new));
  public static final RegistryObject<MenuType<AttackObjectiveConfigurationMenu>>
      ATTACK_OBJECTIVE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "attack_objective_configuration_menu",
              () -> IForgeMenuType.create(AttackObjectiveConfigurationMenu::new));

  // Pose Configuration
  public static final RegistryObject<MenuType<DefaultPoseConfigurationMenu>>
      DEFAULT_POSE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "default_pose_configuration_menu",
              () -> IForgeMenuType.create(DefaultPoseConfigurationMenu::new));
  public static final RegistryObject<MenuType<AdvancedPoseConfigurationMenu>>
      ADVANCED_POSE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "advanced_pose_configuration_menu",
              () -> IForgeMenuType.create(AdvancedPoseConfigurationMenu::new));
  public static final RegistryObject<MenuType<CustomPoseConfigurationMenu>>
      CUSTOM_POSE_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "custom_pose_configuration_menu",
              () -> IForgeMenuType.create(CustomPoseConfigurationMenu::new));

  // Position Configuration
  public static final RegistryObject<MenuType<DefaultPositionConfigurationMenu>>
      DEFAULT_POSITION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "default_position_configuration_menu",
              () -> IForgeMenuType.create(DefaultPositionConfigurationMenu::new));

  // Preset Configuration
  public static final RegistryObject<MenuType<CustomExportPresetConfigurationMenu>>
      CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "custom_export_preset_configuration_menu",
              () -> IForgeMenuType.create(CustomExportPresetConfigurationMenu::new));
  public static final RegistryObject<MenuType<WorldExportPresetConfigurationMenu>>
      WORLD_EXPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "world_export_preset_configuration_menu",
              () -> IForgeMenuType.create(WorldExportPresetConfigurationMenu::new));
  public static final RegistryObject<MenuType<CustomImportPresetConfigurationMenu>>
      CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "custom_import_preset_configuration_menu",
              () -> IForgeMenuType.create(CustomImportPresetConfigurationMenu::new));
  public static final RegistryObject<MenuType<DefaultImportPresetConfigurationMenu>>
      DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "default_import_preset_configuration_menu",
              () -> IForgeMenuType.create(DefaultImportPresetConfigurationMenu::new));
  public static final RegistryObject<MenuType<WorldImportPresetConfigurationMenu>>
      WORLD_IMPORT_PRESET_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "world_import_preset_configuration_menu",
              () -> IForgeMenuType.create(WorldImportPresetConfigurationMenu::new));

  // Rotation Configuration
  public static final RegistryObject<MenuType<DefaultRotationConfigurationMenu>>
      DEFAULT_ROTATION_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "default_rotation_configuration_menu",
              () -> IForgeMenuType.create(DefaultRotationConfigurationMenu::new));

  // Skin Configuration
  public static final RegistryObject<MenuType<CustomSkinConfigurationMenu>>
      CUSTOM_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "custom_skin_configuration_menu",
              () -> IForgeMenuType.create(CustomSkinConfigurationMenu::new));
  public static final RegistryObject<MenuType<DefaultSkinConfigurationMenu>>
      DEFAULT_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "default_skin_configuration_menu",
              () -> IForgeMenuType.create(DefaultSkinConfigurationMenu::new));
  public static final RegistryObject<MenuType<PlayerSkinConfigurationMenu>>
      PLAYER_SKIN_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "player_skin_configuration_menu",
              () -> IForgeMenuType.create(PlayerSkinConfigurationMenu::new));

  // Scaling Configuration
  public static final RegistryObject<MenuType<ScalingConfigurationMenu>>
      SCALING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "scaling_configuration_menu",
              () -> IForgeMenuType.create(ScalingConfigurationMenu::new));

  // Trading Configuration
  public static final RegistryObject<MenuType<NoneTradingConfigurationMenu>>
      NONE_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "none_trading_configuration_menu",
              () -> IForgeMenuType.create(NoneTradingConfigurationMenu::new));
  public static final RegistryObject<MenuType<BasicTradingConfigurationMenu>>
      BASIC_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "basic_trading_configuration_menu",
              () -> IForgeMenuType.create(BasicTradingConfigurationMenu::new));
  public static final RegistryObject<MenuType<AdvancedTradingConfigurationMenu>>
      ADVANCED_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "advanced_trading_configuration_menu",
              () -> IForgeMenuType.create(AdvancedTradingConfigurationMenu::new));
  public static final RegistryObject<MenuType<CustomTradingConfigurationMenu>>
      CUSTOM_TRADING_CONFIGURATION_MENU =
          MENU_TYPES.register(
              "custom_trading_configuration_menu",
              () -> IForgeMenuType.create(CustomTradingConfigurationMenu::new));
}
