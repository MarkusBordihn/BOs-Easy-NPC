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

package de.markusbordihn.easynpc.configui.menu;

import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import java.util.EnumMap;
import java.util.Map;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;

public class MenuHandler implements MenuHandlerInterface {

  protected static final Map<ConfigurationType, MenuType<? extends ConfigurationMenu>>
      configurationMenuMap = new EnumMap<>(ConfigurationType.class);
  protected static final Map<EditorType, MenuType<? extends EditorMenu>> editorMenuMap =
      new EnumMap<>(EditorType.class);

  public MenuHandler() {
    // Register menu handler
  }

  public static void registerMenuHandler(final FMLCommonSetupEvent event) {

    configurationMenuMap.put(
        ConfigurationType.ABILITIES_ATTRIBUTE,
        ModMenuTypes.ABILITIES_ATTRIBUTE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.ADVANCED_DIALOG, ModMenuTypes.ADVANCED_DIALOG_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.ADVANCED_POSE, ModMenuTypes.ADVANCED_POSE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.ADVANCED_TRADING, ModMenuTypes.ADVANCED_TRADING_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.ATTACK_OBJECTIVE, ModMenuTypes.ATTACK_OBJECTIVE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.BASE_ATTRIBUTE, ModMenuTypes.BASE_ATTRIBUTE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.BASIC_ACTION, ModMenuTypes.BASIC_ACTION_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.BASIC_DIALOG, ModMenuTypes.BASIC_DIALOG_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.BASIC_OBJECTIVE, ModMenuTypes.BASIC_OBJECTIVE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.BASIC_POSE, ModMenuTypes.BASIC_POSE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.BASIC_TRADING, ModMenuTypes.BASIC_TRADING_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.COMBAT_ATTRIBUTE, ModMenuTypes.COMBAT_ATTRIBUTE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.CUSTOM_POSE, ModMenuTypes.CUSTOM_POSE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.CUSTOM_PRESET_EXPORT,
        ModMenuTypes.CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.CUSTOM_PRESET_IMPORT,
        ModMenuTypes.CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.CUSTOM_MODEL, ModMenuTypes.CUSTOM_MODEL_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.CUSTOM_SKIN, ModMenuTypes.CUSTOM_SKIN_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.CUSTOM_TRADING, ModMenuTypes.CUSTOM_TRADING_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DEFAULT_MODEL, ModMenuTypes.DEFAULT_MODEL_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DEFAULT_POSE, ModMenuTypes.DEFAULT_POSE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DEFAULT_POSITION, ModMenuTypes.DEFAULT_POSITION_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DEFAULT_PRESET_IMPORT,
        ModMenuTypes.DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DEFAULT_ROTATION, ModMenuTypes.DEFAULT_ROTATION_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DEFAULT_SKIN, ModMenuTypes.DEFAULT_SKIN_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DIALOG_ACTION, ModMenuTypes.DIALOG_ACTION_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DISPLAY_ATTRIBUTE,
        ModMenuTypes.DISPLAY_ATTRIBUTE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.DISTANCE_ACTION, ModMenuTypes.DISTANCE_ACTION_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.EQUIPMENT, ModMenuTypes.EQUIPMENT_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.FOLLOW_OBJECTIVE, ModMenuTypes.FOLLOW_OBJECTIVE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.LOCAL_PRESET_IMPORT,
        ModMenuTypes.LOCAL_IMPORT_PRESET_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.LOOK_OBJECTIVE, ModMenuTypes.LOOK_OBJECTIVE_CONFIGURATION_MENU.get());
    configurationMenuMap.put(ConfigurationType.MAIN, ModMenuTypes.MAIN_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.NONE_DIALOG, ModMenuTypes.NONE_DIALOG_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.NONE_SKIN, ModMenuTypes.NONE_SKIN_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.NONE_TRADING, ModMenuTypes.NONE_TRADING_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.PLAYER_SKIN, ModMenuTypes.PLAYER_SKIN_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.SCALING, ModMenuTypes.SCALING_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.URL_SKIN, ModMenuTypes.URL_SKIN_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.WORLD_PRESET_EXPORT,
        ModMenuTypes.WORLD_EXPORT_PRESET_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.WORLD_PRESET_IMPORT,
        ModMenuTypes.WORLD_IMPORT_PRESET_CONFIGURATION_MENU.get());
    configurationMenuMap.put(
        ConfigurationType.YES_NO_DIALOG, ModMenuTypes.YES_NO_DIALOG_CONFIGURATION_MENU.get());

    editorMenuMap.put(EditorType.ACTION_DATA, ModMenuTypes.ACTION_DATA_EDITOR_MENU.get());
    editorMenuMap.put(
        EditorType.ACTION_DATA_ENTRY, ModMenuTypes.ACTION_DATA_ENTRY_EDITOR_MENU.get());
    editorMenuMap.put(EditorType.CONDITION_DATA, ModMenuTypes.CONDITION_DATA_EDITOR_MENU.get());
    editorMenuMap.put(
        EditorType.CONDITION_DATA_ENTRY, ModMenuTypes.CONDITION_DATA_ENTRY_EDITOR_MENU.get());
    editorMenuMap.put(EditorType.DIALOG, ModMenuTypes.DIALOG_EDITOR_MENU.get());
    editorMenuMap.put(EditorType.DIALOG_BUTTON, ModMenuTypes.DIALOG_BUTTON_EDITOR_MENU.get());
    editorMenuMap.put(EditorType.DIALOG_TEXT, ModMenuTypes.DIALOG_TEXT_EDITOR_MENU.get());
  }

  @Override
  public MenuType<? extends ConfigurationMenu> getMenuTypeByConfigurationType(
      ConfigurationType configurationType) {
    return configurationMenuMap.get(configurationType);
  }

  @Override
  public MenuType<? extends EditorMenu> getMenuTypeByEditorType(EditorType editorType) {
    return editorMenuMap.get(editorType);
  }
}
