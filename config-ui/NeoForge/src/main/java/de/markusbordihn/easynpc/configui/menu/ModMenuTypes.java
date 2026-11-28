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

import de.markusbordihn.easynpc.configui.Constants;
import de.markusbordihn.easynpc.configui.data.custom.CustomMenuType;
import de.markusbordihn.easynpc.configui.data.editor.EditorType;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.equipment.EquipmentConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.inventory.InventoryConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.trading.AdvancedTradingConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.configuration.trading.BasicTradingConfigurationMenu;
import de.markusbordihn.easynpc.configui.menu.editor.EditorMenu;
import de.markusbordihn.easynpc.configui.menu.preset.PresetBrowserMenu;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import java.util.EnumMap;
import java.util.Map;
import java.util.function.Supplier;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.MenuType;
import net.neoforged.neoforge.common.extensions.IMenuTypeExtension;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;

public class ModMenuTypes {

  public static final DeferredRegister<MenuType<?>> MENU_TYPES =
      DeferredRegister.create(BuiltInRegistries.MENU, Constants.MOD_ID);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      ABILITIES_ATTRIBUTE_CONFIGURATION_MENU =
          register(ConfigurationType.ABILITIES_ATTRIBUTE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      ADVANCED_DIALOG_CONFIGURATION_MENU =
          register(ConfigurationType.ADVANCED_DIALOG, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      ADVANCED_POSE_CONFIGURATION_MENU =
          register(ConfigurationType.ADVANCED_POSE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<AdvancedTradingConfigurationMenu>>
      ADVANCED_TRADING_CONFIGURATION_MENU =
          register(ConfigurationType.ADVANCED_TRADING, AdvancedTradingConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      ATTACK_OBJECTIVE_CONFIGURATION_MENU =
          register(ConfigurationType.ATTACK_OBJECTIVE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      FLEE_OBJECTIVE_CONFIGURATION_MENU =
          register(ConfigurationType.FLEE_OBJECTIVE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      BASE_ATTRIBUTE_CONFIGURATION_MENU =
          register(ConfigurationType.BASE_ATTRIBUTE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      BASIC_ACTION_CONFIGURATION_MENU =
          register(ConfigurationType.BASIC_ACTION, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      BASIC_DIALOG_CONFIGURATION_MENU =
          register(ConfigurationType.BASIC_DIALOG, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      BASIC_OBJECTIVE_CONFIGURATION_MENU =
          register(ConfigurationType.BASIC_OBJECTIVE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      BASIC_POSE_CONFIGURATION_MENU =
          register(ConfigurationType.BASIC_POSE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      BASIC_SOUND_CONFIGURATION_MENU =
          register(ConfigurationType.BASIC_SOUND, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<BasicTradingConfigurationMenu>>
      BASIC_TRADING_CONFIGURATION_MENU =
          register(ConfigurationType.BASIC_TRADING, BasicTradingConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      COMBAT_ATTRIBUTE_CONFIGURATION_MENU =
          register(ConfigurationType.COMBAT_ATTRIBUTE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      PROJECTILE_ATTRIBUTE_CONFIGURATION_MENU =
          register(ConfigurationType.PROJECTILE_ATTRIBUTE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      BOSS_BAR_ATTRIBUTE_CONFIGURATION_MENU =
          register(ConfigurationType.BOSS_BAR_ATTRIBUTE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      COMBAT_SOUND_CONFIGURATION_MENU =
          register(ConfigurationType.COMBAT_SOUND, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      CUSTOM_POSE_CONFIGURATION_MENU =
          register(ConfigurationType.CUSTOM_POSE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      CUSTOM_EXPORT_PRESET_CONFIGURATION_MENU =
          register(ConfigurationType.CUSTOM_PRESET_EXPORT, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      LOCAL_EXPORT_PRESET_CONFIGURATION_MENU =
          register(ConfigurationType.LOCAL_PRESET_EXPORT, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      CUSTOM_IMPORT_PRESET_CONFIGURATION_MENU =
          register(ConfigurationType.CUSTOM_PRESET_IMPORT, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      CUSTOM_SKIN_CONFIGURATION_MENU =
          register(ConfigurationType.CUSTOM_SKIN, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      CUSTOM_TRADING_CONFIGURATION_MENU =
          register(ConfigurationType.CUSTOM_TRADING, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      COBBLEMON_MODEL_CONFIGURATION_MENU =
          register(ConfigurationType.COBBLEMON_MODEL, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      CUSTOM_MODEL_CONFIGURATION_MENU =
          register(ConfigurationType.CUSTOM_MODEL, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      EASY_MODEL_ENTITIES_MODEL_CONFIGURATION_MENU =
          register(ConfigurationType.EASY_MODEL_ENTITIES_MODEL, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DEFAULT_MODEL_CONFIGURATION_MENU =
          register(ConfigurationType.DEFAULT_MODEL, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DEFAULT_POSE_CONFIGURATION_MENU =
          register(ConfigurationType.DEFAULT_POSE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DEFAULT_POSITION_CONFIGURATION_MENU =
          register(ConfigurationType.DEFAULT_POSITION, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DEFAULT_IMPORT_PRESET_CONFIGURATION_MENU =
          register(ConfigurationType.DEFAULT_PRESET_IMPORT, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DEFAULT_ROTATION_CONFIGURATION_MENU =
          register(ConfigurationType.DEFAULT_ROTATION, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DEFAULT_SKIN_CONFIGURATION_MENU =
          register(ConfigurationType.DEFAULT_SKIN, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DIALOG_ACTION_CONFIGURATION_MENU =
          register(ConfigurationType.DIALOG_ACTION, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DISPLAY_ATTRIBUTE_CONFIGURATION_MENU =
          register(ConfigurationType.DISPLAY_ATTRIBUTE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      DISTANCE_ACTION_CONFIGURATION_MENU =
          register(ConfigurationType.DISTANCE_ACTION, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      INTERACTION_SOUND_CONFIGURATION_MENU =
          register(ConfigurationType.INTERACTION_SOUND, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      TRADE_SOUND_CONFIGURATION_MENU =
          register(ConfigurationType.TRADE_SOUND, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      INTERVAL_ACTION_CONFIGURATION_MENU =
          register(ConfigurationType.INTERVAL_ACTION, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EquipmentConfigurationMenu>>
      EQUIPMENT_CONFIGURATION_MENU =
          register(ConfigurationType.EQUIPMENT, EquipmentConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<InventoryConfigurationMenu>>
      INVENTORY_CONFIGURATION_MENU =
          register(ConfigurationType.INVENTORY, InventoryConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      FOLLOW_OBJECTIVE_CONFIGURATION_MENU =
          register(ConfigurationType.FOLLOW_OBJECTIVE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      LOCAL_IMPORT_PRESET_CONFIGURATION_MENU =
          register(ConfigurationType.LOCAL_PRESET_IMPORT, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      LOOK_OBJECTIVE_CONFIGURATION_MENU =
          register(ConfigurationType.LOOK_OBJECTIVE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      MAIN_CONFIGURATION_MENU = register(ConfigurationType.MAIN, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      NONE_DIALOG_CONFIGURATION_MENU =
          register(ConfigurationType.NONE_DIALOG, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      ADVANCED_SKIN_CONFIGURATION_MENU =
          register(ConfigurationType.ADVANCED_SKIN, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      NONE_TRADING_CONFIGURATION_MENU =
          register(ConfigurationType.NONE_TRADING, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      PLAYER_SKIN_CONFIGURATION_MENU =
          register(ConfigurationType.PLAYER_SKIN, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      SCALING_CONFIGURATION_MENU = register(ConfigurationType.SCALING, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      URL_SKIN_CONFIGURATION_MENU = register(ConfigurationType.URL_SKIN, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      WORLD_EXPORT_PRESET_CONFIGURATION_MENU =
          register(ConfigurationType.WORLD_PRESET_EXPORT, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      WORLD_IMPORT_PRESET_CONFIGURATION_MENU =
          register(ConfigurationType.WORLD_PRESET_IMPORT, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      YES_NO_DIALOG_CONFIGURATION_MENU =
          register(ConfigurationType.YES_NO_DIALOG, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      MISC_ATTRIBUTE_CONFIGURATION_MENU =
          register(ConfigurationType.MISC_ATTRIBUTE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<ConfigurationMenu>>
      TARGET_OBJECTIVE_CONFIGURATION_MENU =
          register(ConfigurationType.TARGET_OBJECTIVE, ConfigurationMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> ACTION_DATA_EDITOR_MENU =
      register(EditorType.ACTION_DATA, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>>
      ACTION_DATA_ENTRY_EDITOR_MENU = register(EditorType.ACTION_DATA_ENTRY, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> CONDITION_DATA_EDITOR_MENU =
      register(EditorType.CONDITION_DATA, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>>
      CONDITION_DATA_ENTRY_EDITOR_MENU = register(EditorType.CONDITION_DATA_ENTRY, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> DIALOG_EDITOR_MENU =
      register(EditorType.DIALOG, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> DIALOG_BUTTON_EDITOR_MENU =
      register(EditorType.DIALOG_BUTTON, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> DIALOG_OPTIONS_EDITOR_MENU =
      register(EditorType.DIALOG_OPTIONS, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> DIALOG_TEXT_EDITOR_MENU =
      register(EditorType.DIALOG_TEXT, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> FACTION_EDITOR_MENU =
      register(EditorType.FACTION, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<EditorMenu>> FACTIONS_EDITOR_MENU =
      register(EditorType.FACTIONS, EditorMenu::new);
  public static final DeferredHolder<MenuType<?>, MenuType<PresetBrowserMenu>> PRESET_BROWSER_MENU =
      register(CustomMenuType.PRESET_BROWSER, PresetBrowserMenu::new);
  static final Map<ConfigurationType, Supplier<? extends MenuType<? extends ConfigurationMenu>>>
      CONFIGURATION_MENU_TYPES = new EnumMap<>(ConfigurationType.class);
  static final Map<EditorType, Supplier<? extends MenuType<? extends EditorMenu>>>
      EDITOR_MENU_TYPES = new EnumMap<>(EditorType.class);
  static final Map<CustomMenuType, Supplier<? extends MenuType<? extends ConfigUIMenu>>>
      CUSTOM_MENU_TYPES = new EnumMap<>(CustomMenuType.class);

  private ModMenuTypes() {}

  private static <T extends ConfigurationMenu> DeferredHolder<MenuType<?>, MenuType<T>> register(
      ConfigurationType configurationType, MenuFactory<T> menuFactory) {
    DeferredHolder<MenuType<?>, MenuType<T>> menuType =
        register(configurationType.getName(), menuFactory);
    CONFIGURATION_MENU_TYPES.put(configurationType, menuType);
    return menuType;
  }

  private static <T extends EditorMenu> DeferredHolder<MenuType<?>, MenuType<T>> register(
      EditorType editorType, MenuFactory<T> menuFactory) {
    DeferredHolder<MenuType<?>, MenuType<T>> menuType = register(editorType.getName(), menuFactory);
    EDITOR_MENU_TYPES.put(editorType, menuType);
    return menuType;
  }

  private static <T extends ConfigUIMenu> DeferredHolder<MenuType<?>, MenuType<T>> register(
      CustomMenuType customMenuType, MenuFactory<T> menuFactory) {
    DeferredHolder<MenuType<?>, MenuType<T>> menuType =
        register(customMenuType.getName(), menuFactory);
    CUSTOM_MENU_TYPES.put(customMenuType, menuType);
    return menuType;
  }

  private static <T extends ConfigUIMenu> DeferredHolder<MenuType<?>, MenuType<T>> register(
      String name, MenuFactory<T> menuFactory) {
    return MENU_TYPES.register(
        name,
        id ->
            IMenuTypeExtension.create((containerId, inventory, buffer) ->
                menuFactory.create(BuiltInRegistries.MENU.getValue(id), containerId, inventory)));
  }

  private interface MenuFactory<T extends ConfigUIMenu> {
    T create(MenuType<?> menuType, int containerId, Inventory inventory);
  }
}
